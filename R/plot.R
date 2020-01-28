#' Plot time series from a tsibble
#' 
#' Produces a time series plot of one or more variables from a tsibble. If the
#' tsibble contains a multiple keys, separate time series will be identified by
#' colour.
#' 
#' @param object A tsibble.
#' @param .vars A bare expression containing data you wish to plot. Multiple variables can be plotted using [`ggplot2::vars()`].
#' @param ... Further arguments passed to [`ggplot2::geom_line()`], which can be used to specify fixed aesthetics such as `colour = "red"` or `size = 3`. 
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibbledata)
#' library(tsibble)
#' 
#' tsibbledata::gafa_stock %>%
#'  autoplot(vars(Close, log(Close)))
#' }
#' 
#' @importFrom ggplot2 ggplot aes geom_line guides guide_legend xlab
#' @export
autoplot.tbl_ts <- function(object, .vars = NULL, ...){
  quo_vars <- enquo(.vars)
  
  kv <- key_vars(object)
  nk <- n_keys(object)
  
  if(quo_is_null(quo_vars)){
    if(is_empty(measured_vars(object))){
      abort("There are no variables to plot.")
    }
    inform(sprintf(
      "Plot variable not specified, automatically selected `.vars = %s`",
      measured_vars(object)[1]
    ))
    y <- sym(measured_vars(object)[1])
    .vars <- as_quosures(list(y), env = empty_env())
  }
  else if(possibly(compose(is_quosures, eval_tidy), FALSE)(.vars)){
    .vars <- eval_tidy(.vars)
    object <- gather(
      mutate(object, !!!.vars),
      ".response", "value", !!!map(.vars, quo_name), factor_key = TRUE
    )
    y <- sym("value")
  }
  else{
    y <- quo_vars
    .vars <- list(y)
  }
  
  aes_spec <- list(x = index(object), y = y)
  
  if(nk > 1){
    object <- dplyr::mutate_if(object, ~inherits(., "agg_key"), compose(trimws, format))
    aes_spec["colour"] <- list(expr(interaction(!!!syms(kv), sep = "/")))
  }
  
  p <- ggplot(object, eval_tidy(expr(aes(!!!aes_spec)))) + 
    geom_line(...) +
    xlab(paste0(index_var(object), " [", format(interval(object)), "]"))
  
  if(nk > 1){
    p <- p + 
      guides(colour = guide_legend(paste0(kv, collapse = "/")))
  }
  
  if(length(.vars) > 1){
    p <- p + facet_wrap(vars(!!sym(".response")), scales = "free_y", 
                        ncol = length(.vars)) + ggplot2::ylab(NULL)
  }
  
  p
}

#' @rdname autoplot.tbl_ts
#' @importFrom ggplot2 ggplot aes geom_line guides guide_legend xlab
#' @export
autolayer.tbl_ts <- function(object, .vars = NULL, ...){
  quo_vars <- enquo(.vars)
  kv <- key_vars(object)
  nk <- n_keys(object)
  
  if(quo_is_null(quo_vars)){
    if(is_empty(measured_vars(object))){
      abort("There are no variables to plot.")
    }
    inform(sprintf(
      "Plot variable not specified, automatically selected `.vars = %s`",
      measured_vars(object)[1]
    ))
    y <- sym(measured_vars(object)[1])
    .vars <- as_quosures(list(y), env = empty_env())
  }
  else if(possibly(compose(is_quosures, eval_tidy), FALSE)(.vars)){
    .vars <- eval_tidy(.vars)
    object <- gather(
      mutate(object, !!!.vars),
      ".response", "value", !!!map(.vars, quo_name), factor_key = TRUE
    )
    y <- sym("value")
  }
  else{
    y <- quo_vars
    .vars <- list(y)
  }
  
  aes_spec <- list(x = index(object), y = y)
  
  if(nk > 1){
    aes_spec["colour"] <- list(expr(interaction(!!!syms(kv), sep = "/")))
  }
  if(n_keys(object) > 1){
    aes_spec["group"] <- list(expr(interaction(!!!syms(key_vars(object)), sep = "/")))
  }
  
  geom_line(eval_tidy(expr(aes(!!!aes_spec))), data = object, ...)
}

#' @importFrom ggplot2 fortify
#' @export
fortify.fbl_ts <- function(object, level = c(80, 95)){
  resp <- object%@%"response"
  dist <- object%@%"dist"
  idx <- index(object)
  kv <- key_vars(object)
  
  if(length(resp) > 1){
    object <- object %>%
      mutate(
        .response = rep(list(factor(map_chr(resp, expr_text))), NROW(object)),
        value = transpose_dbl(list2(!!!resp))
      )
  }
  
  if(!is.null(level)){
    object <- object %>% 
      mutate(
        !!!set_names(
          map(level, function(.x) expr(hilo(!!dist, !!.x))), 
          level
        )
      )
    
    object <- gather(object, ".rm", ".hilo", !!!syms(as.character(level)))
    
    if(length(resp) > 1){
      object <- unnest_tbl(object, c(".response", "value", ".hilo"))
      resp <- syms("value")
      kv <- c(kv, ".response")
    }
    else{
      object <- unnest_tbl(object, ".hilo")
    }
    kv <- c(kv, ".level")
    
    # Drop temporary col
    object[".rm"] <- NULL
  }
  else if (length(resp) > 1) {
    object <- unnest_tbl(object, c(".response", "value"))
    kv <- c(kv, ".response")
  }
  
  as_tsibble(as_tibble(object)[setdiff(colnames(object), expr_text(dist))],
             key = kv, index = !!idx, validate = FALSE) 
}

#' Plot a set of forecasts
#' 
#' Produces a forecast plot from a fable. As the original data is not included
#' in the fable object, it will need to be specified via the `data` argument.
#' The `data` argument can be used to specify a shorter period of data, which is
#' useful to focus on the more recent observations.
#' 
#' @param object A fable.
#' @param data A tsibble with the same key structure as the fable.
#' @param show_gap Setting this to `FALSE` will connect the historical observations with the forecasts.
#' @param ... Further arguments passed used to specify fixed aesthetics for the forecasts such as `colour = "red"` or `size = 3`.
#' @inheritParams hilo
#' 
#' @examples 
#' library(tsibbledata)
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' 
#' fc <- aus_production %>%
#'   model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A"))) %>% 
#'   forecast(h = "3 years") 
#' 
#' fc %>% 
#'   autoplot(aus_production)
#' }
#' 
#' @importFrom ggplot2 facet_wrap
#' @export
autoplot.fbl_ts <- function(object, data = NULL, level = c(80, 95), show_gap = TRUE, ...){
  fc_resp <- object%@%"response"
  fc_key <- setdiff(key_vars(object), ".model")
  common_models <- duplicated(key_data(object)[[".model"]] %||% rep(TRUE, NROW(key_data)))
  
  aes_y <- if(length(fc_resp) > 1){
    sym("value")
  }
  else{
    fc_resp[[1]]
  }

  if (!is.null(data)){
    if(!identical(fc_key, key_vars(data))){
      abort("Provided data contains a different key structure to the forecasts.")
    }
    
    if(!is_empty(key(data))){
      data <- semi_join(data, object, by = key_vars(data))
    }
    
    if(length(fc_resp) > 1){
      data <- gather(data, ".response", "value", !!!fc_resp, factor_key = TRUE)
    }
    
    data <- data %>% 
      dplyr::mutate_if(~inherits(., "agg_key"), compose(trimws, format))
  }

  # Change colours to be more appropriate for later facets
  fc_layer <- autolayer(object, data = data, level = level, 
                        show_gap = show_gap, ...)
  if(sum(!common_models) > 1){
    fc_layer$mapping$colour <- set_expr(fc_layer$mapping$colour, sym(".model"))
  }
  else{
    fc_layer$mapping$colour <- NULL
  }
  
  p <- ggplot(data, aes(x = !!index(object), y = !!aes_y)) + 
    fc_layer
    
  if(!is.null(data)){
    p <- p + geom_line()
  }
  
  if(length(fc_resp) > 1){
    p <- p + 
      facet_wrap(vars(!!!syms(c(".response", fc_key))),
                 ncol = length(fc_resp), scales = "free_y") +
      ggplot2::ylab(NULL)
  } else if(any(common_models)){
    p <- p + facet_wrap(vars(!!!syms(fc_key)),
                        ncol = 1, scales = "free_y")
  }
  
  p
}

#' @rdname autoplot.fbl_ts
#' @examples 
#' 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' aus_production %>% 
#'   autoplot(Beer) + 
#'   autolayer(fc)
#' }
#' 
#' @export
autolayer.fbl_ts <- function(object, data = NULL, level = c(80, 95), 
                             show_gap = TRUE, ...){
  fc_key <- setdiff(key_vars(object), ".model")
  key_data <- key_data(object)
  resp_var <- map_chr(object%@%"response", as_string)
  idx <- index(object)
  common_models <- duplicated(key_data[[".model"]] %||% rep(TRUE, NROW(key_data)))
  
  if(isFALSE(level)){
    warn("Plot argument `level` should be a numeric vector of levels to display. Setting `level = NULL` will remove the intervals from the plot.")
    level <- NULL
  }
  
  if(!show_gap && is.null(data)){
    warn("Could not connect forecasts to last observation as `data` was not provided. Setting `show_gap = FALSE`.")
  }
  if(!show_gap){
    gap <- key_data(object)
    gap[ncol(gap)] <- NULL
    last_obs <- filter(group_by_key(data), !!idx == max(!!idx))
    if (length(key_vars(last_obs)) == 0) {
      gap[names(last_obs)] <- last_obs
    }
    else {
      gap <- left_join(gap, last_obs, by = key_vars(last_obs))
    }
    if (length(resp_var) > 1) abort("`show_gap = FALSE` is not yet supported for multivariate forecasts.")
    gap[[as_string(object%@%"dist")]] <- dist_normal(gap[[resp_var]], 0)
    gap <- as_fable(gap, index = !!idx, key = key_vars(object),
                    response = object%@%"response",
                    distribution = !!(object%@%"dist"))
    object <- rbind(gap, object)
  }
  
  fc_data <- fortify(object, level = level) %>% 
    dplyr::mutate_if(~inherits(., "agg_key"), compose(trimws, format))
  
  if(length(object%@%"response") > 1){
    resp <- sym("value")
    grp <- syms(".response")
  }
  else{
    resp <- sym(resp_var)
    grp <- NULL
  }
  
  mapping <- aes(
    x = !!idx,
    y = !!resp
  )
  
  if(!is.null(level)){
    mapping$level <- sym(".level")
    mapping$ymin <- sym(".lower")
    mapping$ymax <- sym(".upper")
  }
  
  if(!is_empty(fc_key)){
    grp <- c(grp, syms(fc_key))
  }
  if(NROW(key_data) > 1){
    useful_keys <- fc_key[map_lgl(key_data[fc_key], function(x) sum(!duplicated(x)) > 1)]
    col <- c(
      if(any(common_models)) syms(useful_keys) else NULL,
      if(sum(!common_models) > 1) syms(".model") else NULL
    )
    
    mapping$colour <- if(length(col)==1) col[[1]] else expr(interaction(!!!col, sep = "/"))
    grp <- c(grp, syms(".model"))
  }
  if(length(grp) > 0){
    mapping$group <- expr(interaction(!!!map(grp, function(x) expr(format(!!x))), sep = "/"))
  }
  
  geom_forecast(mapping = mapping, stat = "identity", data = fc_data, ...)
}

#' Decomposition plots
#' 
#' Produces a faceted plot of the components used to build the response 
#' variable of the dable. Useful for visualising how the components contribute
#' in a decomposition or model.
#' 
#' @param object A dable.
#' @param .vars The column of the dable used to plot. By default, this will be the response variable of the decomposition.
#' @param scale_bars If `TRUE`, each facet will include a scale bar which represents the same units across each facet.
#' @inheritParams autoplot.tbl_ts
#' 
#' @examples 
#' if (requireNamespace("feasts", quietly = TRUE)) {
#' library(feasts)
#' library(tsibbledata)
#' aus_production %>% 
#'   model(STL(Beer)) %>%
#'   components() %>%  
#'   autoplot()
#' }
#' 
#' @importFrom ggplot2 ggplot geom_line geom_rect facet_grid vars ylab labs
#' @export
autoplot.dcmp_ts <- function(object, .vars = NULL, scale_bars = TRUE, ...){
  method <- object%@%"method"
  idx <- index(object)
  keys <- key(object)
  n_keys <- n_keys(object)
  
  .vars <- enquo(.vars)
  if(quo_is_null(.vars)){
    .vars <- object%@%"response"
  }
  dcmp_str <- dcmp <- (object%@%"aliases")[[as_string(get_expr(.vars))]]
  if(!is.null(dcmp_str)){
    dcmp_str <- expr_text(dcmp_str)
  }
  object <- object %>% 
    transmute(!!.vars, !!!syms(all.vars(dcmp))) %>% 
    gather(".var", ".val", !!!syms(measured_vars(.)), factor_key = TRUE)
  
  line_aes <- aes(x = !!idx, y = !!sym(".val"))
  if(n_keys > 1){
    line_aes$colour <- expr(interaction(!!!keys, sep = "/"))
  }
  
  p <- object %>% 
    ggplot() + 
    geom_line(line_aes, ...) + 
    facet_grid(vars(!!sym(".var")), scales = "free_y") + 
    ylab(NULL) + 
    labs(
      title = paste(method%||%"A", "decomposition"), 
      subtitle = paste(c(expr_text(get_expr(.vars)), dcmp_str), collapse = " = ")
    )
  
  # Rangebars
  if (scale_bars) {
    xranges <- range(object[[expr_text(idx)]])
    barwidth <- pmax(1, round((1 / 64) * diff(units_since(xranges))))
    
    # Avoid issues with visible bindings
    ymin <- ymax <- center <- diff <- NULL
    
    range_data <- object %>%
      as_tibble %>% 
      group_by(!!sym(".var")) %>% 
      summarise(ymin = min(!!sym(".val"), na.rm = TRUE), ymax = max(!!sym(".val"), na.rm = TRUE)) %>% 
      mutate(
        center = (ymin + ymax) / 2,
        diff = min(ymax - ymin),
        xmin = xranges[1] - barwidth * 2, xmax = xranges[1] - barwidth,
        ymin = center - diff/2, ymax = center + diff/2
      )
    
    p <- p + geom_rect(data = range_data,
      aes(ymin = !!sym("ymin"), ymax = !!sym("ymax"),
          xmin = !!sym("xmin"), xmax = !!sym("xmax")),
      fill = "gray75", colour = "black", size = 1 / 3
    )
  }
  
  if(!is_empty(keys)){
    p <- p + guides(colour = guide_legend(paste0(map_chr(keys, expr_text), collapse = "/")))
  }
  
  p
}
