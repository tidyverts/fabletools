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
    mv <- measured_vars(object)
    pos <- which(vapply(object[mv], is.numeric, logical(1L)))
    if(is_empty(pos)) {
      abort("Could not automatically identify an appropriate plot variable, please specify the variable to plot.")
    }
    inform(sprintf(
      "Plot variable not specified, automatically selected `.vars = %s`",
      mv[pos[1]]
    ))
    y <- sym(mv[pos[1]])
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
    object <- dplyr::mutate_if(object, ~inherits(., "agg_vec"), compose(trimws, format))
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
    mv <- measured_vars(object)
    pos <- which(vapply(object[mv], is.numeric, logical(1L)))
    if(is_empty(pos)) {
      abort("Could not automatically identify an appropriate plot variable, please specify the variable to plot.")
    }
    inform(sprintf(
      "Plot variable not specified, automatically selected `.vars = %s`",
      mv[pos[1]]
    ))
    y <- sym(mv[pos[1]])
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
  
  geom_line(eval_tidy(expr(aes(!!!aes_spec))), data = object, ..., inherit.aes = FALSE)
}

#' @importFrom ggplot2 fortify
#' @export
fortify.fbl_ts <- function(object, level = c(80, 95)){
  if(deparse(match.call()) != "fortify.fbl_ts(object = data)"){
    warn("The output of `fortify(<fable>)` has changed to better suit usage with the ggdist package.
If you're using it to extract intervals, consider using `hilo()` to compute intervals, and `unpack_hilo()` to obtain values.")
  }
  return(as_tibble(object))
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
#' @param level The confidence level(s) for the plotted intervals.
#' @param show_gap Setting this to `FALSE` will connect the most recent value in `data` with the forecasts.
#' @param ... Further arguments passed used to specify fixed aesthetics for the forecasts such as `colour = "red"` or `size = 3`.
#' @param point_forecast The point forecast measure to be displayed in the plot.
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
  fc_resp <- response_vars(object)
  fc_key <- setdiff(key_vars(object), ".model")
  common_models <- duplicated(key_data(object)[[".model"]] %||% rep(TRUE, NROW(key_data(object))))
  
  aes_y <- if(length(fc_resp) > 1){
    sym("value")
  }
  else{
    sym(fc_resp)
  }

  # Structure input data to match fable
  if (!is.null(data)){
    data <- as_tsibble(data)
    if(!identical(fc_key, key_vars(data))){
      abort("Provided data contains a different key structure to the forecasts.")
    }
    
    if(!is_empty(key(data))){
      data <- semi_join(data, object, by = key_vars(data))
    }
    
    if(length(fc_resp) > 1){
      data <- gather(data, ".response", "value", !!!syms(fc_resp), factor_key = TRUE)
    }
    
    data <- data %>% 
      dplyr::mutate_if(~inherits(., "agg_vec"), compose(trimws, format))
  }

  # Compute facets, if any
  facet_vars <- if(length(fc_resp) > 1) ".response" else NULL
  if(any(common_models)) facet_vars <- c(facet_vars, fc_key)
  
  # Change colours to be more appropriate for later facets
  fc_layer <- build_fbl_layer(object, data = data, level = level, 
                              show_gap = show_gap, facet_vars = facet_vars, ...)
  
  # Add forecasts on base plot
  p <- ggplot(data, aes(x = !!index(object))) + 
    fc_layer
    
  # Add historical data
  if(!is.null(data)){
    hist_aes <- aes(y = !!aes_y)
    if(length(key_vars(data)) > 0) {
      hist_aes[["group"]] <- expr(interaction(!!!syms(key_vars(data))))
    }
    p <- p + geom_line(hist_aes)
  }
  
  # Add facets
  if(!is_empty(facet_vars)){
    p <- p + facet_wrap(vars(!!!syms(facet_vars)), ncol = length(fc_resp),
                        scales = "free_y")
  }
  if(length(fc_resp) > 1) p <- p + ggplot2::ylab(NULL)
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
#' @importFrom distributional scale_level_continuous guide_level
#' @export
autolayer.fbl_ts <- function(object, data = NULL, level = c(80, 95), 
                             point_forecast = list(mean = mean), show_gap = TRUE, ...){
  build_fbl_layer(object = object, data = data, level = level, 
                  point_forecast = point_forecast, show_gap = show_gap, ...)

}

build_fbl_layer <- function(object, data = NULL, level = c(80, 95), 
                            colour = NULL, color = NULL, fill = NULL,
                            point_forecast = list(mean = mean), show_gap = TRUE, 
                            ..., facet_vars = NULL){
  mdl_key <- object%@%"model_cn"
  fc_key <- setdiff(key_vars(object), mdl_key)
  key_data <- key_data(object)
  key_vars <- key_vars(object)
  resp_var <- response_vars(object)
  dist_var <- distribution_var(object)
  idx <- index(object)
  common_models <- duplicated(key_data[[mdl_key]] %||% rep(TRUE, NROW(key_data(object))))
  colour <- colour %||% color %||% fill %||% "blue"
  
  if(isFALSE(level)){
    warn("Plot argument `level` should be a numeric vector of levels to display. Setting `level = NULL` will remove the intervals from the plot.")
    level <- NULL
  }
  
  if(!show_gap && is.null(data)){
    warn("Could not connect forecasts to last observation as `data` was not provided. Setting `show_gap = FALSE`.")
  }
  if(!show_gap){
    gap <- key_data
    gap[ncol(gap)] <- NULL
    last_obs <- filter(group_by_key(data), !!idx == max(!!idx))
    if (length(key_vars(last_obs)) == 0) {
      gap[names(last_obs)] <- last_obs
    }
    else {
      gap <- left_join(gap, last_obs, by = key_vars(last_obs))
    }
    if (length(resp_var) > 1) abort("`show_gap = FALSE` is not yet supported for multivariate forecasts.")
    gap[[distribution_var(object)]] <- distributional::dist_degenerate(gap[[resp_var]])
    dimnames(gap[[distribution_var(object)]]) <- resp_var
    gap <- as_fable(gap, index = !!idx, key = key_vars(object),
                    response = resp_var,
                    distribution = distribution_var(object))
    object <- bind_rows(gap, object)
  }
  
  if(length(resp_var) > 1){
    resp <- sym("value")
    grp <- syms(".response")
  }
  else{
    resp <- sym(resp_var)
    grp <- NULL
  }
  
  mapping <- aes(
    x = !!idx,
  )
  
  useful_keys <- fc_key[map_lgl(key_data[fc_key], function(x) sum(!duplicated(x)) > 1)]
  col <- c(
    if(any(common_models)) useful_keys else NULL,
    if(sum(!common_models) > 1) ".model" else NULL
  )
  col <- setdiff(col, facet_vars)
  if(!is_empty(col)){
    col_nm <- paste0(col, collapse = "/")
    col <- if(length(col)==1) sym(col) else expr(interaction(!!!syms(col), sep = "/"))
  } else {
    col <- NULL
  }
  
  grp <- c(grp, syms(key_vars(object)))
  if(length(grp) > 0){
    mapping$group <- expr(interaction(!!!map(grp, function(x) expr(format(!!x))), sep = "/"))
  }
  
  single_row <- filter(key_data(object), lengths(!!sym(".rows")) == 1)
  
  out <- list()
  object <- object %>% 
    dplyr::mutate_if(~inherits(., "agg_vec"), compose(trimws, format))
  if(!is.null(level)){
    interval_data <- as_tibble(hilo(object, level = level)) %>% 
      tidyr::pivot_longer(paste0(level, "%"), names_to = NULL, values_to = "hilo")
    if(length(resp_var) > 1){
      interval_data <- interval_data[setdiff(names(interval_data), resp_var)] %>% 
        tidyr::unpack("hilo", names_repair = "minimal") %>% 
        tidyr::pivot_longer(names(interval_data$hilo), names_to = ".response", values_to = "hilo")
    }
    intvl_mapping <- mapping
    intvl_mapping$hilo <- sym("hilo")
    
    if(!is.null(col)){
      intvl_mapping$fill <- col
      out[[1]] <- distributional::geom_hilo_ribbon(intvl_mapping, data = dplyr::anti_join(interval_data, single_row, by = key_vars), ..., inherit.aes = FALSE)
      intvl_mapping$colour <- col
      intvl_mapping$fill <- NULL
      out[[2]] <- distributional::geom_hilo_linerange(intvl_mapping, data = dplyr::semi_join(interval_data, single_row, by = key_vars), ..., inherit.aes = FALSE)
      out[[3]] <- ggplot2::labs(fill = col_nm)
    } else {
      out[[1]] <- distributional::geom_hilo_ribbon(intvl_mapping, data = dplyr::anti_join(interval_data, single_row, by = key_vars), fill = colour, ..., inherit.aes = FALSE)
      out[[2]] <- distributional::geom_hilo_linerange(intvl_mapping, data = dplyr::semi_join(interval_data, single_row, by = key_vars), colour = colour, ..., inherit.aes = FALSE)
    }
  }
  
  object <- as_tibble(object)
  object[names(point_forecast)] <- map(point_forecast, calc, object[[dist_var]])
  object <- tidyr::pivot_longer(object[-match(dist_var, names(object))], names(point_forecast), names_to = "Point forecast", values_to = dist_var)
  if(length(resp_var) > 1){
    object <- object[setdiff(names(object), resp_var)] %>% 
      tidyr::unpack(!!dist_var) %>% 
      tidyr::pivot_longer(names(object[[dist_var]]), names_to = ".response", values_to = dist_var)
  }
  
  mapping$y <- sym(dist_var)
  if(length(point_forecast) > 1){
    mapping$linetype <- sym("Point forecast")
    grp <- c(grp, mapping$linetype)
    mapping$group <- expr(interaction(!!!map(grp, function(x) expr(format(!!x))), sep = "/"))
  }
  object <- as_tibble(object)
  if(!is.null(col)){
    mapping$colour <- col
    out[[length(out) + 1]] <- geom_line(mapping = mapping, data = dplyr::anti_join(object, single_row, by = key_vars), ..., inherit.aes = FALSE, key_glyph = ggplot2::draw_key_timeseries)
    out[[length(out) + 1]] <- ggplot2::geom_point(mapping = mapping, data = dplyr::semi_join(object, single_row, by = key_vars), ..., inherit.aes = FALSE, key_glyph = ggplot2::draw_key_blank)
    out[[length(out) + 1]] <- ggplot2::labs(colour = col_nm)
  } else {
    out[[length(out) + 1]] <- geom_line(mapping = mapping, data = dplyr::anti_join(object, single_row, by = key_vars), color = colour, ..., inherit.aes = FALSE, key_glyph = ggplot2::draw_key_timeseries)
    out[[length(out) + 1]] <- ggplot2::geom_point(mapping = mapping, data = dplyr::semi_join(object, single_row, by = key_vars), color = colour, ..., inherit.aes = FALSE, key_glyph = ggplot2::draw_key_blank)
  }
  out
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
#' @param level If the decomposition contains distributions, which levels should be used to display intervals?
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
autoplot.dcmp_ts <- function(object, .vars = NULL, scale_bars = TRUE, 
                             level = c(80, 95), ...){
  method <- object%@%"method"
  idx <- index(object)
  keys <- key(object)
  n_keys <- n_keys(object)
  
  .vars <- enquo(.vars)
  if(quo_is_null(.vars)){
    .vars <- sym(response_vars(object))
  }
  dcmp_str <- dcmp <- (object%@%"aliases")[[expr_name(get_expr(.vars))]]
  if(!is.null(dcmp_str)){
    dcmp_str <- expr_text(dcmp_str)
  }
  object <- as_tsibble(object) %>% 
    transmute(!!.vars, !!!syms(all.vars(dcmp))) %>% 
    pivot_longer(measured_vars(.), values_to = ".val",
                 names_to = ".var", names_transform = list(.var = ~ factor(., levels = unique(.))))
  
  if(has_dist <- inherits(object[[".val"]], "distribution")) { 
    interval_data <- as_tibble(object)
    interval_data[paste0(level, "%")] <- lapply(level, hilo, x = interval_data[[".val"]])
    interval_data <- tidyr::pivot_longer(
      interval_data, paste0(level, "%"), names_to = NULL, values_to = "hilo"
    )
    intvl_aes <- aes(x = !!idx, hilo = !!sym("hilo"))
    line_aes <- aes(x = !!idx, y = mean(!!sym(".val")))
    if(n_keys > 1){
      line_aes$colour <- intvl_aes$fill <- expr(interaction(!!!keys, sep = "/"))
    }
    dcmp_geom <- list(
      distributional::geom_hilo_ribbon(intvl_aes, ..., data = interval_data),
      geom_line(line_aes, ...)
    )
  } else {
    line_aes <- aes(x = !!idx, y = !!sym(".val"))
    if(n_keys > 1){
      line_aes$colour <- expr(interaction(!!!keys, sep = "/"))
    }
    dcmp_geom <- geom_line(line_aes, ...)
  }
  
  p <- object %>% 
    ggplot() + 
    dcmp_geom + 
    facet_grid(vars(!!sym(".var")), scales = "free_y") + 
    ylab(NULL) + 
    labs(
      title = paste(method%||%"A", "decomposition"), 
      subtitle = paste(c(expr_text(get_expr(.vars)), dcmp_str), collapse = " = ")
    )
  
  # Rangebars
  if (scale_bars) {
    xranges <- range(object[[expr_name(idx)]])
    barwidth <- pmax(1, round((1 / 64) * diff(as.double(xranges))))
    
    # Avoid issues with visible bindings
    ymin <- ymax <- center <- diff <- NULL
    
    min_fn <- if(has_dist) function(x, ...) min(quantile(x, (100-max(level))/200), ...) else min
    max_fn <- if(has_dist) function(x, ...) max(quantile(x, (100 + max(level))/200), ...) else max
    
    range_data <- as_tibble(object) %>%
      group_by(!!sym(".var")) %>% 
      summarise(ymin = min_fn(!!sym(".val"), na.rm = TRUE), ymax = max_fn(!!sym(".val"), na.rm = TRUE)) %>% 
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
    p <- p + guides(colour = guide_legend(paste0(map_chr(keys, expr_name), collapse = "/")))
  }
  
  p
}
