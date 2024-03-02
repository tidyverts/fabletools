#' Plot time series from a tsibble
#' 
#' Produces a time series plot of one or more variables from a tsibble. If the
#' tsibble contains a multiple keys, separate time series will be identified by
#' colour.
#' 
#' @param object A tsibble.
#' @param .vars A bare expression containing data you wish to plot. Multiple variables can be plotted using [`ggplot2::vars()`].
#' @param ... Further arguments passed to [`ggplot2::geom_line()`], which can be used to specify fixed aesthetics such as `colour = "red"` or `linewidth = 3`. 
#' 
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' library(fable)
#' library(tsibbledata)
#' library(tsibble)
#' 
#' tsibbledata::gafa_stock %>%
#'  autoplot(vars(Close, log(Close)))
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
fortify.fbl_ts <- function(model, data = NULL, level = c(80, 95), ...){
  return(as_tibble(model))
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
#' @param ... Further arguments passed used to specify fixed aesthetics for the forecasts such as `colour = "red"` or `linewidth = 3`.
#' @param point_forecast The point forecast measure to be displayed in the plot.
#' 
#' @examplesIf requireNamespace("fable", quietly = TRUE) && requireNamespace("tsibbledata", quietly = TRUE)
#' library(fable)
#' library(tsibbledata)
#' 
#' fc <- aus_production %>%
#'   model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A"))) %>% 
#'   forecast(h = "3 years") 
#' 
#' fc %>% 
#'   autoplot(aus_production)
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
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' aus_production %>% 
#'   autoplot(Beer) + 
#'   autolayer(fc)
#' 
#' @export
autolayer.fbl_ts <- function(object, data = NULL, level = c(80, 95), 
                             point_forecast = list(mean = mean), show_gap = TRUE, ...){
  build_fbl_layer(object = object, data = data, level = level, 
                  point_forecast = point_forecast, show_gap = show_gap, ...)

}

build_fbl_layer <- function(object, data = NULL, level = c(80, 95), 
                            colour = NULL, color = NULL, fill = NULL,
                            point_forecast = list(mean = mean), show_gap = TRUE, 
                            linetype = 1,
                            ..., facet_vars = NULL){
  mdl_key <- object%@%"model_cn"
  fc_key <- setdiff(key_vars(object), mdl_key)
  key_data <- key_data(object)
  key_vars <- key_vars(object)
  resp_var <- response_vars(object)
  dist_var <- distribution_var(object)
  idx <- index(object)
  common_models <- duplicated(key_data[[mdl_key]] %||% rep(TRUE, NROW(key_data(object))))
  colour <- colour %||% color %||% fill %||% "#446ffc"
  without <- function(x, el) x[setdiff(names(x), el)]
  
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
  
  # Single time point forecasts
  kd <- key_data(object)
  single_row <- lapply(split(kd$.rows, lengths(kd$.rows) == 1), unlist)
  
  out <- list()
  object <- object %>% 
    dplyr::mutate_if(~inherits(., "agg_vec"), compose(trimws, format))
  
  # Adapted from ggdist:::draw_key_lineribbon
  draw_key_ribbon <- function (self, data, params, size) {
    data$alpha <- data$alpha %||% NA
    if (is.null(data[["fill"]]) && (!is.null(data[["fill_ramp"]]) || !all(is.na(data[["alpha"]])))) {
      data$fill = "gray65"
    }
    # Apply ramped fill
    if (!is.null(data[["fill_ramp"]])) {
      if (utils::packageVersion("ggdist") > "3.3.1") {
        data$fill <- get("ramp_colours", asNamespace("ggdist"), mode = "function")(data$fill, data$fill_ramp)
      } else {
        data$fill <- mapply(function(color, amount){
          (scales::seq_gradient_pal(attr(amount, "from") %||% "white", color))(amount %||% NA)
        }, data$fill, data$fill_ramp)
      }
    }
    ggplot2::draw_key_rect(data, params, size)
  }
  
  # Add forecast interval ribbons to plot
  if(!is.null(level)){
    intvl_mapping <- mapping
    # intvl_mapping$dist <- sym(distribution_var(object))
    intvl_mapping$ymin <- sym(".lower")
    intvl_mapping$ymax <- sym(".upper")
    intvl_mapping$fill_ramp <- intvl_mapping$colour_ramp <- sym(".width")
    intvl_mapping$fill <- intvl_mapping$colour <- col
    
    dist_qi_frame <- function(data, level) {
      data <- ggdist::median_qi(as_tibble(data), !!sym(distribution_var(data)), .width = level/100)
      names(data)[match(".index", names(data))] <- ".response"
      data
    }
    
    if(!is.null(col)){
      if(length(single_row[["FALSE"]]) > 0) {
        out[[length(out) + 1L]] <- ggdist::geom_lineribbon(without(intvl_mapping, "colour_ramp"), data = dist_qi_frame(object[single_row[["FALSE"]],], level), ..., inherit.aes = FALSE, key_glyph = draw_key_ribbon)
      }
      if(length(single_row[["TRUE"]]) > 0) {
        out[[length(out) + 1L]] <- ggdist::stat_interval(intvl_mapping, data = dist_qi_frame(object[single_row[["TRUE"]],], level), ..., inherit.aes = FALSE, key_glyph = draw_key_ribbon)
      }
      out[[length(out) + 1L]] <- ggplot2::labs(fill = col_nm)
    } else {
      if(length(single_row[["FALSE"]]) > 0) {
        out[[length(out) + 1L]] <- ggdist::geom_lineribbon(without(intvl_mapping, "colour_ramp"), data = dist_qi_frame(object[single_row[["FALSE"]],], level), fill = colour, ..., inherit.aes = FALSE, key_glyph = draw_key_ribbon)
      }
      if(length(single_row[["TRUE"]]) > 0) {
        out[[length(out) + 1L]] <- ggdist::stat_interval(intvl_mapping, data = dist_qi_frame(object[single_row[["TRUE"]],], level), colour = colour, ..., inherit.aes = FALSE, key_glyph = draw_key_ribbon)
      }
    }
    
    # Add scale for confidence level ramp
    if(length(level) > 6) {
      level_breaks <- ggplot2::waiver()
      level_guide <- ggdist::guide_rampbar()
    } else {
      level_breaks <- level/100
      level_guide <- "legend"
    }
    if(length(single_row[["FALSE"]]) > 0) {
      out[[length(out) + 1L]] <- ggdist::scale_fill_ramp_continuous(name = "level", from = "white", breaks = level_breaks, limits = function(l) range(l), range = c(0.7, 0.3), labels = function(x) scales::percent(as.numeric(x)), guide = level_guide)
    }
    if(length(single_row[["TRUE"]]) > 0) {
      out[[length(out) + 1L]] <- ggdist::scale_colour_ramp_continuous(name = "level", from = "white", breaks = level_breaks, limits = function(l) range(l), range = c(0.7, 0.3), labels = function(x) scales::percent(as.numeric(x)), guide = level_guide)
    }
  }
  
  # Calculate point forecasts
  object <- as_tibble(object)
  object[names(point_forecast)] <- map(point_forecast, calc, object[[dist_var]])
  
  unpack_data <- function(x) {
    x <- tidyr::pivot_longer(x[-match(dist_var, names(x))], names(point_forecast), names_to = "Point forecast", values_to = dist_var)
    if(length(resp_var) > 1){
      x[[dist_var]] <- as_tibble(x[[dist_var]])
      x <- x[setdiff(names(x), resp_var)] %>% 
        tidyr::unpack(!!dist_var) %>% 
        tidyr::pivot_longer(names(x[[dist_var]]), names_to = ".response", values_to = dist_var)
    }
    x
  }
  
  # Add point forecasts to plot
  mapping$y <- sym(dist_var)
  if(length(point_forecast) > 1){
    mapping$linetype <- mapping$shape <- sym("Point forecast")
    grp <- c(grp, mapping$linetype)
    mapping$group <- expr(interaction(!!!map(grp, function(x) expr(format(!!x))), sep = "/"))
  }
  object <- as_tibble(object)
  if(!is.null(col)){
    mapping$colour <- col
    
    if(length(single_row[["FALSE"]]) > 0) {
      out[[length(out) + 1L]] <- geom_line(mapping = without(mapping, "shape"), data = unpack_data(object[single_row[["FALSE"]],]), ..., inherit.aes = FALSE)#, key_glyph = ggplot2::draw_key_timeseries)
    }
    if(length(single_row[["TRUE"]]) > 0) {
      out[[length(out) + 1L]] <- ggplot2::geom_point(mapping = without(mapping, "linetype"), data = unpack_data(object[single_row[["TRUE"]],]), size = 3, ..., inherit.aes = FALSE)
    }
    out[[length(out) + 1L]] <- ggplot2::labs(colour = col_nm)
  } else {
    if(length(single_row[["FALSE"]]) > 0) {
      out[[length(out) + 1L]] <- geom_line(mapping = without(mapping, "shape"), data = unpack_data(object[single_row[["FALSE"]],]), color = colour, ..., inherit.aes = FALSE)#, key_glyph = ggplot2::draw_key_timeseries)
    }
    if(length(single_row[["TRUE"]]) > 0) {
      out[[length(out) + 1L]] <- ggplot2::geom_point(mapping = without(mapping, "linetype"), data = unpack_data(object[single_row[["TRUE"]],]), color = colour, size = 3, ..., inherit.aes = FALSE)
    }
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
#' @examplesIf requireNamespace("feasts", quietly = TRUE) && requireNamespace("tsibbledata", quietly = TRUE)
#' library(feasts)
#' library(tsibbledata)
#' aus_production %>% 
#'   model(STL(Beer)) %>%
#'   components() %>%  
#'   autoplot()
#' 
#' @importFrom ggplot2 ggplot geom_line geom_rect facet_grid vars ylab labs after_stat
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
    intvl_aes <- aes(x = !!idx, dist = !!sym(".val"), fill_ramp = after_stat(level))
    line_aes <- aes(x = !!idx, y = mean(!!sym(".val")))
    if(n_keys > 1){
      line_aes$colour <- intvl_aes$fill <- intvl_aes$group <- expr(interaction(!!!keys, sep = "/"))
    }
    dcmp_geom <- list(
      if(n_keys > 1) {
        ggdist::stat_ribbon(intvl_aes, .width = level/100, ...)
      } else {
        ggdist::stat_ribbon(intvl_aes, fill = "gray65", .width = level/100, ...)
      },
      ggdist::scale_fill_ramp_discrete(from = "white", range = c(0.3, 0.7), labels = function(x) scales::percent(as.numeric(x))),
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
      subtitle = paste(c(expr_text(get_expr(.vars)), dcmp_str), collapse = " = "),
      fill = "Model", colour = "Model"
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
      fill = "gray75", colour = "black", linewidth = 1 / 3
    )
  }
  
  if(!is_empty(keys)){
    colour_title <- paste0(map_chr(keys, expr_name), collapse = "/")
    p <- p + labs(colour = colour_title, fill = colour_title)
  }
  
  p
}
