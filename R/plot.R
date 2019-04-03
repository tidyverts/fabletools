#' @importFrom ggplot2 ggplot aes geom_line guides guide_legend xlab
#' @export
autoplot.tbl_ts <- function(object, y = NULL, ...){
  if(quo_is_null(enquo(y))){
    inform(sprintf(
      "Plot variable not specified, automatically selected `y = %s`",
      measured_vars(object)[1]
    ))
    y <- sym(measured_vars(object)[1])
  }
  else{
    y <- enquo(y)
  }
  
  aes_spec <- list(x = index(object), y = y)
  if(n_keys(object) > 1){
    aes_spec["colour"] <- list(expr(interaction(!!!syms(key_vars(object)), sep = "/")))
  }
  p <- ggplot(object, eval_tidy(expr(aes(!!!aes_spec)))) + 
    geom_line() +
    xlab(paste0(expr_text(index(object)), " [", format(interval(object)), "]"))
  if(n_keys(object) > 1){
    p <- p + 
      guides(colour = guide_legend(paste0(map(syms(key_vars(object)), expr_text), collapse = "/")))
  }
  p
}

#' @importFrom ggplot2 ggplot aes geom_line guides guide_legend xlab
#' @export
autolayer.tbl_ts <- function(object, y = NULL, series = NULL, ...){
  if(quo_is_null(enquo(y))){
    inform(sprintf(
      "Plot variable not specified, automatically selected `y = %s`",
      measured_vars(object)[1]
    ))
    y <- sym(measured_vars(object)[1])
  }
  else{
    y <- enexpr(y)
  }
  
  aes_spec <- list(x = index(object), y = y)
  if(!is.null(series)){
    aes_spec$colour <- series
  }
  else if(n_keys(object) > 1){
    aes_spec["colour"] <- list(expr(interaction(!!!syms(key_vars(object)), sep = "/")))
  }
  
  geom_line(eval_tidy(expr(aes(!!!aes_spec))), data = object, ...)
}

#' @export
autoplot.mable <- function(object, ...){
  if(length(object$model) > 1){
    inform("Only univariate models are supported at the moment, plotting the first model.")
  }
  autoplot(object$model[[1]])
}

#' @importFrom ggplot2 fortify
#' @export
fortify.fbl_ts <- function(object, level = c(80, 95)){
  object <- object %>%
    as_tsibble %>% 
    mutate(!!!set_names(map(level, function(.x) expr(hilo(!!(object%@%"dist"), !!.x))), level)) %>%
    select(!!expr(-!!(object%@%"dist")))
  if(!is.null(level)){
    object <- object %>% 
      gather(level, hilo, !!!syms(as.character(level))) %>%
      mutate(hilo = add_class(hilo, "hilo"),
             level = level(hilo),
             lower = lower(hilo),
             upper = upper(hilo)) %>%
      select(!!expr(-!!sym("hilo")))
  }
  object
}

#' @export
autoplot.fbl_ts <- function(object, data = NULL, level = c(80, 95), ...){
  fc_key <- syms(setdiff(key_vars(object), ".model"))
  has_keys <- any(duplicated(key_data(object)$.model))
  
  if (!is.null(data)){
    if(!identical(fc_key, key(data))){
      abort("Provided data contains a different key structure to the forecasts.")
    }
    if(!is_empty(key(data))){
      data <- semi_join(data, object, by = key_vars(data))
    }
    
    p <- ggplot(data, aes(x = !!index(data), y = !!(object%@%"response"))) + 
      geom_line()
  }
  else{
    p <- ggplot()
  }
  
  p <- p +
    autolayer(object, level = level, ...)
  
  if(has_keys){
    p <- p + facet_grid(vars(!!!fc_key), scales = "free_y")
  }
  
  p
}

#' @export
autolayer.fbl_ts <- function(object, level = c(80, 95), series = NULL, ...){
  fc_key <- syms(setdiff(key_vars(object), ".model"))
  data <- fortify(object, level = level)
  mapping <- aes(
    x = !!index(data),
    y = !!sym(expr_text(object%@%"response"))
  )
  
  if(!is.null(level)){
    mapping$level <- sym("level")
    mapping$ymin <- sym("lower")
    mapping$ymax <- sym("upper")
  }
  
  if(!is_empty(fc_key)){
    mapping$group <- expr(interaction(!!!fc_key, sep = "/"))
  }
  
  if(!is.null(series)){
    mapping$colour <- series
    mapping$group <- expr(interaction(!!series, !!sym(".model")))
  }
  else if(length(unique(key_data(object)[[".model"]])) > 1){
    mapping$colour <- sym(".model")
    mapping$group <- sym(".model")
  }
  
  geom_forecast(mapping = mapping, stat = "identity", data = data, ...)
}

#' @importFrom ggplot2 ggplot geom_line geom_rect facet_grid vars ylab labs
#' @export
autoplot.dcmp_ts <- function(object, component = NULL, scale_bars = TRUE, ...){
  resp <- object%@%"resp"
  method <- object%@%"method"
  idx <- index(object)
  keys <- key(object)
  n_keys <- n_keys(object)
  
  component <- enquo(component)
  if(quo_is_null(component)){
    component <- resp
  }
  dcmp <- (object%@%"aliases")[[as_string(get_expr(component))]]
  object <- object %>% 
    transmute(!!component, !!!syms(all.vars(dcmp))) %>% 
    gather(".var", ".val", !!!syms(measured_vars(.)), factor_key = TRUE)
  
  line_aes <- aes(x = !!idx, y = !!sym(".val"))
  if(n_keys > 1){
    line_aes$colour <- expr(interaction(!!!keys, sep = "/"))
  }
  
  p <- object %>% 
    ggplot() + 
    geom_line(line_aes) + 
    facet_grid(vars(!!sym(".var")), scales = "free_y") + 
    ylab(NULL) + 
    labs(
      title = paste(method%||%"A", "decomposition"), 
      subtitle = paste(expr_text(resp), expr_text(dcmp), sep = " = ")
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
