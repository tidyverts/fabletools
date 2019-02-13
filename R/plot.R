#' @importFrom ggplot2 ggplot aes geom_line guides guide_legend xlab
#' @export
autoplot.tbl_ts <- function(object, var = NULL, ...){
  if(quo_is_null(enquo(var))){
    inform(sprintf(
      "Plot variable not specified, automatically selected `var = %s`",
      measured_vars(object)[1]
    ))
    var <- sym(measured_vars(object)[1])
  }
  else{
    var <- enexpr(var)
  }
  
  aes_spec <- list(x = index(object), y = var)
  if(n_keys(object) > 1){
    aes_spec["colour"] <- list(expr(interaction(!!!syms(key_vars(object)), sep = "/")))
  }
  ggplot(object, eval_tidy(expr(aes(!!!aes_spec)))) + 
    geom_line() +
    guides(colour = guide_legend(paste0(map(syms(key_vars(object)), expr_text), collapse = "/"))) + 
    xlab(paste0(expr_text(index(object)), " [", format(interval(object)), "]"))
}

#' @importFrom ggplot2 ggplot aes geom_line guides guide_legend xlab
#' @export
autolayer.tbl_ts <- function(object, var = NULL, ...){
  if(quo_is_null(enquo(var))){
    inform(sprintf(
      "Plot variable not specified, automatically selected `var = %s`",
      measured_vars(object)[1]
    ))
    var <- sym(measured_vars(object)[1])
  }
  else{
    var <- enexpr(var)
  }
  
  aes_spec <- list(x = index(object), y = var)
  if(n_keys(object) > 1){
    aes_spec["colour"] <- list(expr(interaction(!!!syms(key_vars(object)), sep = "/")))
  }
  
  geom_line(eval_tidy(expr(aes(!!!aes_spec))), data = object)
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
  if (!is.null(data)){
    p <- autoplot(data, !!(object%@%"response"))
  }
  else{
    p <- ggplot()
  }
  p +
    autolayer(object, level = level, ...)
}

#' @export
autolayer.fbl_ts <- function(object, level = c(80, 95), series = NULL, ...){
  data <- fortify(object, level = level, ...)
  mapping <- eval_tidy(quo(aes(x = !!index(data), y = !!(object%@%"response"))))
  if(!is.null(level)){
    mapping$level <- sym("level")
    mapping$ymin <- sym("lower")
    mapping$ymax <- sym("upper")
  }
  if(!is.null(series)){
    mapping$colour <- series
  }
  else if(!is_empty(key_vars(object))){
    mapping$colour <- expr(interaction(!!!syms(key_vars(object))))
  }
  geom_forecast(mapping = mapping, stat = "identity", data = data)
}