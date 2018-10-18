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

#' @export
autoplot.mable <- function(object, ...){
  if(length(object$model) > 1){
    inform("Only univariate models are supported at the moment, plotting the first model.")
  }
  autoplot(object$model[[1]])
}

# Add multiple via facets by rows
#' @export
autoplot.fable <- function(object, level = c(80, 95), ...){
  if(NROW(object)>1){
    warn("Only univariate forecast plots are currently supported. Plotting the first forecast.")
  }
  ggplot() +
    autolayer(new_fable(object[1,]), level = level, ...)
}

#' @export
autolayer.fable <- function(object, level = c(80, 95), series = NULL, ...){
  data <- fortify(object, level = level, ...)
  mapping <- eval_tidy(quo(aes(x = !!index(data), y = !!sym("mean"))))
  if(!is.null(level)){
    mapping$level <- sym("level")
    mapping$ymin <- sym("lower")
    mapping$ymax <- sym("upper")
  }
  if(!is.null(series)){
    mapping$colour <- series
  }
  geom_forecast(mapping = mapping, stat = "identity", data = data)
}

#' @importFrom ggplot2 fortify
#' @export
fortify.fable <- function(object, level = c(80, 95), showgap = TRUE){
  # Tidy format with repeated predicted values
  keys <- syms(key_vars(object))
  if(!showgap){
    extract_last_obs <- function(data, model) {
      data %>%
        filter(!!index(.) == last(!!index(.))) %>%
        transmute(!!!keys,
                  !!index(.),
                  mean = !!attr(!!sym("model"), "response"),
                  !!!set_names(map(level, function(.x) expr(new_hilo(mean, mean, !!.x))), level)
        )
    }
    gap <- suppressWarnings(object %>% 
      transmute(
        !!!keys,
        gap = map2(!!sym("data"), !!sym("model"), extract_last_obs)
      ) %>%
      unnest(gap, key = keys)
    )
  }
  else{
    gap <- NULL
  }
  tsbl <- suppressWarnings(
    object %>% 
      select(!!!keys, forecast) %>%
      mutate(
        forecast = map(forecast, 
                       function(fc){
                         fc %>%
                           mutate(!!!set_names(map(level, function(.x) expr(hilo(!!sym("distribution"), !!.x))), level)) %>%
                           select(exclude("distribution")) %>%
                           rbind(gap)
                       })
      ) %>%
      unnest(forecast, key = keys)
  )
  if(!is.null(level)){
    tsbl <- tsbl %>%
      gather(level, hilo, !!!syms(as.character(level))) %>%
      mutate(hilo = add_class(hilo, "hilo"),
             level = level(hilo),
             lower = lower(hilo),
             upper = upper(hilo)) %>%
      select(exclude("hilo"))
  }
  tsbl
}


#' @importFrom ggplot2 fortify
#' @export
fortify.fbl_ts <- function(object, level = c(80, 95)){
  object %>%
    mutate(!!!set_names(map(level, function(.x) expr(hilo(!!sym("distribution"), !!.x))), level)) %>%
    select(exclude("distribution")) %>%
    gather(level, hilo, !!!syms(as.character(level))) %>%
    mutate(hilo = add_class(hilo, "hilo"),
           level = level(hilo),
           lower = lower(hilo),
           upper = upper(hilo)) %>%
    select(exclude("hilo"))
}

#' @export
autoplot.fbl_ts <- function(object, level = c(80, 95), ...){
  if(NROW(object)>1){
    warn("Only univariate forecast plots are currently supported. Plotting the first forecast.")
  }
  ggplot() +
    autolayer(object, level = level, ...)
}

#' @export
autolayer.fbl_ts <- function(object, level = c(80, 95), series = NULL, ...){
  data <- fortify(object, level = level, ...)
  mapping <- eval_tidy(quo(aes(x = !!index(data), y = !!sym("mean"))))
  browser()
  if(!is.null(level)){
    mapping$level <- sym("level")
    mapping$ymin <- sym("lower")
    mapping$ymax <- sym("upper")
  }
  if(!is.null(series)){
    mapping$colour <- series
  }
  if(!is_empty(key_vars(object))){
    mapping$colour <- expr(interaction(!!!syms(key_vars(object))))
  }
  geom_forecast(mapping = mapping, stat = "identity", data = data)
}