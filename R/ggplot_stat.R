#' @rdname geom_forecast
#' @export
StatForecast <- ggplot2::ggproto(
  "StatForecast", ggplot2::Stat,
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales, params, series=NULL,
                           model=ETS(y), fc_args = list(), levels = c(80, 95), ...) {
    model <- enexpr(model)
    if(inherits(scales$x, "ScaleContinuousDatetime")){
      index <- as.POSIXct(data$x, origin = "1970-01-01")
    }
    else if(inherits(scales$x, "ScaleContinuousDate")){
      index <- as.Date(data$x, origin = "1970-01-01")
    }
    else if(inherits(scales$x, "ScaleContinuous")){
      index <- data$x
    }
    else{
      abort("Cannot determine time scale on x-axis. Perhaps produce your forecasts separately, then use `autolayer()`")
    }
    if(inherits(scales$x, "ScaleContinuousYearmonth")){
      index <- tsibble::yearmonth(index)
    }
    else if(inherits(scales$x, "ScaleContinuousYearquarter")){
      index <- tsibble::yearquarter(index)
    }
    else if(inherits(scales$x, "ScaleContinuousYearweek")){
      index <- tsibble::yearweek(index)
    }

    plot_data <- tsibble(x = index, y = data$y, index=x)

    fit <- eval_tidy(quo(plot_data %>% model(!!model)))
    fcast <- do.call("forecast", append(list(fit), fc_args))
    fcast <- fortify(fcast, level = levels) %>% 
      mutate(x := as.numeric(!!sym("x")))
    
    aes_cn <- match(c("mean", "lower", "upper"), colnames(fcast))
    
    colnames(fcast)[aes_cn[!is.na(aes_cn)]] <- c("y", "ymin", "ymax")[!is.na(aes_cn)]
    
    extra_vars <- data[-na.omit(match(c("x", "y", "level", "ymin", "ymax"), colnames(data)))] %>%
      as.list %>%
      map(unique) %>%
      map(
        function(.x){
          if(length(.x)>1){
            warning("Plot parameters unable to be set by `stat_forecast`, defaulting to first available values")
          }
          .x[1]
        }
      )
    fcast %>%
      mutate(!!!extra_vars) %>% 
      as_tibble()
  }
)
