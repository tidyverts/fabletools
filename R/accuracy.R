#' Point estimate accuracy measures
#' 
#' @param .resid A vector of residuals from either the training (model accuracy)
#'  or test (forecast accuracy) data.
#' @param .actual A vector of responses matching the fitted values 
#' (for forecast accuracy, `new_data` must be provided).
#' @param .train A vector of responses used to train the model
#' (for forecast accuracy, the `orig_data` must be provided).
#' @param .period The seasonal period of the data (defaulting to 'smallest' seasonal period).
# #' @param .fc A vector containing the one-step-ahead fitted (forecasted) values 
#' from a model, or forecasted values from the forecast.
# #' @param .expr_resp An expression for the response variable.
#' @param na.rm Remove the missing values before calculating the accuracy measure
#' @param ... Additional arguments for each measure.
#' @param demean Should the response be demeaned (MASE)
#' @param d Should the response model include a first difference?
#' @param D Should the response model include a seasonal difference?
#' @param na.action Function to handle missing values.
#' 
#' @name point-accuracy-measures
NULL

#' @rdname point-accuracy-measures
#' @export
ME <- function(.resid, na.rm = TRUE, ...){
  mean(.resid, na.rm = na.rm, ...)
}

#' @rdname point-accuracy-measures
#' @export
MSE <- function(.resid, na.rm = TRUE, ...){
  mean(.resid ^ 2, na.rm = na.rm, ...)
}

#' @rdname point-accuracy-measures
#' @export
RMSE <- function(.resid, na.rm = TRUE, ...){
  sqrt(MSE(.resid, na.rm = na.rm, ...))
}

#' @rdname point-accuracy-measures
#' @export
MAE <- function(.resid, na.rm = TRUE, ...){
  mean(abs(.resid), na.rm = na.rm, ...)
}

#' @rdname point-accuracy-measures
#' @export
MPE <- function(.resid, .actual, na.rm = TRUE, ...){
  mean(.resid / .actual * 100, na.rm = TRUE, ...)
}
#' @rdname point-accuracy-measures
#' @export
MAPE <- function(.resid, .actual, na.rm = TRUE, ...){
  mean(abs(.resid / .actual * 100), na.rm = TRUE, ...)
}

#' @rdname point-accuracy-measures
#' @export
MASE <- function(.resid, .train, demean = FALSE, na.rm = TRUE, .period, d = .period == 1, D = .period > 1, ...){
  if (D > 0) { # seasonal differencing
    .train <- diff(.train, lag = .period, differences = D)
  }
  if (d > 0) {
    .train <- diff(.train, differences = d)
  }
  if(demean){
    scale <- mean(abs(.train - mean(.train, na.rm = na.rm, ...)), na.rm = na.rm, ...)
  }
  else{
    scale <- mean(abs(.train), na.rm = na.rm, ...)
  }
  mase <- mean(abs(.resid / scale), na.rm = na.rm, ...)
}

#' @rdname point-accuracy-measures
#' @export
ACF1 <- function(.resid, na.action = stats::na.pass, ...){
  stats::acf(.resid, plot = FALSE, lag.max = 2, na.action = na.action, ...)$acf[2, 1, 1]
}

#' @rdname point-accuracy-measures
#' @export
point_measures <- list(ME = ME, RMSE = RMSE, MAE = MAE,
                       MPE = MPE, MAPE = MAPE, MASE = MASE, ACF1 = ACF1)

#' Interval estimate accuracy measures
#' 
#' @inheritParams point-accuracy-measures
#' @param .dist The distribution of fitted values from the model, or forecasted values from the forecast.
#' @param level The level of the forecast interval.
#' 
#' @name interval-accuracy-measures
NULL

#' @rdname interval-accuracy-measures
#' @export
winkler_score <- function(.dist, .actual, level = 95, na.rm = TRUE){
  interval <- hilo(.dist, level)
  alpha <- 1-level/100
  lt <- interval$lower
  ut <- interval$upper
  score <- ifelse(
    .actual < lt, 
      (ut - lt) + (2/alpha)*(lt-.actual),
  ifelse(
    .actual > ut,
      (ut - lt) + (2/alpha)*(.actual-ut),
  # else     
    ut-lt)
  )
  mean(score, na.rm = na.rm)
}

#' @rdname interval-accuracy-measures
#' @export
interval_measures <- list(winkler = winkler_score)

#' Distribution accuracy measures
#' 
#' @inheritParams interval-accuracy-measures
#' @name dist-accuracy-measures
NULL

#' @rdname dist-accuracy-measures
#' @export
percentile_score <- function(.dist, .actual, na.rm = TRUE){
  probs <- seq(0.01, 0.99, 0.01)
  percentiles <- quantile(.dist, probs)
  map2_dbl(percentiles, probs, function(percentile, prob){
    L <- ifelse(.actual < percentile, (1-prob), prob)*abs(percentile-.actual)
    mean(L, na.rm = na.rm)
  }) %>% 
    mean(na.rm = na.rm)
}

#' @rdname interval-accuracy-measures
#' @export
distribution_measures <- list(percentile = percentile_score)

#' Evaluate model/forecast accuracy
#' 
#' @param x A model or forecast object
#' @param ... Additional arguments to be passed to measures that use it.
#' 
#' The measures calculated are:
#' \itemize{
#'   \item ME: Mean Error
#'   \item RMSE: Root Mean Squared Error
#'   \item MAE: Mean Absolute Error
#'   \item MPE: Mean Percentage Error
#'   \item MAPE: Mean Absolute Percentage Error
#'   \item MASE: Mean Absolute Scaled Error
#'   \item ACF1: Autocorrelation of errors at lag 1.
#' }
#' 
#' @export
accuracy <- function(x, ...){
  UseMethod("accuracy")
}

build_accuracy_calls <- function(measures, available_args){
  # Build measure calls  
  missing_args <- chr()
  fns <- map(measures, function(fn){
    args <- formals(fn)
    args <- args[names(args) != "..."]
    req_args <- names(args)[map_lgl(args, is_missing)]
    if(!all(match_req <- req_args %in% available_args)){
      missing_args <<- c(missing_args, req_args[!match_req])
      return(NULL)
    }
    
    # Function call
    inputs <- available_args[available_args%in%names(args)]
    call2(fn, !!!set_names(syms(inputs), inputs))
  })
  
  if(!is_empty(missing_args)){
    warn(
      sprintf("Could not estimate all measures as the following arguments are missing: %s",
              paste0(missing_args, collapse = ", "))
    )
  }
  
  names(fns) <- names(fns) %||% seq_along(fns)
  fns
}

#' @export
accuracy.mdl_df <- function(x, measures = point_measures, ...){
  as_tibble(x) %>% 
    gather(".model", "fit", !!!(x%@%"models")) %>% 
    unnest(fit = map(!!sym("fit"), accuracy, measures, ...))
}

#' @export
accuracy.model <- function(x, measures = list(point_measures, MASE = MASE), ...){
  dots <- dots_list(...)
  
  aug <- augment(x) %>% 
    rename(
      ".actual" := !!x[["response"]],
      ".fc" = ".fitted"
    ) %>% 
    mutate(
      .resid = !!sym(".actual") - !!sym(".fc"),
      .train = !!sym(".actual")
    )
  measures <- squash(measures)
  
  if(is.null(dots$.period)){
    dots$.period <- get_frequencies(NULL, aug, .auto = "smallest")
  }
  
  fns <- build_accuracy_calls(measures, c(names(dots), names(aug)))
  
  with(dots,
       aug %>% 
         as_tibble %>%
         summarise(
           .type = "Training",
           !!!compact(fns)
         ) %>% 
         ungroup()
  )
}

#' @export
accuracy.fbl_ts <- function(x, data, measures = point_measures, ...,
                            join_by = setdiff(key_vars(x), c(".model", ".id"))){
  dots <- dots_list(...)

  join_by <- union(expr_text(index(x)), join_by)
  
  aug <- x %>% 
    as_tsibble %>% 
    transmute(
      .fc = !!(x%@%"response"),
      .dist = !!(x%@%"dist")
    ) %>% 
    left_join(
      transmute(data, !!index(data), .actual = !!(x%@%"response")),
      by = join_by
    ) %>% 
    mutate(.resid = !!sym(".actual") - !!sym(".fc"))
  
  measures <- squash(measures)
  
  if(is.null(dots$.period)){
    dots$.period <- get_frequencies(NULL, aug, .auto = "smallest")
  }
  if(is.null(dots$.train)){
    orig_data <- anti_join(data, x, by = join_by)
    dots$.train <- eval_tidy(x%@%"response", data = orig_data)
  }
  
  fns <- build_accuracy_calls(measures, c(names(dots), names(aug)))
  with(dots,
    aug %>% 
      as_tibble %>% 
      group_by(!!!key(x), !!!groups(x)) %>% 
      summarise(
        .type = "Test",
        !!!compact(fns)
      ) %>% 
      ungroup()
  )
}