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
                       MPE = MPE, MAPE = MAPE, ACF1 = ACF1)

#' Interval estimate accuracy measures
#' 
#' @inheritParams point-accuracy-measures
#' @param .dist The distribution of fitted values from the model, or forecasted values from the forecast.
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
#' @param ... Additional arguments to be passed to other methods
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
accuracy.mdl_df <- function(x, measures = list(point_measures), ...){
  dots <- dots_list(...)
  aug <- augment(x) %>% 
    rename(
      ".actual" := !!response(x[["model"]][[1]]),
      ".fc" = ".fitted"
    ) %>% 
    mutate(
      .resid = !!sym(".actual") - !!sym(".fc"),
      .train = !!sym(".actual")
    )
  measures <- squash(measures)
  
  if(is.null(dots$.period)){
    dots$.period <- get_frequencies("smallest", aug)
  }
  
  fns <- build_accuracy_calls(measures, c(names(dots), names(aug)))
  
  aug %>% 
    group_by_key %>% 
    as_tibble %>% 
    summarise(
      Type = "Training",
      !!!compact(fns)
    )
}

#' @export
accuracy.fbl_ts <- function(x, new_data, measures = list(point_measures), ...){
  dots <- dots_list(...)

  aug <- x %>% 
    transmute(
      .fc = !!response(x),
      .dist = !!(x%@%"dist")
    ) %>% 
    left_join(
      transmute(new_data, !!index(new_data), .actual = !!response(x)),
      by = c(expr_text(index(x)), key_vars(x))
    ) %>% 
    mutate(.resid = !!sym(".actual") - !!sym(".fc"))
  
  measures <- squash(measures)
  
  if(is.null(dots$.period)){
    dots$.period <- get_frequencies("smallest", aug)
  }
  
  fns <- build_accuracy_calls(measures, c(names(dots), names(aug)))
  aug %>% 
    group_by_key %>% 
    as_tibble %>% 
    summarise(
      Type = "Test",
      !!!compact(fns)
    )
}