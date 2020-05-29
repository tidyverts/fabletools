#' @rdname point_accuracy_measures
#' @export
ME <- function(.resid, na.rm = TRUE, ...){
  mean(.resid, na.rm = na.rm)
}

#' @rdname point_accuracy_measures
#' @export
MSE <- function(.resid, na.rm = TRUE, ...){
  mean(.resid ^ 2, na.rm = na.rm)
}

#' @rdname point_accuracy_measures
#' @export
RMSE <- function(.resid, na.rm = TRUE, ...){
  sqrt(MSE(.resid, na.rm = na.rm))
}

#' @rdname point_accuracy_measures
#' @export
MAE <- function(.resid, na.rm = TRUE, ...){
  mean(abs(.resid), na.rm = na.rm)
}

#' @rdname point_accuracy_measures
#' @export
MPE <- function(.resid, .actual, na.rm = TRUE, ...){
  mean(.resid / .actual * 100, na.rm = na.rm)
}
#' @rdname point_accuracy_measures
#' @export
MAPE <- function(.resid, .actual, na.rm = TRUE, ...){
  mean(abs(.resid / .actual * 100), na.rm = na.rm)
}

#' @rdname point_accuracy_measures
#' @export
MASE <- function(.resid, .train, demean = FALSE, na.rm = TRUE, .period, d = .period == 1, D = .period > 1, ...){
  if (D > 0) { # seasonal differencing
    .train <- diff(.train, lag = .period, differences = D)
  }
  if (d > 0) {
    .train <- diff(.train, differences = d)
  }
  if(demean){
    .train <- .train - mean(.train, na.rm = na.rm)
  }
  scale <- mean(abs(.train), na.rm = na.rm)
  mean(abs(.resid / scale), na.rm = na.rm)
}

#' @rdname point_accuracy_measures
#' @export
RMSSE <- function(.resid, .train, demean = FALSE, na.rm = TRUE, .period, d = .period == 1, D = .period > 1, ...){
  if (D > 0) { # seasonal differencing
    .train <- diff(.train, lag = .period, differences = D)
  }
  if (d > 0) {
    .train <- diff(.train, differences = d)
  }
  if(demean){
    .train <- .train - mean(.train, na.rm = na.rm)
  }
  scale <- mean(.train^2, na.rm = na.rm)
  sqrt(mean(.resid^2 / scale, na.rm = na.rm))
}

#' @rdname point_accuracy_measures
#' @export
ACF1 <- function(.resid, na.action = stats::na.pass, demean = TRUE, ...){
  stats::acf(.resid, plot = FALSE, lag.max = 2, na.action = na.action, 
             demean = demean)$acf[2, 1, 1]
}

#' Mean Arctangent Absolute Percentage Error
#' 
#' @inheritParams point_accuracy_measures
#' 
#' @references 
#' Kim, Sungil and Heeyoung Kim (2016) "A new metric of absolute percentage error
#' for intermittent demand forecasts". \emph{International Journal of Forecasting},
#' \bold{32}(3), 669-679.
#' 
#' @export
MAAPE <- function(.resid, .actual, na.rm = TRUE, ...){
  mean(atan(abs(.resid / .actual * 100)), na.rm = na.rm)
}

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
#' @export
point_accuracy_measures <- list(ME = ME, RMSE = RMSE, MAE = MAE,
                       MPE = MPE, MAPE = MAPE, MASE = MASE, ACF1 = ACF1)

#' @rdname interval_accuracy_measures
#' @export
winkler_score <- function(.dist, .actual, level = 95, na.rm = TRUE, ...){
  interval <- hilo(.dist, level)
  if(!inherits(interval, "hilo")) abort("Winkler scores are not supported for multivariate distributions.")
  alpha <- 1-level/100
  lt <- vctrs::vec_proxy(interval)$lower
  ut <- vctrs::vec_proxy(interval)$upper
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

#' @rdname interval_accuracy_measures
#' @export
pinball_loss <- function(.dist, .actual, level = 95, na.rm = TRUE, ...){
  q <- quantile(.dist, level/100)
  loss <- ifelse(.actual>=q, level/100 * (.actual-q), (1-level/100) * (q-.actual))
  mean(loss, na.rm = na.rm)
}

#' @rdname interval_accuracy_measures
#' @export
scaled_pinball_loss <- function(.dist, .actual, .train, level = 95, na.rm = TRUE,
                                demean = FALSE, .period, d = .period == 1, 
                                D = .period > 1, ...){
  if (D > 0) { # seasonal differencing
    .train <- diff(.train, lag = .period, differences = D)
  }
  if (d > 0) {
    .train <- diff(.train, differences = d)
  }
  if(demean){
    .train <- .train - mean(.train, na.rm = na.rm)
  }
  scale <- mean(abs(.train), na.rm = na.rm)
  
  q <- quantile(.dist, level/100)
  loss <- ifelse(.actual>=q, level/100 * (.actual-q), (1-level/100) * (q-.actual))
  mean(loss/scale, na.rm = na.rm)
}

#' Interval estimate accuracy measures
#' 
#' @inheritParams point_accuracy_measures
#' @param .dist The distribution of fitted values from the model, or forecasted values from the forecast.
#' @param level The level of the forecast interval.
#' 
#' @export
interval_accuracy_measures <- list(winkler = winkler_score)

#' @rdname distribution_accuracy_measures
#' @export
percentile_score <- function(.dist, .actual, na.rm = TRUE, ...){
  probs <- seq(0.01, 0.99, 0.01)
  percentiles <- map(probs, quantile, x = .dist)
  if(!is.numeric(percentiles[[1]])) abort("Percentile scores are not supported for multivariate distributions.")
  map2_dbl(percentiles, probs, function(percentile, prob){
    L <- ifelse(.actual < percentile, (1-prob), prob)*abs(percentile-.actual)
    mean(L, na.rm = na.rm)
  }) %>% 
    mean(na.rm = na.rm)
}

#' @rdname distribution_accuracy_measures
#' @export
CRPS <- function(.dist, .actual, n_quantiles = 1000, na.rm = TRUE, ...){
  is_normal <- map_lgl(.dist, inherits, "dist_normal")
  z <- rep(NA_real_, length(.dist))
  
  if(any(is_normal)){
    mean <- mean(.dist[is_normal])
    sd <- sqrt(distributional::variance(.dist[is_normal]))
    zn <- (.actual[is_normal]-mean)/sd
    zn <- sd*(zn*(2*stats::pnorm(zn)-1)+2*stats::dnorm(zn)-1/sqrt(pi))
    z[is_normal] <- zn
  }
  
  if(any(!is_normal)){
    probs <- seq(0, 1, length.out = n_quantiles + 2)[seq_len(n_quantiles) + 1]
    percentiles <- map(probs, quantile, x = .dist[!is_normal])
    if(!is.numeric(percentiles[[1]])) abort("Percentile scores are not supported for multivariate distributions.")
    za <- map2_dbl(percentiles, probs, function(percentile, prob){
      L <- ifelse(.actual[!is_normal] < percentile, (1-prob), prob)*abs(percentile-.actual[!is_normal])
      mean(L, na.rm = na.rm)
    })
    
    z[!is_normal] <- za*2
  }
  
  mean(z, na.rm = na.rm)
}

#' Distribution accuracy measures
#' 
#' @inheritParams interval_accuracy_measures
#' @param n_quantiles The number of quantiles to use in approximating CRPS when an exact solution is not available.
#' 
#' @export
distribution_accuracy_measures <- list(percentile = percentile_score, CRPS = CRPS)

#' Evaluate accuracy of a forecast or model
#' 
#' Summarise the performance of the model using accuracy measures. Accuracy
#' measures can be computed directly from models as the one-step-ahead fitted
#' residuals are available. When evaluating accuracy on forecasts, you will
#' need to provide a complete dataset that includes the future data and data
#' used to train the model.
#' 
#' @param object A model or forecast object
#' @param ... Additional arguments to be passed to measures that use it.
#' 
#' @seealso 
#' [Evaluating forecast accuracy](https://otexts.com/fpp3/accuracy.html)
#' 
#' @export
accuracy <- function(object, ...){
  UseMethod("accuracy")
}

#' @rdname accuracy
#' 
#' @param measures A list of accuracy measure functions to compute (such as [`point_accuracy_measures`], [`interval_accuracy_measures`], or [`distribution_accuracy_measures`])
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibble)
#' library(tsibbledata)
#' library(dplyr)
#' 
#' fit <- aus_production %>%
#'   filter(Quarter < yearquarter("2006 Q1")) %>% 
#'   model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A")))
#' 
#' # In-sample training accuracy does not require extra data provided.
#' accuracy(fit)
#' 
#' # Out-of-sample forecast accuracy requires the future values to compare with.
#' # All available future data will be used, and a warning will be given if some
#' # data for the forecast window is unavailable.
#' fc <- fit %>% 
#'   forecast(h = "5 years")
#' fc %>% 
#'   accuracy(aus_production)
#'   
#' # It is also possible to compute interval and distributional measures of
#' # accuracy for models and forecasts which give forecast distributions.
#' fc %>% 
#'   accuracy(
#'     aus_production,
#'     measures = list(interval_accuracy_measures, distribution_accuracy_measures)
#'   )
#' }
#' 
#' @export
accuracy.mdl_df <- function(object, measures = point_accuracy_measures, ...){
  as_tibble(object) %>% 
    tidyr::pivot_longer(mable_vars(object), names_to = ".model", values_to = "fit") %>% 
    mutate(fit = map(!!sym("fit"), accuracy, measures = measures, ...)) %>% 
    unnest_tbl("fit")
}

#' @export
accuracy.mdl_ts <- function(object, measures = point_accuracy_measures, ...){
  dots <- dots_list(...)
  resp <- if(length(object$response) > 1) sym("value") else object$response[[1]]
  
  aug <- as_tibble(augment(object, type = "response"))
  
  # Compute inputs for each response variable
  if(length(object$response) > 1){
    aug <- group_by(aug, !!sym(".response"))
  }
  
  aug <- aug %>% 
    rename(
      ".actual" := !!resp,
    ) %>% 
    summarise(
      .resid = list(!!sym(".actual") - !!sym(".fitted")),
      .train = list(!!sym(".actual")),
      .actual = list(!!sym(".actual")), 
      .fc = list(!!sym(".fitted")),
    )
  
  # Re-group after summarise
  if(length(object$response) > 1){
    aug <- group_by(aug, !!sym(".response"))
  }
  
  # Add user inputs
  aug <- mutate(aug, ...)
  
  if(is.null(aug[[".period"]])){
    aug <- mutate(aug, .period = get_frequencies(NULL, object[["data"]], .auto = "smallest"))
  }
  aug <- aug %>% 
    nest_grps(nm = ".accuracy")
  
  measures <- map(aug[[".accuracy"]], function(measures, inputs){
    map(measures, safely(do.call, otherwise = NA_real_), flatten(transpose(inputs)))
  }, measures = squash(measures))
  
  measures <- imap(measures, function(x, nm){
    err <- map_lgl(x, function(x) !is.null(x[["error"]]))
    if((tot_err <- sum(err)) > 0){
      err_msg <- table(map_chr(x[err], function(x) x[["error"]][["message"]]))
      warn(
        sprintf("%i error%s encountered\n%s\n",
                tot_err,
                if(tot_err > 1) sprintf("s (%i unique)", length(err_msg)) else "", 
                paste0("[", err_msg, "] ", names(err_msg), collapse = "\n")
        )
      )
    }
    as_tibble(map(x, function(x) x[["result"]]))
  })
  
  aug %>% 
    mutate(
      .type = "Training",
      .accuracy = measures
    ) %>% 
    unnest_tbl(".accuracy")
}

#' @param data A dataset containing the complete model dataset (both training and test data). The training portion of the data will be used in the computation of some accuracy measures, and the test data is used to compute the forecast errors.
#' @param by Variables over which the accuracy is computed (useful for computing across forecast horizons in cross-validation). If `by` is `NULL`, groups will be chosen automatically from the key structure.
#' 
#' @rdname accuracy
#' @export
accuracy.fbl_ts <- function(object, data, measures = point_accuracy_measures, ..., 
                            by = NULL){
  resp <- response_vars(object)
  dist <- sym(distribution_var(object))
  
  if(is.null(by)){
    by <- intersect(c(".model", key_vars(data)), colnames(object))
    if(length(resp) > 1) by <- c(by, ".response")
  }
  
  grp <- c(syms(by), groups(object))
  by <- union(index_var(object), by)
  
  
  if(!(".model" %in% by)){
    warn('Accuracy measures should be computed separately for each model, have you forgotten to add ".model" to your `by` argument?')
  }
  
  if(NROW(missing_test <- suppressWarnings(anti_join(object, data, by = intersect(colnames(data), by)))) > 0){
    n_miss <- length(unique(missing_test[[index_var(missing_test)]]))
    warn(sprintf(
      "The future dataset is incomplete, incomplete out-of-sample data will be treated as missing. 
%i %s %s", 
      n_miss, 
      ifelse(n_miss==1, "observation is missing at", "observations are missing between"),
      paste(unique(range(missing_test[[index_var(missing_test)]])), collapse = " and ")
    ))
  }
  
  # Compute .fc, .dist, .actual and .resid
  object <- as_tsibble(object)
  
  if(mv <- length(resp) > 1){
    aug <- object <- tidyr::pivot_longer(
      transmute(object, mean(!!dist), .dist = !!dist, !!!syms(intersect(by, colnames(object)))),
      !!resp, names_to = ".response", values_to = ".fc")
    aug_dt <- data <- tidyr::pivot_longer(
      transmute(data, !!index(data), !!!syms(resp)),
      !!resp, names_to = ".response", values_to = ".actual")
    resp <- ".actual"
  } else {
    aug <- transmute(object, .fc = mean(!!dist), .dist = !!dist, !!!syms(by))
    aug_dt <- transmute(data, !!index(data), .actual = !!sym(resp))
  }
  aug <- left_join(aug, aug_dt,
      by = intersect(colnames(aug_dt), by),
      suffix = c("", ".y")
    )
  aug <- summarise(group_by(as_tibble(aug), !!!grp),
                   .resid = list(!!sym(".actual") - !!sym(".fc")),
                   .fc = list(!!sym(".fc")), .dist = list(!!sym(".dist")), 
                   .actual = list(!!sym(".actual")))
  
  # Extract training data (min key index, max grp index)
  extract_train <- function(idx, ...){
    cnds <- dots_list(...)
    cnds <- map2(syms(names(cnds)), cnds, function(x, y){
      if(is.na(y)) expr(is.na(!!x)) else expr(!!x == !!y)
    })
    eval_tidy(sym(resp), data = filter(data, !!index(data) < idx, !!!cnds))
  }
  mutual_keys <- intersect(key(data), key(object))
  mutual_keys <- set_names(mutual_keys, map_chr(mutual_keys, as_string))
  .train <- as_tibble(object) %>% 
    group_by(!!!union(key(object), grp)) %>% 
    filter(dplyr::row_number() == which.min(!!index(object))) %>% 
    group_by(!!!grp) %>% 
    filter(dplyr::row_number() == which.max(!!index(object))) %>% 
    transmute(.train = pmap(list2(idx = !!index(object), !!!mutual_keys), extract_train))
  aug <- left_join(aug, .train, by = map_chr(grp, as_string))

  # Add user inputs
  aug <- mutate(aug, ...)
  
  if(is.null(aug[[".period"]])){
    aug <- mutate(aug, .period = get_frequencies(NULL, object, .auto = "smallest"))
  }
  
  aug <- aug %>% 
    group_by(!!!grp) %>% 
    nest_grps(nm = ".accuracy")
  
  measures <- map(aug[[".accuracy"]], function(measures, inputs){
    map(measures, safely(do.call, otherwise = NA_real_), flatten(transpose(inputs)))
  }, measures = squash(measures))
  
  measures <- imap(measures, function(x, nm){
    err <- map_lgl(x, function(x) !is.null(x[["error"]]))
    if((tot_err <- sum(err)) > 0){
      err_msg <- table(map_chr(x[err], function(x) x[["error"]][["message"]]))
      warn(
        sprintf("%i error%s encountered\n%s\n",
                tot_err,
                if(tot_err > 1) sprintf("s (%i unique)", length(err_msg)) else "", 
                paste0("[", err_msg, "] ", names(err_msg), collapse = "\n")
        )
      )
    }
    as_tibble(map(x, function(x) x[["result"]]))
  })
  
  aug %>% 
    mutate(
      .type = "Test",
      .accuracy = measures
    ) %>% 
    unnest_tbl(".accuracy")
}