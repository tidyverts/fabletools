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
                       MPE = MPE, MAPE = MAPE, MASE = MASE, RMSSE = RMSSE,
                       ACF1 = ACF1)

#' @rdname directional_accuracy_measures
#' 
#' @param reward,penalty The weights given to correct and incorrect predicted
#'   directions.
#' 
#' @export
MDA <- function(.resid, .actual, na.rm = TRUE, reward = 1, penalty = 0, ...){
  actual_change <- diff(.actual)
  actual_direction <- sign(actual_change)
  predicted_change <- actual_change - .resid[-1]
  predicted_direction <- sign(predicted_change)
  directional_error <- actual_direction == predicted_direction
  
  (reward-penalty) * mean(directional_error, na.rm = na.rm) + penalty
}

#' @rdname directional_accuracy_measures
#' @export
MDV <- function(.resid, .actual, na.rm = TRUE, ...){
  actual_change <- diff(.actual)
  actual_direction <- sign(actual_change)
  predicted_change <- actual_change - .resid[-1]
  predicted_direction <- sign(predicted_change)
  directional_accuracy <- ifelse(actual_direction == predicted_direction, 1, -1)
  mean(abs(actual_change) * directional_accuracy, na.rm = na.rm)
}

#' @rdname directional_accuracy_measures
#' @export
MDPV <- function(.resid, .actual, na.rm = TRUE, ...){
  actual_change <- diff(.actual)
  actual_direction <- sign(actual_change)
  predicted_change <- actual_change - .resid[-1]
  predicted_direction <- sign(predicted_change)
  directional_accuracy <- ifelse(actual_direction == predicted_direction, 1, -1)
  
  mean(abs(actual_change / .actual[-1]) * directional_accuracy, na.rm = na.rm) * 100
}

#' Directional accuracy measures
#' 
#' A collection of accuracy measures based on the accuracy of the prediction's 
#' direction (say, increasing or decreasing).
#' 
#' `MDA()`: Mean Directional Accuracy
#' `MDV()`: Mean Directional Value
#' `MDPV()`: Mean Directional Percentage Value
#' 
#' @inheritParams point_accuracy_measures
#' 
#' @references 
#' Blaskowitz and H. Herwartz (2011) "On economic evaluation of directional forecasts". \emph{International Journal of Forecasting},
#' \bold{27}(4), 1058-1065.
#' 
#' @export
directional_accuracy_measures <- list(MDA = MDA, MDV = MDV, MDPV = MDPV)

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
#' @importFrom stats quantile
#' @export
pinball_loss <- function(.dist, .actual, level = 95, na.rm = TRUE, ...){
  q <- stats::quantile(.dist, level/100)
  loss <- ifelse(.actual>=q, level/100 * (.actual-q), (1-level/100) * (q-.actual))
  2*mean(loss, na.rm = na.rm)
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
  
  q <- stats::quantile(.dist, level/100)
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
  quantile_score(.dist, .actual, probs = seq(0.01, 0.99, 0.01), na.rm = TRUE, ...)
}

#' @rdname distribution_accuracy_measures
#' @param probs A vector of probabilities at which the metric is evaluated.
#' @export
quantile_score <- function(.dist, .actual, probs = c(0.05,0.25,0.5,0.75,0.95),
                           na.rm = TRUE, ...){
  percentiles <- map(probs, quantile, x = .dist)
  if(!is.numeric(percentiles[[1]])) abort("Quantile scores are not supported for multivariate distributions.")
  2*mean(map2_dbl(percentiles, probs, function(percentile, prob){
    L <- ifelse(.actual < percentile, (1-prob), prob)*abs(percentile-.actual)
    mean(L, na.rm = na.rm)
  }), na.rm = na.rm)
}

#' @rdname distribution_accuracy_measures
#' @export
CRPS <- function(.dist, .actual, n_quantiles = 1000, na.rm = TRUE, ...){
  is_normal <- map_lgl(.dist, inherits, "dist_normal")
  is_sample <- map_lgl(.dist, inherits, "dist_sample")
  z <- rep(NA_real_, length(.dist))
  
  if(any(is_normal)){
    mean <- mean(.dist[is_normal])
    sd <- sqrt(distributional::variance(.dist[is_normal]))
    zn <- (.actual[is_normal]-mean)/sd
    zn <- sd*(zn*(2*stats::pnorm(zn)-1)+2*stats::dnorm(zn)-1/sqrt(pi))
    z[is_normal] <- zn
  }
  if(any(is_sample)){
    z[is_sample] <- map2_dbl(
      .dist[is_sample], .actual[is_sample], 
      function(d, y){
        x <- sort(d$x)
        m <- length(x)
        (2/m) * mean((x-y)*(m*(y<x) - seq_len(m) + 0.5))
      }
    )
  }
  is_default <- is.na(z)
  if(any(is_default)){
    probs <- seq(0, 1, length.out = n_quantiles + 2)[seq_len(n_quantiles) + 1]
    percentiles <- map(probs, quantile, x = .dist[is_default])
    if(!is.numeric(percentiles[[1]])) abort("Percentile scores are not supported for multivariate distributions.")
    za <- transpose_dbl(map2(percentiles, probs, function(percentile, prob){
      ifelse(.actual[is_default] < percentile, (1-prob), prob)*abs(percentile-.actual[is_default])
    }))
    
    z[is_default] <- vapply(za, mean, numeric(1L), na.rm = na.rm)*2
  }
  
  mean(z, na.rm = na.rm)
}

#' Distribution accuracy measures
#' 
#' These accuracy measures can be used to evaluate how accurately a forecast 
#' distribution predicts a given actual value.
#' 
#' @section Quantile/percentile score (pinball loss):
#' 
#' A quantile (or percentile) score evaluates how accurately a set of quantiles
#' (or percentiles) from the distribution match the given actual value. This 
#' score uses a pinball loss function, and can be calculated via the average of
#' the score function given below:
#' 
#' The score function \eqn{s_p(q_p,y)} is given by \eqn{(1-p)(q_p-y)} if 
#' \eqn{y < q_p}, and \eqn{p(y-q_p)} if \eqn{y \ge q_p}. Where \eqn{p} is the 
#' quantile probability, \eqn{q_p = F^{-1}(p)} is the quantile with probability 
#' \eqn{p}, and \eqn{y} is the actual value.
#' 
#' The resulting accuracy measure will average this score over all predicted
#' points at all desired quantiles (defined via the `probs` argument).
#' 
#' The percentile score is uses the same method with `probs` set to all 
#' percentiles `probs = seq(0.01, 0.99, 0.01)`.
#' 
#' @section Continuous ranked probability score (CRPS):
#' 
#' The continuous ranked probability score (CRPS) is the continuous analogue of
#' the pinball loss quantile score defined above. Its value is twice the 
#' integral of the quantile score over all possible quantiles:
#' 
#' \deqn{
#'  \text{CRPS}(F,y) = 2 \int_0^1 s_p(q_p,y) dp
#' }{
#'   CRPS(F,y) = 2 integral_0^1 s_p(q_p,y) dp
#' }
#' 
#' It can be computed directly from the distribution via:
#' 
#' \deqn{
#'   \text{CRPS}(F,y) = \int_{-\infty}^\infty (F(x) - 1{y\leq x})^2 dx
#' }{
#'   CRPS(F,y) = integral_{-\infty}^\infty (F(x) - 1{y\leq x})^2 dx
#' }
#' 
#' For some forecast distribution \eqn{F} and actual value \eqn{y}.
#' 
#' Calculating the CRPS accuracy measure is computationally difficult for many
#' distributions, however it can be computed quickly and exactly for Normal and
#' emperical (sample) distributions. For other distributions the CRPS is 
#' approximated using the quantile score of many quantiles (using the number of 
#' quantiles specified in the `n_quantiles` argument).
#' 
#' @inheritParams interval_accuracy_measures
#' @param n_quantiles The number of quantiles to use in approximating CRPS when an exact solution is not available.
#' 
#' @export
distribution_accuracy_measures <- list(percentile = percentile_score, CRPS = CRPS)


#' Forecast skill score measure
#' 
#' This function converts other error metrics such as `MSE` into a skill score.
#' The reference or benchmark forecasting method is the Naive method for
#' non-seasonal data, and the seasonal naive method for seasonal data.
#' When used within \code{\link{accuracy.fbl_ts}}, it is important that the data
#' contains both the training and test data, as the training data is used to
#' compute the benchmark forecasts.
#'
#' @param measure The accuracy measure to use in computing the skill score.
#' 
#' @examples 
#' 
#' skill_score(MSE)
#' 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibble)
#' 
#' lung_deaths <- as_tsibble(cbind(mdeaths, fdeaths))
#' lung_deaths %>% 
#'   dplyr::filter(index < yearmonth("1979 Jan")) %>%
#'   model(
#'     ets = ETS(value ~ error("M") + trend("A") + season("A")),
#'     lm = TSLM(value ~ trend() + season())
#'   ) %>%
#'   forecast(h = "1 year") %>%
#'   accuracy(lung_deaths, measures = list(skill = skill_score(MSE)))
#' }
#' 
#' @export
skill_score <- function(measure) {
  function(...) {
    # Compute accuracy measure for forecasts
    score <- measure(...)
    
    # Compute arguments of benchmark method using .train
    bench <- list(...)
    lag <- bench$.period
    n <- length(bench$.train)
    y <- bench$.train
    
    ## Compute point forecast from benchmark
    bench$.fc <- rep_len(
      y[c(rep(NA, max(0, lag - n)), seq_len(min(n, lag)) + n - min(n, lag))],
      length(bench$.fc)
    )
    bench$.resid <- bench$.actual - bench$.fc
    
    # Compute forecast distribution from benchmark
    e <- y - c(rep(NA, min(lag, n)), y[seq_len(length(y) - lag)])
    mse <- mean(e^2, na.rm = TRUE)
    h <- length(bench$.actual)
    fullperiods <- (h - 1) / lag + 1
    steps <- rep(seq_len(fullperiods), rep(lag, fullperiods))[seq_len(h)]
    bench$.dist <- distributional::dist_normal(bench$.fc, sqrt(mse * steps))
    ref_score <- do.call(measure, bench)
    
    1 - score / ref_score
  }
}

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
  if(is_tsibble(measures)){
    abort("The `measures` argument must contain a list of accuracy measures.
Hint: A tsibble of future values is only required when computing accuracy of a fable. To compute forecast accuracy, you'll need to compute the forecasts first.")
  }
  as_tibble(object) %>% 
    tidyr::pivot_longer(mable_vars(object), names_to = ".model", values_to = "fit") %>% 
    mutate(fit = map(!!sym("fit"), accuracy, measures = measures, ...)) %>% 
    unnest_tbl("fit")
}

#' @export
accuracy.mdl_ts <- function(object, measures = point_accuracy_measures, ...){
  dots <- dots_list(...)
  resp <- if(length(object$response) > 1) sym("value") else object$response[[1]]
  
  aug <- as_tibble(augment(object))
  
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
  
  
  if(!(".model" %in% by) & ".model" %in% names(object)){
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