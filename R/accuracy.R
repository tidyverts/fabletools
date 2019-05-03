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
  mean(.resid, na.rm = na.rm)
}

#' @rdname point-accuracy-measures
#' @export
MSE <- function(.resid, na.rm = TRUE, ...){
  mean(.resid ^ 2, na.rm = na.rm)
}

#' @rdname point-accuracy-measures
#' @export
RMSE <- function(.resid, na.rm = TRUE, ...){
  sqrt(MSE(.resid, na.rm = na.rm))
}

#' @rdname point-accuracy-measures
#' @export
MAE <- function(.resid, na.rm = TRUE, ...){
  mean(abs(.resid), na.rm = na.rm)
}

#' @rdname point-accuracy-measures
#' @export
MPE <- function(.resid, .actual, na.rm = TRUE, ...){
  mean(.resid / .actual * 100, na.rm = na.rm)
}
#' @rdname point-accuracy-measures
#' @export
MAPE <- function(.resid, .actual, na.rm = TRUE, ...){
  mean(abs(.resid / .actual * 100), na.rm = na.rm)
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
    scale <- mean(abs(.train - mean(.train, na.rm = na.rm)), na.rm = na.rm)
  }
  else{
    scale <- mean(abs(.train), na.rm = na.rm)
  }
  mase <- mean(abs(.resid / scale), na.rm = na.rm)
}

#' @rdname point-accuracy-measures
#' @export
ACF1 <- function(.resid, na.action = stats::na.pass, demean = TRUE, ...){
  stats::acf(.resid, plot = FALSE, lag.max = 2, na.action = na.action, 
             demean = demean)$acf[2, 1, 1]
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
winkler_score <- function(.dist, .actual, level = 95, na.rm = TRUE, ...){
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
percentile_score <- function(.dist, .actual, na.rm = TRUE, ...){
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
#' 
#' 
#' @param object A model or forecast object
#' @param ... Additional arguments to be passed to measures that use it.
#' 
#' @export
accuracy <- function(object, ...){
  UseMethod("accuracy")
}

#' @rdname accuracy
#' 
#' @param measures A list of accuracy measures to compute (such as [`point_measures`], [`interval_measures`], or [`distribution_measures`])
#' 
#' @export
accuracy.mdl_df <- function(object, measures = point_measures, ...){
  as_tibble(object) %>% 
    gather(".model", "fit", !!!(object%@%"models")) %>% 
    unnest(fit = map(!!sym("fit"), accuracy, measures = measures, ...))
}

#' @export
accuracy.model <- function(object, measures = point_measures, ...){
  dots <- dots_list(...)
  
  aug <- as_tibble(augment(object)) %>% 
    rename(
      ".actual" := !!sym(deparse(model_lhs(object[["model"]]))),
    ) %>% 
    summarise(
      .resid = list(!!sym(".actual") - !!sym(".fitted")),
      .actual = list(!!sym(".actual")), .fc = list(!!sym(".fitted")),
      .train = !!sym(".actual")
    )
  
  # Add user inputs
  aug <- mutate(aug, ...)
  
  if(is.null(aug[[".period"]])){
    aug <- mutate(aug, .period = get_frequencies(NULL, object[["data"]], .auto = "smallest"))
  }
  
  measures <- squash(measures)
  
  aug %>% 
    nest(.key = ".accuracy_inputs") %>% 
    mutate(
      .accuracy_inputs = map(!!sym(".accuracy_inputs"), compose(flatten, transpose))
    ) %>% 
    unnest(
      .type = "Training",
      map(!!sym(".accuracy_inputs"), 
          function(measures, inputs) as_tibble(map(measures, do.call, inputs)),
          measures = measures),
      .drop = TRUE
    )
}

#' @param data A dataset containing the complete model dataset (both training and test data). The training portion of the data will be used in the computation of some accuracy measures, and the test data is used to compute the forecast errors.
#' @param by Variables over which the accuracy is computed (useful for computing across forecast horizons in cross-validation).
#' 
#' @rdname accuracy
#' @export
accuracy.fbl_ts <- function(object, data, measures = point_measures, ..., 
                            by = c(".model", key_vars(data))){
  by <- union(expr_text(index(object)), by)
  grp <- c(syms(setdiff(by, expr_text(index(object)))), groups(object))
  
  if(!(".model" %in% by)){
    warn('Accuracy measures should be computed separately for each model, have you forgotten to add ".model" to your `by` argument?')
  }
  
  if(NROW(missing_test <- anti_join(object, data, by = intersect(colnames(data), by))) > 0){
    warn(sprintf(
      "The future dataset is incomplete, incomplete out-of-sample data will be treated as missing. 
%i %s %s", 
      NROW(missing_test), 
      ifelse(NROW(missing_test)==1, "observation is missing at", "observations are missing between"),
      paste(unique(range(missing_test[[expr_text(index(missing_test))]])), collapse = " and ")
    ))
  }
  
  # Compute .fc, .dist, .actual and .resid
  aug <- transmute(object, .fc = !!(object%@%"response"), .dist = !!(object%@%"dist"), !!!syms(by))
  aug <- left_join(aug,
      transmute(data, !!index(data), .actual = !!(object%@%"response")),
      by = intersect(colnames(data), by),
      suffix = c("", ".y")
    )
  aug <- summarise(group_by(as_tibble(aug), !!!grp),
                   .resid = list(!!sym(".actual") - !!sym(".fc")),
                   .fc = list(!!sym(".fc")), .dist = list(!!sym(".dist")), 
                   .actual = list(!!sym(".actual")))
  
  # Extract training data (min key index, max grp index)
  extract_train <- function(idx, ...){
    cnds <- dots_list(...)
    cnds <- map2(syms(names(cnds)), cnds, call2, .fn = "==")
    eval_tidy(object%@%"response", data = filter(data, !!index(data) < idx, !!!cnds))
  }
  mutual_keys <- intersect(key(data), key(object))
  mutual_keys <- set_names(mutual_keys, map_chr(mutual_keys, as_string))
  .train <- object %>% 
    group_by(!!!key(object), !!!grp) %>% 
    filter(!!index(object) == min(!!index(object))) %>% 
    group_by(!!!grp) %>% 
    filter(!!index(object) == max(!!index(object))) %>% 
    transmute(.train = pmap(list2(!!index(object), !!!mutual_keys), extract_train))
  aug <- left_join(aug, .train, by = map_chr(grp, as_string))

  # Add user inputs
  aug <- mutate(aug, ...)
  
  if(is.null(aug[[".period"]])){
    aug <- mutate(aug, .period = get_frequencies(NULL, object, .auto = "smallest"))
  }
  
  measures <- squash(measures)
  
  aug %>% 
    group_by(!!!grp) %>% 
    nest(.key = ".accuracy_inputs") %>% 
    mutate(
      .accuracy_inputs = map(!!sym(".accuracy_inputs"), compose(flatten, transpose))
    ) %>% 
    unnest(
      .type = "Test",
      map(!!sym(".accuracy_inputs"), 
          function(measures, inputs) as_tibble(map(measures, do.call, inputs)),
          measures = measures),
      .drop = TRUE
    )
}