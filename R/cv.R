slide_period_with_step <- function(.x,
                                   .period,
                                   .f,
                                   ...,
                                   .before = 0L,
                                   .after = 0L,
                                   .step = 1L,
                                   .origin = NULL) {
  i <- .x[[index_var(.x)]]
  groups <- warp::warp_distance(i, .period, origin = .origin)
  
  full <- seq.int(groups[1], groups[length(i)])
  starts <- slider::slide_dbl(
    full,
    identity,
    .step = .step,
    .complete = TRUE
  )
  starts <- starts[!is.na(starts)]
  stops <- starts + .after
  if (is.infinite(.before)) {
    starts <- rep_len(starts[[1]], length(starts))
  }
  
  slider::hop_index(.x, groups, starts, stops, .f, ...)
}

compact_slide <- function(tsbl, ...) {
  args <- list2(...)
  args$.complete <- TRUE
  compact(exec(slider::slide, tsbl, !!!args))
}

#' Cross-validation
#'
#' `ExpandingWindow()` and `SlidingWindow()` iterate through each time series in
#' `data`, applying `.f` to each sub-window. `Holdout()` creates a single slice.
#'
#' Last observations are dropped if they cannot be included in the last slice
#' because of insufficient data points.
#'
#' Window parameters `.init`, `.size`, `.step`, and the cutoff `h` can be specfied
#' in terms of calendar periods, or in terms of number of observations 
#' if `.period` is NULL. The implementation relies on `slider::slide_period()`.
#'  
#' @param data A `tsibble`
#' @param h The forecast horizon for cut-off.
#' @param .init A positive integer for an initial window size.
#' @param .size A positive integer for window size.
#' @param .step A positive integer for incremental step.
#' @param .id A character naming the new column containing the 
#' 
#' @param ... Depending on the method:
#'    
#'   - `roll` : Additional arguments passed on to the mapped function.
#'   
#'   - `cv`: Definitions for the models to be used. All models must share the
#'     same response variable.
#'     
#' @inheritParams slider::slide_period
#' 
#' @return 
#'   - `roll` : A tibble with results stored in the column defined by `.id`. 
#' 
#' @inheritSection model Parallel
#' 
#' @examples
#' library(tsibbledata)
#' library(fable)
#'   
#' ExpandingWindow(.init = 10) %>% 
#'   roll(aus_retail, h = 5)
#' 
#' @seealso [accuracy()]
#' @rdname cross_validation
#' @name cross_validation
#' @export
ExpandingWindow <- function(.init = 1L, .step = 1L, .period = NULL) {
  new_rolling_window("expanding_window", Inf, .init, .step, .period)
}

#' @name cross_validation
#' @export
SlidingWindow <- function(.size = 1L, .step = 1L, .period = NULL) {
  new_rolling_window("expanding_window", 0, .size, .step, .period)
}

#' @name cross_validation
#' @export
Holdout <- function(.period = NULL) {
  object <- structure(
    list(
      slider = function(x, .f, ..., .period) {
        list(.f(x, ...))
      },
      args = list(.period = .period)
    ),
    class = c("holdout", "rolling_window")
  )
  invisible(object)
}

new_rolling_window <- function(cls,
                               .before = Inf,
                               .size = 1L,
                               .step = 1L,
                               .period = NULL) {
  args <- list(.before = .before, .after = .size - 1L, .step = .step)
  if (is.null(.period)) {
    slider <- compact_slide
  } else {
    slider <- slide_period_with_step
    args <- append(args, list(.period = .period))
  }
  object <- structure(
    list(slider = slider, args = args),
    class = c(cls, "rolling_window")
  )
  invisible(object)
}

#' @name cross_validation
#' @export
roll <- function(object, ...) {
  UseMethod("roll", object)
}

#' @name cross_validation
#' @export
roll.rolling_window <- function(object, data, h = 1,
                                .f = identity, ..., .id = ".fold") {
  period <- object$args$.period
  idx <- index_var(data)
  
  cut_horizon <- function(tsbl) {
    n <- NROW(tsbl)
    
    if (is.null(period)) {
      max_h <- n
      cutoff <- max_h - h
    } else {
      i <- tsbl[[idx]]
      last_idx <- i[[length(i)]]
      distances <- warp::warp_distance(i, period = period, origin = last_idx)
      cutoff <- which.min(distances <= -h) - 1
      max_h <- sprintf("%s %ss", -distances[[1]] + 1, period)
    }

    if (cutoff == 0 || cutoff >= n) {
      abort(sprintf("`h` is %s, but must be less than %s.", h, max_h))
    }
    
    dplyr::slice(tsbl, 1:cutoff)
  }
  
  out <- nest_keys(data, .id)
  
  if(is_attached("package:future")){
    require_package("future.apply")
    map_ <- function(...) future.apply::future_lapply(..., future.globals = FALSE)
  } else {
    map_ <- map
  } 
  args <- c(object$args, list(.f = .f), list2(...))
  
  out[[.id]] <- map_(out[[.id]], function(tsbl) {
    if (h > 0) {
      tsbl <- cut_horizon(tsbl)
    }
    exec(object$slider, tsbl, !!!args)
  })
  
  out
}

  
#' @inheritParams model
#'
#' @return 
#'   - `cv`: A [fable object][fable]
#'
#' @examples
#' ts <- aus_retail %>%
#'  filter(State %in% c("Queensland", "Victoria"), Industry == "Food retailing") 
#'  
#' models <- list(
#'   snaive = SNAIVE(Turnover),
#'   ets = TSLM(log(Turnover) ~ trend() + season())
#' )
#' 
#' suppressWarnings({
#' ExpandingWindow(.init = 25, .step = 1, .period = "year") %>% 
#'   cv(ts, h = 3, !!!models)
#' })
#' 
#' @name cross_validation
#' @export
cv <- function(object, ...) {
  UseMethod("cv", object)
}
  
#' @name cross_validation
#' @export
cv.rolling_window <- function(object, data, h, ..., .safely = TRUE, .id = ".fold") {
  models <- list2(...)
  
  fit_cv <- function(.data, ...) {
    .data %>%
      update_tsibble(validate = FALSE) %>% # update .nrows (outdated following slice)
      model(...)
  }
  
  folds <- roll(object, data, h, fit_cv, !!!models, .id = "..fold..")
  
  folds[[.id]] <- map(folds[["..fold.."]], function(fits) {
    map2_int(fits, seq_along(fits), function(.x, .y) rep_len(.y, NROW(.x)))
  })
  
  mdl <- folds %>% 
    unnest(c(.id, "..fold..")) %>%
    unnest("..fold..") %>%
    as_mable(key = c(.id, key_vars(data)), model = names(models))
  
  if (!is.null(object$args$.period)) {
    h <- paste(h, object$args$.period)
  }
  
  forecast(mdl, h = h)
}