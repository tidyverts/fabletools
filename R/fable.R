#' Create a new fable
#'
#' @param mable The mable used to produce the forecasts
#' @param forecast A list of tsibble forecasts (from `construct_f`)
#' @export
fable <- function(mable, forecast){
  mable[["forecast"]] <- forecast
  new_fable(mable)
}

#' Constructor
#' 
#' A constructor function for producing a fable (most useful for extension package authors)
#' 
#' @param x A fable-like object
#' 
#' @export
new_fable <- function(x){
  stopifnot(!is.null(x[["model"]]), !is.null(x[["forecast"]]))
  if(!inherits(x[["model"]], "lst_mdl")){
    x[["model"]] <- add_class(x[["model"]], "lst_mdl")
  }
  if(!inherits(x[["forecast"]], "lst_fc")){
    x[["forecast"]] <- add_class(x[["forecast"]], "lst_fc")
  }
  add_class(x, c("fable", "lst_ts"))
}


#' Coerce a dataset to a fable
#'
#' @inheritParams as_mable
#' @param forecast A bare input containing the forecast column's name
#'
#' @export
as_fable <- function(data, model, forecast){
  model <- enexpr(model)
  forecast <- enexpr(model)
  data %>%
    mutate(!!!list(model = expr(enclass(!!model, "lst_mdl")),
                   forecast = expr(enclass(!!forecast, "lst_mdl")))) %>%
    enclass("fable")
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.fable <- function(x){
  intervals <- x %>%
    pull(!!sym("data")) %>%
    map(interval) %>%
    unique
  if(length(intervals)==1){
    int_disp <- format(intervals[[1]])
  }
  else{
    int_disp <- "MIXED"
  }

  out <- c(`A fable` = sprintf("%s forecast%s [%s]", big_mark(NROW(x)), ifelse(NROW(x)==1, "", "s"), int_disp))

  if(!is_empty(key_vars(x))){
    out <- c(out, key_sum(x))
  }

  out
}

getPointForecast <- function(object, ...){
  object %>%
    transmute(!!!syms(key_vars(.)),
              mean = map(!!sym("forecast"), ~ transmute(.x, !!sym("mean")))
    ) %>%
    unnest(key = id(key_vars(object)))
}

#' @importFrom dplyr mutate_if
#' @export
summary.fable <- function(object, level=c(80,95), ...){
  suppressWarnings(
    object %>%
      select(!!!syms(key_vars(object)), "forecast") %>%
      mutate(
        forecast = map(forecast,
                       function(fc){
                         fc %>%
                           mutate(!!!set_names(map(level, ~ expr(hilo(!!sym("distribution"), !!.x))), paste0(level, "%"))) %>%
                           select(exclude("distribution"))
                       }
        )
      ) %>%
      unnest(forecast, key = id(key_vars(object))) %>%
      mutate_if(is.list, enclass, "hilo")
    )
}

#' @export
key.fable <- key.dable

#' @export
key_vars.fable <- function(x){
  setdiff(colnames(x), c("data", "model", "newdata", "forecast"))
}

#' @export
n_keys.fable <- function (x){
  key <- key_vars(x)
  if (is_empty(key)) {
    return(1L)
  }
  NROW(distinct(ungroup(as_tibble(x)), !!!syms(key)))
}