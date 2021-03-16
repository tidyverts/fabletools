#' Create a fable object
#'
#' A fable (forecast table) data class (`fbl_ts`) which is a tsibble-like data
#' structure for representing forecasts. In extension to the key and index from 
#' the tsibble (`tbl_ts`) class, a fable (`fbl_ts`) must also contain a single
#' distribution column that uses values from the distributional package.
#'
#' @param ... Arguments passed to [tsibble::tsibble()].
#' @param response The character vector of response variable(s).
#' @param distribution The name of the distribution column (can be provided
#' using a bare expression).
#'
#' @export
fable <- function(..., response, distribution){
  build_fable(tsibble(...), !!enquo(response), !!enquo(distribution))
}

#' Is the object a fable
#' 
#' @param x An object.
#' 
#' @export
is_fable <- function(x){
  inherits(x, "fbl_ts")
}

#' Coerce to a fable object
#' 
#' @inheritParams fable
#' @param x Object to be coerced to a fable (`fbl_ts`)
#' @param ... Additional arguments passed to methods
#' 
#' @rdname as-fable
#' @export
as_fable <- function(x, ...){
  UseMethod("as_fable")
}

#' @rdname as-fable
#' @export
as_fable.tbl_ts <- function(x, response, distribution, ...){
  build_fable(x, 
              response = !!enquo(response),
              distribution = !!enquo(distribution))
}

#' @rdname as-fable
#' @export
as_fable.grouped_ts <- as_fable.tbl_ts

#' @rdname as-fable
#' @export
as_fable.tbl_df <- function(x, response, distribution, ...){
  build_fable(as_tsibble(x, ...), 
              response = !!enquo(response),
              distribution = !!enquo(distribution))
}

#' @rdname as-fable
#' @export
as_fable.fbl_ts <- function(x, response, distribution, ...){
  if(missing(response)){
    response <- response_vars(x)
  }
  else{
    response <- eval_tidy(enquo(response))
  }
  if(missing(distribution)){
    distribution <- distribution_var(x)
  }
  else{
    distribution <- names(x)[tidyselect::eval_select(enquo(distribution), x)]
  }
  build_fable(update_tsibble(x, ...),
              response = response, distribution = distribution)
}

#' @rdname as-fable
#' @export
as_fable.grouped_df <- as_fable.tbl_df

#' @inheritParams forecast.mdl_df
#' @rdname as-fable
#' @export
as_fable.forecast <- function(x, ..., point_forecast = list(.mean = mean)){
  if(is.null(x$upper)){
    # Without intervals, the best guess is the point forecast
    dist <- distributional::dist_degenerate(x$mean)
  } else {
    if(!is.null(x$lambda)){
      x$upper <- box_cox(x$upper, x$lambda)
      x$lower <- box_cox(x$lower, x$lambda)
    }
    warn("Assuming intervals are computed from a normal distribution.")
    level <- colnames(x$upper)[1]
    level <- as.numeric(gsub("^[^0-9]+|%", "", level))/100
    mid <- (x$upper[,1] - x$lower[,1])/2
    mu <- x$lower[,1] + mid
    sigma <- mid/(stats::qnorm((1+level)/2))
    dist <- distributional::dist_normal(mu = as.numeric(mu), sigma = as.numeric(sigma))
    if(!is.null(x$lambda)){
      dist <- distributional::dist_transformed(
        dist, 
        transform = rlang::new_function(exprs(x = ), expr(inv_box_cox(x, !!x$lambda)), env = rlang::pkg_env("fabletools")), 
        inverse = rlang::new_function(exprs(x = ), expr(inv_box_cox(x, !!x$lambda)), env = rlang::pkg_env("fabletools"))
      )
    }
  }
  out <- as_tsibble(x$mean)
  dimnames(dist) <- "value"
  out[["value"]] <- dist
  
  point_fc <- compute_point_forecasts(dist, point_forecast)
  out[names(point_fc)] <- point_fc
  
  build_fable(
    out,
    response = "value",
    distribution = "value"
  )
}

build_fable <- function (x, response, distribution) {
  # If the response (from user input) needs converting
  response <- eval_tidy(enquo(response))
  distribution <- names(x)[tidyselect::eval_select(enquo(distribution), x)]
  
  if(is_grouped_ts(x)){
    fbl <- structure(x, class = c("grouped_fbl", "grouped_ts", "grouped_df", 
                                  "fbl_ts", "tbl_ts", "tbl_df", "tbl", "data.frame"),
                     response = response, dist = distribution,
                     model_cn = ".model")
  } else {
    fbl <- tsibble::new_tsibble(
      x, response = response, dist = distribution, model_cn = ".model",
      class = "fbl_ts")
  }
  if(is.null(dimnames(fbl[[distribution]]))) {
    warn("The dimnames of the fable's distribution are missing and have been set to match the response variables.")
    dimnames(fbl[[distribution]]) <- response
  }
  if(!identical(response, dimnames(fbl[[distribution]]))) {
    dimnames(fbl[[distribution]]) <- response
  }
  validate_fable(fbl)
  fbl
}

#' @export
as_tsibble.fbl_ts <- function(x, ...){
  new_tsibble(x)
}

#' @export
as_tsibble.grouped_fbl <- function(x, ...){
  structure(x, class=setdiff(class(x), c("grouped_fbl", "fbl_ts")),
            response = NULL, dist = NULL, model_cn = NULL)
}

#' @export
as_tibble.fbl_ts <- function(x, ...) {
  new_tibble(vec_data(x), nrow = nrow(x))
}

#' @export
as_tibble.grouped_fbl <- function(x, ...) {
  dplyr::new_grouped_df(as_tibble(vec_data(x)), groups = group_data(x))
}

validate_fable <- function(fbl){
  stopifnot(inherits(fbl, "fbl_ts"))
  chr_dist <- distribution_var(fbl)
  if (!(chr_dist %in% names(fbl))){
    abort(sprintf("Could not find distribution variable `%s` in the fable. A fable must contain a distribution, if you want to remove it convert to a tsibble with `as_tsibble()`.",
                  chr_dist))
  }
  vec_assert(fbl[[chr_dist]], distributional::new_dist(dimnames = response_vars(fbl)))
}

tbl_sum.fbl_ts <- function(x){
  out <- NextMethod()
  names(out)[1] <- "A fable"
  out
}

#' @importFrom distributional hilo
#' @export
hilo.fbl_ts <- function(x, level = c(80, 95), ...){
  as_tsibble(x) %>%
    mutate(
      !!!set_names(map(level,function(.x) expr(hilo(!!sym(distribution_var(x)), !!.x))),
                   paste0(level, "%"))
    )
}

restore_fable <- function(data, template){
  data_cols <- names(data)
  
  # Variables to keep
  fbl_vars <- setdiff(distribution_var(template), data_cols)
  res <- bind_cols(data, template[fbl_vars])
  
  build_fable(data, response = response_vars(template), distribution = !!distribution_var(template))
}

#' @export
select.fbl_ts <- function (.data, ...){
  res <- select(as_tsibble(.data), ...)
  restore_fable(res, .data)
}

#' @export
select.grouped_fbl <- select.fbl_ts

#' @export
transmute.fbl_ts <- function (.data, ...) {
  res <- transmute(as_tsibble(.data), ...)
  restore_fable(res, .data)
}

#' @export
transmute.grouped_fbl <- transmute.fbl_ts

#' @export
group_by.fbl_ts <- function(.data, ...) {
  build_fable(NextMethod(), response_vars(.data), distribution_var(.data))
}

#' @export
group_by.grouped_fbl <- group_by.fbl_ts

#' @export
ungroup.fbl_ts <- group_by.fbl_ts

#' @export
ungroup.grouped_fbl <- group_by.fbl_ts

#' @export
fill_gaps.fbl_ts <- function(.data, ..., .full = FALSE) {
  vec_restore(NextMethod(.data), .data)
}

#' @export
rbind.fbl_ts <- function(...){
  deprecate_warn("0.2.0", "rbind.fbl_ts()", "bind_rows()")
  fbls <- dots_list(...)
  response <- map(fbls, response_vars)
  dist <- map(fbls, distribution_var)
  if(length(response <- unique(response)) > 1){
    abort("Cannot combine fables with different response variables.")
  }
  if(length(dist <- unique(dist)) > 1){
    abort("Cannot combine fables with different distribution names.")
  }
  out <- suppressWarnings(invoke(bind_rows, map(fbls, as_tsibble)))
  build_fable(out, response[[1]], dist[[1]])
}

#' @export
`[.fbl_ts` <- function (x, i, j, drop = FALSE){
  out <- NextMethod()
  # Drop fable if tsibble is dropped
  
  cn <- colnames(out)
  not_fable <- !(distribution_var(x) %in% cn) || !is_tsibble(out)
  
  if(not_fable)
    return(out)
  else
    build_fable(out, response_vars(x), distribution_var(x))
}

type_sum.fbl_ts <- function(x){
  "fable"
}