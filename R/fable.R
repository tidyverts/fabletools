#' Create a fable object
#'
#' A fable (forecast table) data class (`fbl_ts`) which is a tsibble-like data
#' structure for representing forecasts. In extension to the key and index from 
#' the tsibble (`tbl_ts`) class, a fable (`fbl_ts`) must contain columns of 
#' point forecasts for the response variable(s), and a single distribution 
#' column (`fcdist`).
#'
#' @param ... Arguments passed to [tsibble::tsibble()].
#' @param response The response variable(s). A single response can be specified
#' directly via `response = y`, multiple responses should be use `response = c(y, z)`.
#' @param distribution The distribution variable (given as a bare or unquoted variable).
#'
#' @export
fable <- function(..., response, distribution){
  tsbl <- tsibble(...)
  as_fable(tsbl, !!enquo(response), !!enquo(distribution))
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
  response <- expr_name(get_expr(enquo(response)))
  distribution <- names(x)[tidyselect::eval_select(enquo(distribution), x)]
  
  fbl <- new_tsibble(x, class = "fbl_ts",
                     response = response, dist = distribution,
                     model_cn = ".model")
  validate_fable(fbl)
  fbl
}

#' @rdname as-fable
#' @export
as_fable.grouped_ts <- function(x, response, distribution, ...){
  # If the response (from user input) needs converting
  response <- expr_name(get_expr(enquo(response)))
  distribution <- names(x)[tidyselect::eval_select(enquo(distribution), x)]
  
  fbl <- structure(x, class = c("grouped_fbl", "grouped_ts", "grouped_df", 
                                "fbl_ts", "tbl_ts", "tbl_df", "tbl", "data.frame"),
                   response = response, dist = distribution,
                   model_cn = ".model")
  validate_fable(fbl)
  fbl
}

#' @rdname as-fable
#' @export
as_fable.tbl_df <- function(x, response, distribution, ...){
  as_fable(as_tsibble(x, ...), response = !!enquo(response),
           distribution = !!enquo(distribution))
}

#' @rdname as-fable
#' @export
as_fable.fbl_ts <- function(x, response, distribution, ...){
  if(missing(response)){
    response <- response_var(x)
  }
  else{
    response <- expr_name(get_expr(enquo(response)))
  }
  if(missing(distribution)){
    distribution <- distribution_var(x)
  }
  else{
    distribution <- names(x)[tidyselect::eval_select(enquo(distribution), x)]
  }
  as_fable(update_tsibble(x, ...), response = response,
           distribution = distribution)
}

#' @rdname as-fable
#' @export
as_fable.grouped_df <- as_fable.tbl_df

#' @export
as_tsibble.fbl_ts <- function(x, ...){
  new_tsibble(x)
}

#' @export
as_tsibble.grouped_fbl <- function(x, ...){
  structure(x, class=setdiff(class(x), c("grouped_fbl", "fbl_ts")),
            response = NULL, dist = NULL, model_cn = NULL)
}

validate_fable <- function(fbl){
  stopifnot(inherits(fbl, "fbl_ts"))
  chr_resp <- response_var(fbl)
  chr_dist <- distribution_var(fbl)
  if (!(chr_dist %in% names(fbl))){
    abort(sprintf("Could not find distribution variable `%s` in the fable. A fable must contain a distribution, if you want to remove it convert to a tsibble with `as_tsibble()`.",
                  chr_dist))
  }
  vec_is(fbl[[chr_dist]], distributional::new_dist())
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

#' @export
select.fbl_ts <- function (.data, ...){
  as_fable(NextMethod(), response_var(.data), distribution_var(.data))
}

#' @export
select.grouped_fbl <- select.fbl_ts

filter.fbl_ts <- function (.data, ...){
  as_fable(NextMethod(), response_var(.data), distribution_var(.data))
}

filter.grouped_fbl <- filter.fbl_ts

#' @export
group_by.fbl_ts <- function(.data, ...) {
  as_fable(NextMethod(), response_var(.data), distribution_var(.data))
}

#' @export
group_by.grouped_fbl <- group_by.fbl_ts

#' @export
ungroup.fbl_ts <- group_by.fbl_ts

#' @export
ungroup.grouped_fbl <- group_by.fbl_ts

#' @export
mutate.fbl_ts <- function(.data, ...) {
  as_fable(NextMethod(), response_var(.data), distribution_var(.data))
}

#' @export
mutate.grouped_fbl <- mutate.fbl_ts

#' @export
rbind.fbl_ts <- function(...){
  fbls <- dots_list(...)
  response <- map(fbls, response_var)
  dist <- map(fbls, distribution_var)
  if(length(response <- unique(response)) > 1){
    abort("Cannot combine fables with different response variables.")
  }
  if(length(dist <- unique(dist)) > 1){
    abort("Cannot combine fables with different distribution names.")
  }
  out <- suppressWarnings(invoke(dplyr::bind_rows, map(fbls, as_tsibble)))
  as_fable(out, response[[1]], dist[[1]])
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
    as_fable(out, response_var(x), distribution_var(x))
}

response_var <- function(x){
  stopifnot(is_fable(x))
  x%@%"response"
}

distribution_var <- function(x){
  stopifnot(is_fable(x))
  x%@%"dist"
}

type_sum.fbl_ts <- function(x){
  "fable"
}