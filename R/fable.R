#' Create a fable object
#'
#' @inheritParams tsibble::tsibble
#' @param resp The response variable (given as a bare or unquoted variable).
#' @param dist The distribution variable (given as a bare or unquoted variable).
#'
#' @export
fable <- function(..., key = id(), index, resp, dist, regular = TRUE){
  tsbl <- tsibble(..., key = !!enquo(key), index = !!enexpr(index), regular = regular)
  as_fable(tsbl)
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
as_fable.tbl_ts <- function(x, resp, dist, ...){
  fbl <- new_tsibble(x, class = "fbl_ts",
                     response = enexpr(resp), dist = enexpr(dist))
  validate_fable(fbl)
  fbl
}

#' @rdname as-fable
#' @export
as_fable.grouped_ts <- function(x, resp, dist, ...){
  fbl <- structure(x, class = c("grouped_fbl", "grouped_ts", "grouped_df", "fbl_ts", "tbl_ts", "tbl_df", "tbl", "data.frame"),
                   response = enexpr(resp), dist = enexpr(dist))
  validate_fable(fbl)
  fbl
}

#' @rdname as-fable
#' @export
as_fable.tbl_df <- function(x, resp, dist, ...){
  as_fable(as_tsibble(x, ...), resp = !!enexpr(resp), dist = !!enexpr(dist))
}

#' @rdname as-fable
#' @export
as_fable.fbl_ts <- function(x, resp, dist, ...){
  if(!missing(resp)){
    x%@%"resp" <- enexpr(resp)
  }
  if(!missing(dist)){
    x%@%"dist" <- enexpr(dist)
  }
  validate_fable(x)
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
            resp = NULL, dist = NULL)
}

validate_fable <- function(fbl){
  stopifnot(inherits(fbl, "fbl_ts"))
  if (!(expr_text(fbl%@%"response") %in% names(fbl))){
    abort(sprintf("Could not find response variable `%s` in the fable.",
          as_string(fbl%@%"response")))
  }
  if (!(as_string(fbl%@%"dist") %in% names(fbl))){
    abort(sprintf("Could not find distribution variable `%s` in the fable.",
          as_string(fbl%@%"dist")))
  }
  if (!inherits(fbl[[expr_text(fbl%@%"dist")]], "fcdist")){
    abort('Distribution variable must be of class "fcdist"')
  }
}

tbl_sum.fbl_ts <- function(x){
  out <- NextMethod()
  names(out)[1] <- "A fable"
  out
}

#' @export
report.fbl_ts <- function(object, level = c(80, 95), ...){
  object %>%
    transmute(
      !!(object%@%"response"),
      !!!set_names(map(level,function(.x) expr(hilo(!!(object%@%"dist"), !!.x))),
                   paste0(level, "%")))
}

#' @export
select.fbl_ts <- function (.data, ...){
  as_fable(NextMethod(), !!(.data%@%"response"), !!(.data%@%"dist"))
}

#' @export
select.grouped_fbl <- select.fbl_ts

filter.fbl_ts <- function (.data, ...){
  as_fable(NextMethod(), !!(.data%@%"response"), !!(.data%@%"dist"))
}

filter.grouped_fbl <- filter.fbl_ts

#' @export
group_by.fbl_ts <- function(.data, ...) {
  as_fable(NextMethod(), !!(.data%@%"response"), !!(.data%@%"dist"))
}

#' @export
group_by.grouped_fbl <- group_by.fbl_ts

#' @export
ungroup.fbl_ts <- group_by.fbl_ts

#' @export
ungroup.grouped_fbl <- group_by.fbl_ts

#' @export
mutate.fbl_ts <- function(.data, ...) {
  as_fable(NextMethod(), !!(.data%@%"response"), !!(.data%@%"dist"))
}

#' @export
mutate.grouped_fbl <- mutate.fbl_ts