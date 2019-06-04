#' Create a fable object
#'
#' @param ... Arguments passed to [tsibble::tsibble()].
#' @param resp The response variable (a list of expressions).
#' @param dist The distribution variable (given as a bare or unquoted variable).
#'
#' @export
fable <- function(..., resp, dist){
  tsbl <- tsibble(...)
  as_fable(tsbl, resp, !!enquo(dist))
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
                     response = resp, dist = enexpr(dist))
  validate_fable(fbl)
  fbl
}

#' @rdname as-fable
#' @export
as_fable.grouped_ts <- function(x, resp, dist, ...){
  fbl <- structure(x, class = c("grouped_fbl", "grouped_ts", "grouped_df", 
                                "fbl_ts", "tbl_ts", "tbl_df", "tbl", "data.frame"),
                   response = resp, dist = enexpr(dist))
  validate_fable(fbl)
  fbl
}

#' @rdname as-fable
#' @export
as_fable.tbl_df <- function(x, resp, dist, ...){
  as_fable(as_tsibble(x, ...), resp = resp, dist = !!enexpr(dist))
}

#' @rdname as-fable
#' @export
as_fable.fbl_ts <- function(x, resp, dist, ...){
  if(!missing(resp)){
    x%@%"response" <- resp
  }
  if(!missing(dist)){
    x%@%"dist" <- enexpr(dist)
  }
  validate_fable(x)
  x
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
  if (!all(map_chr(fbl%@%"response", expr_text) %in% names(fbl))){
    bad_resp <- map_chr(fbl%@%"response", expr_text)
    bad_resp <- paste0(setdiff(bad_resp, names(fbl)), collapse = ", ")
    abort(sprintf("Could not find response variable(s) in the fable: %s", bad_resp))
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
      !!!(object%@%"response"),
      !!!set_names(map(level,function(.x) expr(hilo(!!(object%@%"dist"), !!.x))),
                   paste0(level, "%")))
}

#' @export
select.fbl_ts <- function (.data, ...){
  as_fable(NextMethod(), .data%@%"response", !!(.data%@%"dist"))
}

#' @export
select.grouped_fbl <- select.fbl_ts

filter.fbl_ts <- function (.data, ...){
  as_fable(NextMethod(), .data%@%"response", !!(.data%@%"dist"))
}

filter.grouped_fbl <- filter.fbl_ts

#' @export
group_by.fbl_ts <- function(.data, ...) {
  as_fable(NextMethod(), .data%@%"response", !!(.data%@%"dist"))
}

#' @export
group_by.grouped_fbl <- group_by.fbl_ts

#' @export
ungroup.fbl_ts <- group_by.fbl_ts

#' @export
ungroup.grouped_fbl <- group_by.fbl_ts

#' @export
mutate.fbl_ts <- function(.data, ...) {
  as_fable(NextMethod(), .data%@%"response", !!(.data%@%"dist"))
}

#' @export
mutate.grouped_fbl <- mutate.fbl_ts

#' @export
rbind.fbl_ts <- function(...){
  fbls <- dots_list(...)
  response <- map(fbls, attr, "response")
  dist <- map(fbls, attr, "dist")
  if(length(response <- unique(response)) > 1){
    abort("Cannot combine fables with different response variables.")
  }
  if(length(dist <- unique(dist)) > 1){
    abort("Cannot combine fables with different distribution names.")
  }
  out <- update_tsibble(NextMethod("rbind"))
  as_fable(out, response[[1]], !!dist[[1]])
}

type_sum.fbl_ts <- function(x){
  "fable"
}