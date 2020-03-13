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
  quo_response <- enquo(response)
  response <- possibly(eval_tidy, NULL)(quo_response)
  
  # If the response (from user input) needs converting
  if(!is.list(response)){
    if(quo_is_call(quo_response) && call_name(quo_response) == "c"){
      response[[1]] <- rlang::exprs
      response <- eval_tidy(quo_response)
    }
    else{
      response <- list(get_expr(quo_response))
    }
  }
  
  fbl <- new_tsibble(x, class = "fbl_ts",
                     response = response, dist = enexpr(distribution),
                     model_cn = ".model")
  validate_fable(fbl)
  fbl
}

#' @rdname as-fable
#' @export
as_fable.grouped_ts <- function(x, response, distribution, ...){
  quo_response <- enquo(response)
  response <- possibly(eval_tidy, NULL)(quo_response)
  
  # If the response (from user input) needs converting
  if(!is.list(response)){
    if(quo_is_call(quo_response) && call_name(quo_response) == "c"){
      quo_response[[1]] <- rlang::exprs
      response <- eval_tidy(quo_response)
    }
    else{
      response <- list(get_expr(quo_response))
    }
  }
  
  fbl <- structure(x, class = c("grouped_fbl", "grouped_ts", "grouped_df", 
                                "fbl_ts", "tbl_ts", "tbl_df", "tbl", "data.frame"),
                   response = response, dist = enexpr(distribution),
                   model_cn = ".model")
  validate_fable(fbl)
  fbl
}

#' @rdname as-fable
#' @export
as_fable.tbl_df <- function(x, response, distribution, ...){
  as_fable(as_tsibble(x, ...), response = !!enquo(response),
           distribution = !!enexpr(distribution))
}

#' @rdname as-fable
#' @export
as_fable.fbl_ts <- function(x, response, distribution, ...){
  if(missing(response)){
    response <- x%@%"response"
  }
  else{
    quo_response <- enquo(response)
    # If the response (from user input) needs converting
    if(quo_is_call(quo_response) && call_name(quo_response) == "c"){
      response[[1]] <- rlang::exprs
      response <- eval_tidy(quo_response)
    }
    else{
      response <- list(get_expr(quo_response))
    }
  }
  if(missing(distribution)){
    distribution <- x%@%"dist"
  }
  as_fable(update_tsibble(x, ...), response = response,
           distribution = !!enexpr(distribution))
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
  chr_resp <- map_chr(attr(fbl, "response"), expr_text)
  chr_dist <- as_string(attr(fbl, "dist"))
  if (!all(chr_resp %in% names(fbl))){
    bad_resp <- paste0(setdiff(chr_resp, names(fbl)), collapse = ", ")
    abort(sprintf("Could not find response variable(s) in the fable: %s", bad_resp))
  }
  if (!(chr_dist %in% names(fbl))){
    abort(sprintf("Could not find distribution variable `%s` in the fable. A fable must contain a distribution, if you want to remove it convert to a tsibble with `as_tsibble()`.",
                  chr_dist))
  }
  if (!inherits(fbl[[chr_dist]], "fcdist")){
    abort('Distribution variable must be of class "fcdist"')
  }
}

tbl_sum.fbl_ts <- function(x){
  out <- NextMethod()
  names(out)[1] <- "A fable"
  out
}

#' @rdname hilo
#' @export
hilo.fbl_ts <- function(x, level = c(80, 95), ...){
  x %>%
    transmute(
      !!!(x%@%"response"),
      !!!set_names(map(level,function(.x) expr(hilo(!!(x%@%"dist"), !!.x))),
                   paste0(level, "%"))
    )
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
  out <- suppressWarnings(invoke("rbind", map(fbls, as_tsibble)))
  class(out[[as_string(dist[[1]])]]) <- c("fcdist", "list")
  as_fable(out, response[[1]], !!dist[[1]])
}

#' @export
`[.fbl_ts` <- function (x, i, j, drop = FALSE){
  as_fable(NextMethod(), x%@%"response", !!(x%@%"dist"))
}

type_sum.fbl_ts <- function(x){
  "fable"
}