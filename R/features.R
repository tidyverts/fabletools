features_impl <- function(.tbl, .var, features, ...){
  dots <- dots_list(...)
  
  if(is_function(features)){
    features <- list(features)
  }
  features <- map(squash(features), rlang::as_function)
  
  if(is.null(dots$.period)){
    dots$.period <- get_frequencies(NULL, .tbl, .auto = "smallest")
  }
  
  .resp <- map(.var, eval_tidy, data = .tbl)
  key_dt <- key_data(.tbl)
  out <- map(.resp, function(x){
    tbl <- imap(features, function(fn, nm){
      fmls <- formals(fn)[-1]
      tbl <- invoke(dplyr::bind_rows, map(key_dt[[".rows"]], function(i){
        do.call(fn, c(list(x[i]), dots[intersect(names(fmls), names(dots))]))
      }))
      if(is.character(nm) && nzchar(nm)){
        tbl <- set_names(tbl, paste(nm, colnames(tbl), sep = "_"))
      }
      tbl
    })
    invoke(dplyr::bind_cols, tbl)
  })
  
  if(!is.null(names(out))){
    out <- imap(out, function(tbl, nm){
      set_names(tbl, paste(nm, colnames(tbl), sep = "_"))
    })
  }
  
  dplyr::bind_cols(
    key_dt[-NCOL(key_dt)],
    !!!out
  )
}

#' Extract features from a dataset
#'
#' @param .tbl A dataset
#' @param .var,.vars The variable(s) to compute features on
#' @param features A list of functions (or lambda expressions) for the features to compute.
#' @param .predicate A predicate function (or lambda expression) to be applied to the columns or a logical vector. The variables for which .predicate is or returns TRUE are selected.
#' @param ... Additional arguments to be passed to each feature.
#'
#' @export
features <- function(.tbl, .var, features, ...){
  UseMethod("features")
}

#' @export
features.tbl_ts <- function(.tbl, .var = NULL, features = list(), ...){
  .var <- enquo(.var)
  if(quo_is_null(.var)){
    inform(sprintf(
      "Feature variable not specified, automatically selected `.var = %s`",
      measured_vars(.tbl)[1]
    ))
    .var <- as_quosure(sym(measured_vars(.tbl)[[1]]), env = empty_env())
  }
  else if(possibly(compose(is_quosures, eval_tidy), FALSE)(.var)){
    abort("`features()` only supports a single variable. To compute features across multiple variables consider scoped variants like `features_at()`")
  }
  
  features_impl(.tbl, list(.var), features, ...)
}

#' @rdname features
#' @export
features_at <- function(.tbl, .vars, features, ...){
  UseMethod("features_at")
}

#' @export
features_at.tbl_ts <- function(.tbl, .vars = NULL, features = list(), ...){
  .vars <- syms(tidyselect::vars_select(names(.tbl), !!!enquo(.vars)))
  features_impl(.tbl, syms(.vars), features = features, ...)
}

#' @rdname features
#' @export
features_all <- function(.tbl, features, ...){
  UseMethod("features_all")
}

#' @export
features_all.tbl_ts <- function(.tbl, features = list(), ...){
  features_impl(.tbl, syms(measured_vars(.tbl)), features = features, ...)
}

#' @rdname features
#' @export
features_if <- function(.tbl, .predicate, features, ...){
  UseMethod("features_if")
}

#' @export
features_if.tbl_ts <- function(.tbl, .predicate, features = list(), ...){
  .vars <- measured_vars(.tbl)
  .vars <- .vars[map_lgl(.tbl[.vars], rlang::as_function(.predicate))]
  features_impl(.tbl, syms(.vars), features = features, ...)
}