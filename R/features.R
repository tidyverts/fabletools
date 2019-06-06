tbl_features <- function(features){
  function(...){
    list(as_tibble(squash(map(features, function(.fn, ...) as.list(.fn(...)), ...))))
  }
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
  dots <- dots_list(...)
  
  if(is_function(features)){
    features <- list(features)
  }
  features <- map(squash(features), rlang::as_function)
  
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
  
  if(is.null(dots$.period)){
    dots$.period <- get_frequencies(NULL, .tbl, .auto = "smallest")
  }
  
  as_tibble(.tbl) %>%
    group_by(!!!key(.tbl)) %>%
    dplyr::summarise(
      .funs = tbl_features(features)(!!.var, !!!dots),
    ) %>%
    unnest_tbl(".funs") %>%
    dplyr::ungroup()
}

#' @rdname features
#' @export
features_at <- function(.tbl, .vars, features, ...){
  UseMethod("features_at")
}

#' @export
features_at.tbl_ts <- function(.tbl, .vars = NULL, features = list(), ...){
  dots <- dots_list(...)
  
  if(is_function(features)){
    features <- list(features)
  }
  features <- map(squash(features), rlang::as_function)
  
  quo_vars <- enquo(.vars)
  if(!possibly(compose(is_quosures, eval_tidy), FALSE)(.vars)){
    .vars <- new_quosures(list(quo_vars))
  }
  
  if(is.null(dots$.period)){
    dots$.period <- get_frequencies(NULL, .tbl, .auto = "smallest")
  }
  
  as_tibble(.tbl) %>%
    group_by(!!!key(.tbl), !!!dplyr::groups(.tbl)) %>%
    dplyr::summarise_at(
      .vars = .vars,
      .funs = tbl_features(features),
      !!!dots
    ) %>%
    unnest_tbl(., .sep = "_",
               setdiff(colnames(.), c(key_vars(.tbl), dplyr::group_vars(.tbl)))) %>%
    dplyr::ungroup()
}

#' @rdname features
#' @export
features_all <- function(.tbl, features, ...){
  UseMethod("features_all")
}

#' @export
features_all.tbl_ts <- function(.tbl, features = list(), ...){
  features_at(.tbl, .vars = as_quosures(syms(measured_vars(.tbl)), empty_env()),
              features = features, ...)
}

#' @rdname features
#' @export
features_if <- function(.tbl, .predicate, features, ...){
  UseMethod("features_if")
}

#' @export
features_if.tbl_ts <- function(.tbl, .predicate, features = list(), ...){
  mv_if <- map_lgl(.tbl[measured_vars(.tbl)], rlang::as_function(.predicate))
  features_at(.tbl,
              .vars = as_quosures(syms(measured_vars(.tbl)[mv_if]), empty_env()),
              features = features, ...)
}