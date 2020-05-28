features_impl <- function(.tbl, .var, features, ...){
  dots <- dots_list(...)
  
  if(is_function(features)){
    features <- list(features)
  }
  features <- map(squash(features), as_function)
  
  if(is.null(dots$.period)){
    dots$.period <- get_frequencies(NULL, .tbl, .auto = "smallest")
  }
  
  # Compute response
  key_dt <- key_data(.tbl)
  .tbl <- as_tibble(.tbl)
  if(NCOL(key_dt) > 1){
    .tbl <- dplyr::new_grouped_df(.tbl, key_dt)
  }
  .resp <- unclass(dplyr::transmute(.tbl, !!!.var))
  .resp <- .resp[seq_along(.var) + NCOL(key_dt) - 1]
  names(.resp) <- names(.var)
  
  # Compute features
  out <- map(.resp, function(x){
    res <- imap(features, function(fn, nm){
      fmls <- formals(fn)[-1]
      fn_safe <- safely(fn, tibble(.rows = 1))
      res <- transpose(map(key_dt[[".rows"]], function(i){
        out <- do.call(fn_safe, c(list(x[i]), dots[intersect(names(fmls), names(dots))]))
        if(is.null(names(out[["result"]]))) 
          names(out[["result"]]) <- rep(".?", length(out[["result"]]))
        out
      }))
      err <- compact(res[["error"]])
      tbl <- vctrs::vec_rbind(!!!res[["result"]])
      
      names(tbl)[names(tbl) == ".?"] <- ""
      if(is.character(nm) && nzchar(nm)){
        names(tbl) <- sprintf("%s%s%s", nm, ifelse(nzchar(names(tbl)), "_", ""), names(tbl))
      }
      list(error = err, result = tbl)
    })
    res <- transpose(res)
    res[["result"]] <- invoke(bind_cols, res[["result"]])
    res
  })
  out <- transpose(out)
  
  # Report errors
  err <- flatten(unname(out$error))
  imap(err, function(err, nm){
    err <- compact(err)
    if((tot_err <- length(err)) > 0){
      err_msg <- table(map_chr(err, function(x) x[["message"]]))
      warn(
        sprintf("%i error%s encountered for feature %s\n%s\n",
                tot_err,
                if(tot_err > 1) sprintf("s (%i unique)", length(err_msg)) else "", 
                nm,
                paste0("[", err_msg, "] ", names(err_msg), collapse = "\n")
        )
      )
    }
  })
  
  out <- out[["result"]]
  
  if(!is.null(names(out))){
    out <- imap(out, function(tbl, nm){
      set_names(tbl, sprintf("%s_%s", nm, colnames(tbl)))
    })
  }
  
  bind_cols(
    key_dt[-NCOL(key_dt)],
    !!!out
  )
}

#' Extract features from a dataset
#'
#' Create scalar valued summary features for a dataset from feature functions.
#' 
#' Lists of available features can be found in the following pages:
#' - [Features by package][features_by_pkg]
#' - [Features by tag][features_by_tag]
#' 
#' @param .tbl A dataset
#' @param .var,.vars The variable(s) to compute features on
#' @param features A list of functions (or lambda expressions) for the features to compute. [`feature_set()`] is a useful helper for building sets of features.
#' @param .predicate A predicate function (or lambda expression) to be applied to the columns or a logical vector. The variables for which .predicate is or returns TRUE are selected.
#' @param ... Additional arguments to be passed to each feature. These arguments will only be passed to features which use it in their formal arguments ([`base::formals()`]), and not via their `...`. While passing `na.rm = TRUE` to [`stats::var()`] will work, it will not for [`base::mean()`] as its formals are `x` and `...`. To more precisely pass inputs to each function, you can use lambdas in the list of features (`~ mean(., na.rm = TRUE)`).
#'
#' @seealso [`feature_set()`]
#'
#' @examples 
#' # Provide a set of functions as a named list to features.
#' library(tsibble)
#' tourism %>% 
#'   features(Trips, features = list(mean = mean, sd = sd))
#'
#' # Search and use useful features with `feature_set()`. 
#' if(requireNamespace("feasts")) library(feasts)
#' tourism %>% 
#'   features(Trips, features = feature_set(tags = "autocorrelation"))
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
  .vars <- syms(tidyselect::vars_select(names(.tbl), !!!.vars))
  features_impl(.tbl, syms(.vars), features = features, ...)
}

#' @rdname features
#' @export
features_all <- function(.tbl, features, ...){
  UseMethod("features_all")
}

#' @export
features_all.tbl_ts <- function(.tbl, features = list(), ...){
  .vars <- measured_vars(.tbl)
  features_impl(.tbl, set_names(syms(.vars), .vars), features = features, ...)
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
  features_impl(.tbl, set_names(syms(.vars), .vars), features = features, ...)
}

# Lookup table for features
feature_table <- function() {
  table <- new.env(parent = emptyenv())
  list(
    add = function(fn, fn_name, tags) {
      pkg <- environmentName(environment(fn))
      table[[pkg]] <- as.list(table[[pkg]])
      table[[pkg]][[fn_name]] <- list(fn = fn, tags = tags)
    },
    get = function(pkg) {
      if(is.null(pkg)){
        as.list(table)
      }
      else{
        as.list(table)[pkg]
      }
    },
    list = function() {
      map(table, function(x) map(x, `[[`, "tags"))
    }
  )
}

feature_table <- feature_table()

#' Register a feature function
#' 
#' Allows users to find and use features from your package using [`feature_set()`].
#' If the features are being registered from within a package, this feature
#' registration should happen at load time using `[.onLoad()]`.
#' 
#' @param fn The feature function
#' @param tags Identifying tags
#' 
#' @examples 
#' 
#' \dontrun{
#' tukey_five <- function(x){
#'   setNames(fivenum(x), c("min", "hinge_lwr", "med", "hinge_upr", "max"))
#' }
#' 
#' register_feature(tukey_five, tags = c("boxplot", "simple"))
#' 
#' }
#' 
#' @export
register_feature <- function(fn, tags){
  nm <- enexpr(fn)
  nm <- if(is_call(nm)) call_name(fn) else as_string(nm)
  feature_table$add(fn, nm, tags)
}

#' Create a feature set from tags
#' 
#' Construct a feature set from features available in currently loaded packages.
#' Lists of available features can be found in the following pages:
#' - [Features by package][features_by_pkg]
#' - [Features by tag][features_by_tag]
#' 
#' @param pkgs The package(s) from which to search for features. If `NULL`, 
#' all registered features from currently loaded packages will be searched.
#' @param tags Tags used to identify similar groups of features. If `NULL`,
#' all tags will be included.
#' 
#' @section Registering features:
#' Features can be registered for use with the `feature_set()` function using
#' [`register_feature()`]. This function allows you to register a feature along
#' with the tags associated with it. If the features are being registered from
#' within a package, this feature registration should happen at load time using
#' `[.onLoad()]`.
#' 
#' @export
feature_set <- function(pkgs = NULL, tags = NULL){
  f_set <- flatten(unname(feature_table$get(pkgs)))
  if(!is.null(tags)){
    f_set <- f_set[map_lgl(f_set, function(x) any(x[["tags"]] %in% tags))]
  }
  unname(map(f_set, `[[`, "fn"))
}

rd_features_pkg <- function(){
  features <- map(feature_table$list(), names)
  
  if (length(features) == 0) {
    return("No features found in currently loaded packages.")
  }
  
  feature_links <- paste0(
    map2_chr(features, names(features), function(fns, pkg) {
      sprintf(
        "\\subsection{%s}{\n\\itemize{\n%s\n}\n}", pkg, 
        paste0(
          map_chr(fns, function(fn){
            sprintf("\\item \\code{\\link[%s]{%s}}", pkg, fn)
          }),
          collapse = "\n"
        )
      )
    }),
    collapse = "\n"
  )
  
  sprintf(
"See the following help topics for more details about currently available features:\n%s",
feature_links
  )
}

rd_features_tag <- function(){
  features <- imap(feature_table$list(), function(fns, pkg){
    fns <- set_names(
      unlist(fns, use.names = FALSE), 
      rep(names(fns), map_dbl(fns, length))
    )
    set_names(fns, sprintf("\\item \\code{\\link[%s]{%s}}", pkg, names(fns)))
  })
  
  if (length(features) == 0) {
    return("No features found in currently loaded packages.")
  }
  
  features <- invoke(c, unname(features))
  features <- split(names(features), features)
  
  feature_links <- paste0(
    map2_chr(features, names(features), function(fns, tag) {
      sprintf(
        "\\subsection{%s}{\n\\itemize{\n%s\n}\n}",
        tag, paste0(fns, collapse = "\n")
      )
    }),
    collapse = "\n"
  )
  
  sprintf(
    "See the following help topics for more details about each feature:\n%s",
    feature_links
  )
}

#' Features by package
#' 
#' This documentation lists all available in currently loaded packages. This is
#' a useful reference for making a [`feature_set()`] from particular package(s).
#' 
#' \Sexpr[stage=render,results=rd]{fabletools:::rd_features_pkg()}
#' 
#' @seealso [features_by_tag]
#' 
#' @name features_by_pkg
NULL

#' Features by tag
#' 
#' This documentation lists all available in currently loaded packages. This is
#' a useful reference for making a [`feature_set()`] from particular tag(s).
#' 
#' \Sexpr[stage=render,results=rd]{fabletools:::rd_features_tag()}
#' 
#' @seealso [features_by_pkg]
#' 
#' @name features_by_tag
NULL