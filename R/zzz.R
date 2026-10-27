# nocov start
.onLoad <- function(...) {
  register_s3_method("pillar", "type_sum", "mdl_ts")
  register_s3_method("pillar", "type_sum", "mdl_lst")
  register_s3_method("pillar", "type_sum", "fbl_df")
  register_s3_method("pillar", "type_sum", "mdl_df")

  register_s3_method("pillar", "pillar_shaft", "agg_vec")

  register_s3_method("tibble", "tbl_sum", "dcmp_ts")
  register_s3_method("tibble", "tbl_sum", "mbl_df")
  register_s3_method("tibble", "tbl_sum", "mdl_df")
  register_s3_method("tibble", "tbl_sum", "fbl_df")
  
  register_s3_method("ggplot2", "scale_type", "agg_vec")
  
  op <- options()
  op.fable <- list(
    fable.show_progress = TRUE
  )
  toset <- !(names(op.fable) %in% names(op))
  if (any(toset)) options(op.fable[toset])
  
  invisible()
}

.onAttach <- function(...) {
  # fabletools -> ggtime method migration
  ggtime_version <- as.character(suppressWarnings(
    utils::packageDescription("ggtime", fields = "Version")
  ))
  register_plot_methods(ggtime_version)
}

# Register ggtime's plot methods if installed, otherwise fabletools' own.
# Fable methods are registered for both the `fbl_df` and legacy `fbl_ts` classes.
register_plot_methods <- function(ggtime_version) {
  register_fable_method <- function(generic, fun) {
    for (cls in c("fbl_df", "fbl_ts")) register_s3_method("ggplot2", generic, cls, fun)
  }
  if (!is.na(ggtime_version)) {
    ggtime_ns <- getNamespace("ggtime")
    ggtime_method <- function(generic, class) utils::getS3method(generic, class, envir = ggtime_ns)
    register_s3_method("ggplot2", "autoplot", "tbl_ts", ggtime_method("autoplot", "tbl_ts"))
    register_s3_method("ggplot2", "autolayer", "tbl_ts", ggtime_method("autolayer", "tbl_ts"))
    register_s3_method("ggplot2", "autoplot", "dcmp_ts", ggtime_method("autoplot", "dcmp_ts"))
    if (package_version(ggtime_version) >= "0.2.0") {
      # ggtime's `fbl_df` methods are used if available
      for (generic in c("autoplot", "autolayer", "fortify")) {
        fun <- utils::getS3method(generic, "fbl_df", envir = ggtime_ns, optional = TRUE) %||%
          ggtime_method(generic, "fbl_ts")
        register_fable_method(generic, fun)
      }
    } else {
      register_fable_method("autoplot", autoplot.fbl_ts)
      register_fable_method("autolayer", autolayer.fbl_ts)
      register_fable_method("fortify", fortify.fbl_ts)
    }
  } else {
    register_fable_method("autoplot", autoplot.fbl_ts)
    register_s3_method("ggplot2", "autoplot", "tbl_ts", autoplot.tbl_ts)
    register_s3_method("ggplot2", "autoplot", "dcmp_ts", autoplot.dcmp_ts)
    register_fable_method("autolayer", autolayer.fbl_ts)
    register_s3_method("ggplot2", "autolayer", "tbl_ts", autolayer.tbl_ts)
    register_fable_method("fortify", fortify.fbl_ts)
  }
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end