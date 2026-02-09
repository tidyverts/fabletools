# nocov start
.onLoad <- function(...) {
  register_s3_method("pillar", "type_sum", "mdl_ts")
  register_s3_method("pillar", "type_sum", "lst_mdl")
  register_s3_method("pillar", "type_sum", "fbl_ts")
  
  register_s3_method("pillar", "pillar_shaft", "agg_vec")
  
  register_s3_method("tibble", "tbl_sum", "dcmp_ts")
  register_s3_method("tibble", "tbl_sum", "mdl_df")
  register_s3_method("tibble", "tbl_sum", "fbl_ts")
  
  register_s3_method("ggplot2", "scale_type", "agg_vec")
  
  # fabletools -> ggtime method migration
  ggtime_version <- package_version(as.character(
    suppressWarnings(
      utils::packageDescription("ggtime", fields = "Version")
    )
  ))
  if (!is.na(ggtime_version) && isFALSE(as.logical(Sys.getenv("R_PACKAGE_BUILDING")))) {
    ggtime_ns <- getNamespace("ggtime")
    
    register_s3_method("ggplot2", "autoplot", "tbl_ts", utils::getS3method("autoplot", "tbl_ts", envir = ggtime_ns))
    register_s3_method("ggplot2", "autolayer", "tbl_ts", utils::getS3method("autolayer", "tbl_ts", envir = ggtime_ns))
    register_s3_method("ggplot2", "autoplot", "dcmp_ts", utils::getS3method("autoplot", "dcmp_ts", envir = ggtime_ns))
    if (ggtime_version >= "0.2.0") {
      register_s3_method("ggplot2", "autoplot", "fbl_ts", utils::getS3method("autoplot", "fbl_ts", envir = ggtime_ns))
      register_s3_method("ggplot2", "autolayer", "fbl_ts", utils::getS3method("autolayer", "fbl_ts", envir = ggtime_ns))
      register_s3_method("ggplot2", "fortify", "tbl_ts", utils::getS3method("fortify", "fbl_ts", envir = ggtime_ns))
    } else {
      register_s3_method("ggplot2", "autoplot", "fbl_ts", autoplot.fbl_ts)
      register_s3_method("ggplot2", "autolayer", "fbl_ts", autolayer.fbl_ts)
      register_s3_method("ggplot2", "fortify", "tbl_ts", fortify.fbl_ts)
    }
  } else {
    register_s3_method("ggplot2", "autoplot", "fbl_ts", autoplot.fbl_ts)
    register_s3_method("ggplot2", "autoplot", "tbl_ts", autoplot.tbl_ts)
    register_s3_method("ggplot2", "autoplot", "dcmp_ts", autoplot.dcmp_ts)
    register_s3_method("ggplot2", "autolayer", "fbl_ts", autolayer.fbl_ts)
    register_s3_method("ggplot2", "autolayer", "tbl_ts", autolayer.fbl_ts)
    register_s3_method("ggplot2", "fortify", "tbl_ts", fortify.fbl_ts)
  }


  op <- options()
  op.fable <- list(
    fable.show_progress = TRUE
  )
  toset <- !(names(op.fable) %in% names(op))
  if (any(toset)) options(op.fable[toset])
  
  invisible()
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