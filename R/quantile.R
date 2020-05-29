#' Create a forecast distribution object
#'  
#' @param ... Arguments for `f` function
#' @param .env An environment produced using `new_fcdist_env`
#' 
#' @rdname fcdist
#' @export
new_fcdist <- function(..., .env){
  structure(
    pmap(dots_list(...), list, .env = .env),
    class = c("fcdist", "list")
  )
}

#' @param quantile A distribution function producing quantiles (such as `qnorm`)
#' @param transformation Transformation to be applied to resulting quantiles 
#' from `quantile`
#' @param display Function that is used to format the distribution display
#' 
#' @rdname fcdist
#' @export
new_fcdist_env <- function(quantile, transformation = list(identity), display = NULL){
  if(is.null(display)){
    display <- format_dist(as_string(enexpr(quantile)))
  }
  new_environment(
    list(f = quantile, t = transformation, format = display,
         trans = any(map_lgl(transformation, compose(`!`, is.name, body))))
  )
}

update_fcdist <- function(x, quantile = NULL, transformation = NULL, format_fn = NULL){
  .env_ids <- map_chr(x, function(x) env_label(x[[length(x)]]))
  x <- map(split(x, .env_ids), function(dist){
    env <- env_clone(dist[[1]][[length(dist[[1]])]])
    if(!is.null(quantile)){
      env$f <- quantile
    }
    if(!is.null(transformation)){
      env$t <- transformation
      env$trans <- any(map_lgl(transformation, compose(`!`, is.name, body)))
    }
    if(!is.null(format_fn)){
      env$format_fn <- format_fn
    }
    map(dist, function(x){x[[length(x)]] <- env; x})
  })
  structure(unsplit(x, .env_ids), class = c("fcdist", "list"))
}

#' @importFrom stats qnorm
#' @export
Ops.fcdist <- function(e1, e2){
  ok <- switch(.Generic, `+` = , `-` = , `*` = , `/` = TRUE, FALSE)
  if (!ok) {
    warn(sprintf("`%s` not meaningful for distributions", .Generic))
    return(dist_unknown(max(length(e1), if (!missing(e2)) length(e2))))
  }
  if(.Generic == "/" && inherits(e2, "fcdist")){
    warn(sprintf("Cannot divide by a distribution"))
    return(dist_unknown(max(length(e1), if (!missing(e2)) length(e2))))
  }
  if(.Generic %in% c("-", "+") && missing(e2)){
    e2 <- e1
    e1 <- if(.Generic == "+") 1 else -1
    .Generic <- "*"
  }
  if(.Generic == "-"){
    .Generic <- "+"
    e2 <- -e2
  }
  else if(.Generic == "/"){
    .Generic <- "*"
    e2 <- 1/e2
  }
  e_len <- c(length(e1), length(e2))
  if(max(e_len) %% min(e_len) != 0){
    warn("longer object length is not a multiple of shorter object length")
  }
  if(e_len[[1]] != e_len[[2]]){
    if(which.min(e_len) == 1){
      e1 <- rep(e1, e_len[[2]])
    }
    else{
      e2 <- rep(e2, e_len[[1]])
    }
  }
  
  if(is_dist_unknown(e1) || is_dist_unknown(e2)){
    return(dist_unknown(length(e1)))
  }
  
  if(inherits(e1, "fcdist") && inherits(e2, "fcdist")){
    if(.Generic == "*"){
      warn(sprintf("Multiplying forecast distributions is not supported."))
      return(dist_unknown(max(length(e1), if (!missing(e2)) length(e2))))
    }
    
    grps <- paste(sep = "-",
      map_chr(e1, function(x) env_label(x[[length(x)]])),
      map_chr(e2, function(x) env_label(x[[length(x)]]))
    )
    
    e1 <- map2(split(e1, grps), split(e2, grps), function(x, y){
      if(!is_dist_normal(x) || !is_dist_normal(y)){
        warn("Combinations of non-normal forecast distributions is not supported.")
        return(dist_unknown(max(length(e1), length(e2))))
      }
      x <- transpose(x) %>% map(unlist, recursive = FALSE)
      y <- transpose(y) %>% map(unlist, recursive = FALSE)
      if(.Generic == "+"){
        x$mean <- x$mean + y$mean
        x$sd <- sqrt(x$sd^2 + y$sd^2)
      }
      transpose(x)
    })
    
    return(structure(unsplit(e1, grps), class = c("fcdist", "list")))
  }

  if(inherits(e1, "fcdist")){
    dist <- e1
    scalar <- e2
  } else {
    dist <- e2
    scalar <- e1
  }
  if(!is.numeric(scalar)){
    warn(sprintf("Cannot %s a `%s` with a distribution", switch(.Generic, 
      `+` = "add", `-` = "subtract", `*` = "multiply", `/` = "divide"), class(scalar)))
    return(dist_unknown(length(e1)))
  }
  
  .env_ids <- map_chr(dist, function(x) env_label(x[[length(x)]]))
  dist <- map2(split(dist, .env_ids), split(scalar, .env_ids), function(x, y){
    if(!is_dist_normal(x)){
      warn("Cannot perform calculations with this non-normal distributions")
      return(dist_unknown(length(x)))
    }
    x <- transpose(x) %>% map(unlist, recursive = FALSE)
    if(.Generic == "+"){
      x$mean <- x$mean + y
    }
    else if(.Generic == "*"){
      x$mean <- x$mean * y
      x$sd <- x$sd * y
    }
    transpose(x)
  })
  structure(unsplit(dist, .env_ids), class = c("fcdist", "list"))
}

type_sum.fcdist <- function(x){
  "dist"
}

is_vector_s3.fcdist <- function(x){
  TRUE
}

obj_sum.fcdist <- function(x) {
  rep("dist", length(x))
}

pillar_shaft.fcdist <- function(x, ...){
  pillar::new_pillar_shaft_simple(format(x), align = "left", min_width = 10)
}

#' @export
print.fcdist <- function(x, ...) {
  print(format(x, ...), quote = FALSE)
  invisible(x)
}

# Brief hack for vctrs support. To be replaced by distributional.
#' @importFrom vctrs vec_ptype2
#' @method vec_ptype2 fcdist
#' @export
vec_ptype2.fcdist <- function(x, y, ...) UseMethod("vec_ptype2.fcdist", y)
#' @method vec_ptype2.fcdist default
#' @export
vec_ptype2.fcdist.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.fcdist fcdist
#' @export
vec_ptype2.fcdist.fcdist <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  x
}

#' @importFrom vctrs vec_cast
#' @method vec_cast fcdist
#' @export
vec_cast.fcdist <- function(x, to, ...) UseMethod("vec_cast.fcdist")
#' @method vec_cast.fcdist default
#' @export
vec_cast.fcdist.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)
#' @method vec_cast.fcdist fcdist
#' @export
vec_cast.fcdist.fcdist <- function(x, to, ...) x

format_dist <- function(fn_nm){
  function(x, ...){
    out <- transpose(x) %>% 
      imap(function(arg, nm){
        arg <- unlist(arg, recursive = FALSE)
        if(!is_list(arg)){
          out <- format(arg, digits = 2, ...)
        }
        else{
          out <- sprintf("%s[%i]", map_chr(arg, tibble::type_sum), map_int(arg, length))
        }
        if(is_character(nm)){
          out <- paste0(nm, "=", out)
        }
        out
      }) %>%
      invoke("paste", ., sep = ", ")
    
    # Add dist name q()
    sprintf("%s(%s)", fn_nm, out)
  }
}

#' @export
format.fcdist <- function(x, ...){
  .env_ids <- map_chr(x, function(x) possibly(env_label, ".na")(x[[length(x)]]))
  split(x, .env_ids) %>%
    set_names(NULL) %>% 
    map(function(x){
      if(!is_environment(x[[1]][[length(x[[1]])]])) return("NA")
      out <- x[[1]]$.env$format(map(x, function(x) x[-length(x)]))
      if(x[[1]]$.env$trans){
        out <- paste0("t(", out, ")")
      }
      out
    }) %>% 
    unsplit(.env_ids)
}

#' @export
`[.fcdist` <- function(x, ...){
  structure(NextMethod(), class = c("fcdist", "list"))
}

#' @export
c.fcdist <- function(...){
  structure(NextMethod(), class = c("fcdist", "list"))
}

#' @export
rep.fcdist <- function(x, ...){
  structure(NextMethod(), class = c("fcdist", "list"))
}

#' @export
unique.fcdist <- function(x, ...){
  structure(NextMethod(), class = c("fcdist", "list"))
}

#' @export
length.fcdist <- function(x){
  NextMethod()
}


#' @export
quantile.fcdist <- function(x, probs = seq(0, 1, 0.25), ...){
  .Deprecated("distributional::quantile")
  env <- x[[1]][[length(x[[1]])]]
  args <- transpose(x)[-length(x[[1]])]
  map(probs, function(prob){
    intr <- do.call(env$f, c(list(prob), as.list(args), dots_list(...)))
    if(!is.list(intr)){
      intr <- list(intr)
    }
    map2(env$t, intr, calc)
  })
}

format_dist_normal <- function(x, ...){
  args <- transpose(x) %>% 
    map(unlist)
  
  # Add dist name q()
  sprintf("N(%s, %s)", 
          format(args$mean, digits = 2, ...),
          format(args$sd^2, digits = 2, ...)
  )
}

env_dist_normal <- new_fcdist_env(function(mean, sd, ...){
    qnorm(..., mean = unlist(mean), sd = unlist(sd))
  }, display = format_dist_normal)

#' Distributions for intervals
#' 
#' @param mean vector of distributional means.
#' @param sd vector of distributional standard deviations.
#' @param ... Additional arguments passed on to quantile methods.
#' 
#' @rdname distributions
#' 
#' @examples
#' dist_normal(rep(3, 10), seq(0, 1, length.out=10))
#' 
#' @export
dist_normal <- function(mean, sd, ...){
  new_fcdist(mean = mean, sd = sd, ..., .env = env_dist_normal)
}

env_dist_mv_normal <- new_fcdist_env(function(p, mean, sd, var){
  map(transpose(map2(mean, map(sd, diag), qnorm, p = p)), as.numeric)
  # abort("Multivariate normal intervals are not currently supported.")
}, display = function(x, ...) rep(sprintf("MVN[%i]", length(x[[1]][["mean"]])), length(x)))

#' @rdname distributions
#' @export
dist_mv_normal <- function(mean, sd, ...){
  new_fcdist(mean = mean, sd = sd, ..., .env = env_dist_mv_normal)
}

#' @importFrom stats quantile
qsample <- function(p, x = list(), ...){
  map_dbl(x, function(x) as.numeric(stats::quantile(unlist(x), p, ...)))
}

env_dist_sim <- new_fcdist_env(qsample, display = format_dist("sim"))

#' @rdname distributions
#' 
#' @param sample a list of simulated values
#' 
#' @examples 
#' dist_sim(list(rnorm(100), rnorm(100), rnorm(100)))
#' 
#' @export
dist_sim <- function(sample, ...){
  new_fcdist(map(sample, list), ..., .env = env_dist_sim)
}

env_dist_unknown <- new_fcdist_env(function(x, ...) rep(NA, length(x)),
                                   display = function(x, ...) rep("?", length(x)))

#' @rdname distributions
#' 
#' @param n The number of distributions.
#' 
#' @examples 
#' dist_unknown(10)
#' 
#' @export
dist_unknown <- function(n, ...){
  new_fcdist(vector("double", n), ..., .env = env_dist_unknown)
}


is_dist_normal <- function(dist){
  if(!inherits(dist, "fcdist")) return(FALSE)
  identical(dist[[1]]$.env$f, env_dist_normal$f) && !dist[[1]]$.env$trans
}

is_dist_unknown <- function(dist){
  if(!inherits(dist, "fcdist")) return(FALSE)
  identical(dist[[1]]$.env$f, env_dist_unknown$f) && !dist[[1]]$.env$trans
}