#' Create a forecast distribution object
#'  
#' @param ... Arguments for `f` function
#' @param .env An environment produced using `new_fcdist_env`
#' 
#' @examples 
#' tnorm <- new_fcdist_env(qnorm, exp)
#' mydist <- new_fcdist(qnorm, transformation = exp,
#'  mean = rep(3, 10), sd = seq(0, 1, length.out=10))
#' mydist
#' hilo(mydist, 95)
#' 
#' @rdname fcdist
#' @export
new_fcdist <- function(..., .env){
  pmap(dots_list(...), list, .env = .env) %>%
    structure(class = "fcdist")
}

#' @param quantile A distribution function producing quantiles (such as `qnorm`)
#' @param transformation Transformation to be applied to resulting quantiles 
#' from `quantile`
#' @param display Function that is used to format the distribution display
#' 
#' @rdname fcdist
#' @export
new_fcdist_env <- function(quantile, transformation = identity, display = NULL){
  if(is.null(display)){
    display <- format_dist(as_string(enexpr(quantile)))
  }
  new_environment(
    list(f = quantile, t = transformation, format = display,
         trans = !is.name(body(transformation)))
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
      env$trans <- !is.name(body(transformation))
    }
    if(!is.null(format_fn)){
      env$format_fn <- format_fn
    }
    map(dist, function(x){x[[length(x)]] <- env; x})
  })
  structure(unsplit(x, .env_ids), class = "fcdist")
}

#' @export
Ops.fcdist <- function(e1, e2){
  ok <- switch(.Generic, `+` = , `-` = , `*` = , `/` = TRUE, FALSE)
  if (!ok) {
    warn(sprintf("`%s` not meaningful for distributions", .Generic))
    return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
  }
  if(.Generic %in% c("-", "+") && missing(e2)){
    e2 <- e1
    .Generic <- "*"
    e1 <- 1
  }
  if(inherits(e1, "fcdist") && inherits(e2, "fcdist")){
    warn("Combinations of forecast distributions is not yet supported.")
    return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
  }
  e_len <- c(length(e1), length(e2))
  if(max(e_len) %% min(e_len) != 0){
    warn("longer object length is not a multiple of shorter object length")
  }
  if(e_len[[1]] != e_len[[2]]){
    if(which.min(e_len) == 1){
      e1 <- rep_len(e1, e_len[[2]])
    }
    else{
      e2 <- rep_len(e2, e_len[[1]])
    }
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
    return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
  }
  if(.Generic == "/" && inherits(e2, "fcdist")){
    warn(sprintf("Cannot divide by a distribution"))
    return(rep.int(NA, max(length(e1), if (!missing(e2)) length(e2))))
  }
  
  if(.Generic == "-"){
    .Generic <- "+"
    scalar <- -scalar
  }
  else if(.Generic == "/"){
    .Generic <- "*"
    scalar <- 1/scalar
  }
  
  .env_ids <- map_chr(dist, function(x) env_label(x[[length(x)]]))
  dist <- map2(split(dist, .env_ids), split(scalar, .env_ids), function(x, y){
    if(!identical(x[[1]]$.env$f, qnorm) || x[[1]]$.env$trans){
      warn("Cannot perform calculations with this non-normal distributions")
      return(rep.int(NA, length(x)))
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
  structure(unsplit(dist, .env_ids), class = "fcdist")
}

#' @export
type_sum.fcdist <- function(x){
  "dist"
}

#' @export
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

format_dist <- function(fn_nm){
  function(x, ...){
    out <- transpose(x) %>% 
      imap(function(arg, nm){
        arg <- unlist(arg, recursive = FALSE)
        if(!is_list(arg)){
          out <- format(arg, digits = 2, ...)
        }
        else{
          out <- sprintf("%s[%i]", map_chr(arg, type_sum), map_int(arg, length))
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
  .env_ids <- map_chr(x, function(x) env_label(x[[length(x)]]))
  split(x, .env_ids) %>% 
    set_names(NULL) %>% 
    map(function(x){
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
  structure(NextMethod(), class = "fcdist")
}

#' @export
c.fcdist <- function(...){
  structure(NextMethod(), class = "fcdist")
}

#' @export
length.fcdist <- function(x){
  NextMethod()
}

# #' @importFrom ggplot2 aes_
# autoplot.fcdist <- function(q_fn, q_range = c(0.0001, 0.9999), precision = 0.01){
#   tibble(x = seq(q_range[1], q_range[2], by = precision)) %>%
#     mutate(!!"density":=q_fn(!!sym("x"))) %>%
#     ggplot(aes_(x=~x, y=~density)) + 
#     geom_line()
# }
#' @export
hilo.fcdist <- function(x, level = 95, ...){
  if(length(level)!=1){
    abort("Only one value of 'level' is supported.")
  }
  if (level < 0 || level > 100) {
    abort("'level' can't be negative or greater than 100.")
  }
  
  .env_ids <- map_chr(transpose(x)$.env, env_label)
  split(x, .env_ids) %>% 
    set_names(NULL) %>% 
    map(hilo_fcdist, level = level) %>% 
    unsplit(.env_ids)
}

hilo_fcdist <- function(level, x){
  env <- x[[1]][[length(x[[1]])]]
  args <- transpose(x)[-length(x[[1]])] %>% 
    map(unlist, recursive = FALSE)
  list(lower = 50-level/2, upper = 50+level/2) %>%
    map(function(level){
      env$t(do.call(env$f, c(list(level/100), as.list(args))))
    }) %>%
    append(list(level = level)) %>%
    invoke("new_hilo", .)
}

#' @export
quantile.fcdist <- function(x, probs = seq(0, 1, 0.25), ...){
  args <- merge_pos_list(!!!as_list(x))
  map(probs, function(prob){
    attr(x,"t")(do.call(attr(x, "f"), c(list(prob), as.list(args))))
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

env_dist_normal <- new_fcdist_env(qnorm, display = format_dist_normal)

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
#' dist_sim(list(rnorm(100), rnorm(100), rnorm(100)))
#' 
#' @export
dist_normal <- function(mean, sd, ...){
  new_fcdist(mean = mean, sd = sd, ..., .env = env_dist_normal)
}

#' @importFrom stats quantile
qsample <- function(p, x = list(), ...){
  map_dbl(x, function(x) as.numeric(stats::quantile(x, p, ...)))
}

env_dist_sim <- new_fcdist_env(qsample, display = format_dist("sim"))

#' @rdname distributions
#' 
#' @param sample a list of simulated values
#' 
#' @export
dist_sim <- function(sample, ...){
  new_fcdist(map(sample, list), ..., .env = env_dist_sim)
}