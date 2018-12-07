#' Create a forecast distribution object
#'  
#' @param f A distribution function producing quantiles (such as `qnorm`)
#' @param ... Arguments for `f` function
#' @param transformation Transformation to be applied to resulting quantiles from `f`
#' @param abbr Abbreviation for display purposes, defaults to the object name of `f`
#' 
#' @examples 
#' mydist <- new_fcdist(qnorm, transformation = exp,
#'  mean = rep(3, 10), sd = seq(0, 1, length.out=10))
#' mydist
#' hilo(mydist, 95)
#' @export
new_fcdist <- function(f, ..., transformation = identity,
                       format_fn = format_dist(deparse(substitute(f)))){
  pmap(dots_list(...), list) %>%
    set_names(NULL) %>% 
    enclass("fcdist",
            f = f,
            t = transformation,
            format = format_fn,
            trans = !is.name(body(transformation)))
}

`transformation<-` <- function(x, value){
  attr(x, "t") <- value
  attr(x, "trans") = !is.name(body(value))
  x
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
        if(nchar(nm)){
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
  out <- (x%@%"format")(x, ...)
  
  # Add transformation indicator t()
  if(attr(x, "trans")){
    out <- paste0("t(", out, ")")
  }
  
  out
}

#' @export
`[.fcdist` <- function(x, ...){
  enclass(NextMethod(), "fcdist", 
          !!!attributes(x))
}

#' @export
c.fcdist <- function(...){
  sameAttr <- dots_list(...) %>%
    map(function(.x) if(!inherits(.x, "fcdist")) {abort("Only combining fcdist objects is supported")} else {attributes(.x)}) %>%
    duplicated %>%
    .[-1]
  if(any(!sameAttr)){
    abort("Cannot combine fcdist objects of different types.")
  }
    
  enclass(NextMethod(), "fcdist", 
          !!!attributes(..1))
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
  args <- merge_pos_list(!!!as_list(x))
  list(lower = 50-level/2, upper = 50+level/2) %>%
    map(function(level){
      attr(x,"t")(do.call(attr(x, "f"), c(list(level/100), as.list(args))))
    }) %>%
    append(list(level = level)) %>%
    invoke("new_hilo", .)
}

#' @importFrom stats quantile
#' @export
quantile.fcdist <- function(x, probs = seq(0, 1, 0.25), ...){
  args <- merge_pos_list(!!!as_list(x))
  map(probs, function(prob){
    attr(x,"t")(do.call(attr(x, "f"), c(list(prob), as.list(args))))
  })
}

qsample <- function(p, x = list(), ...){
  map_dbl(x, function(x) as.numeric(stats::quantile(x, p, ...)))
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

#' Distributions for intervals
#' 
#' @param mean vector of distributional means.
#' @param sd vector of distributional standard deviations.
#' @param ... Additional arguments passed on to quantile methods.
#' 
#' @rdname distributions
#' 
#' @example 
#' dist_normal(rep(3, 10), seq(0, 1, length.out=10))
#' dist_sim(list(rnorm(100), rnorm(100), rnorm(100)))
#' 
#' @export
dist_normal <- function(mean, sd, ...){
  new_fcdist(stats::qnorm, mean = mean, sd = sd, ..., format_fn = format_dist_normal)
}

#' @rdname distributions
#' 
#' @param sample a list of simulated values
#' 
#' @export
dist_sim <- function(sample, ...){
  new_fcdist(qsample, x = map(sample, list), ..., format_fn = format_dist("sim"))
}