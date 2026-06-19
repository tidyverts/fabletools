# Wrap a model with extra classes and store `...` as attributes.
# Building block for bootstrap_iid()/bootstrap_block()/simulate_iid().
mdl_wrap <- function(x, .class, ...) {
  x <- structure(x, class = c(.class, class(x)))
  attrs <- list(...)
  for (nm in names(attrs)) attr(x, nm) <- attrs[[nm]]
  x
}
