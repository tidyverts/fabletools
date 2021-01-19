#' Box Cox Transformation
#'
#' `box_cox()` returns a transformation of the input variable using a Box-Cox
#' transformation. `inv_box_cox()` reverses the transformation.
#'
#' The Box-Cox transformation is given by \deqn{f_\lambda(x) =\frac{x^\lambda -
#' 1}{\lambda}}{f(x;lambda)=(x^lambda - 1)/lambda} if \eqn{\lambda\ne0}{lambda
#' is not equal to 0}. For \eqn{\lambda=0}{lambda=0},
#' \deqn{f_0(x)=\log(x)}{f(x;0)=log(x)}.
#'
#' @param x a numeric vector.
#' @param lambda a numeric value for the transformation parameter.
#' @return a transformed numeric vector of the same length as x.
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#' 
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#' transformations. \emph{JRSS B} \bold{26} 211--246.
#' 
#' @examples
#' library(tsibble)
#' library(dplyr)
#' airmiles %>% 
#'   as_tsibble() %>% 
#'   mutate(box_cox = box_cox(value, lambda = 0.3))
#'
#' @export
box_cox <- function(x, lambda) {
  lambda <- vec_recycle(lambda, vec_size(x))
  x[lambda < 0 & x < 0] <- NA
  lambda_0 <- lambda == 0
  x[lambda_0] <- log(x[lambda_0])
  x[!lambda_0] <- (sign(x[!lambda_0]) * abs(x[!lambda_0]) ^ lambda[!lambda_0] - 1) / lambda[!lambda_0]
  x
}

#' @rdname box_cox
#' @export
inv_box_cox <- function(x, lambda) {
  lambda <- vec_recycle(lambda, vec_size(x))
  x[lambda < 0 & (x > -1 / lambda)] <- NA
  lambda_0 <- lambda == 0
  x[lambda_0] <- exp(x[lambda_0])
  z <- x[!lambda_0] * lambda[!lambda_0] + 1
  x[!lambda_0] <- sign(z) * abs(z) ^ (1 / lambda[!lambda_0])
  x
}