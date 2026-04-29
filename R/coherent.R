#' Structural (summing) matrix for coherent time series
#'
#' Constructs the \eqn{n \times n_b} structural matrix \eqn{\mathbf{S}} for a
#' set of linearly related time series, where \eqn{n} is the total number
#' of series and \eqn{n_b} is the number of bottom-level series.
#'
#' Let \eqn{\boldsymbol{b}_t} be the \eqn{n_b}-vector of bottom-level series at
#' time \eqn{t}, and \eqn{\boldsymbol{a}_t = \mathbf{A}\boldsymbol{b}_t} be the
#' \eqn{n_a = n - n_b} aggregated series, where \eqn{\mathbf{A}} is the
#' \eqn{n_a \times n_b} aggregation matrix. The full \eqn{n}-vector of series is
#' \eqn{\boldsymbol{y}_t = [\boldsymbol{a}_t', \boldsymbol{b}_t']'}, and the
#' structural matrix satisfies
#'
#' \deqn{\boldsymbol{y}_t = \mathbf{S}\boldsymbol{b}_t, \quad
#'   \mathbf{S} = \begin{bmatrix} \mathbf{A} \\ \mathbf{I}_{n_b} \end{bmatrix}.}
#'
#' Any linear reconciliation method can be written as
#' \eqn{\tilde{\boldsymbol{y}}_h = \mathbf{S}\mathbf{G}\hat{\boldsymbol{y}}_h},
#' where \eqn{\hat{\boldsymbol{y}}_h} are the \eqn{h}-step base forecasts.
#' Optimal reconciled forecasts use
#' \eqn{\mathbf{G} = (\mathbf{S}'\mathbf{W}^{-1}\mathbf{S})^{-1}\mathbf{S}'\mathbf{W}^{-1}},
#' with \eqn{\mathbf{W}} a positive definite weight matrix (see
#' [`min_trace()`]).
#'
#' @param data A data object which contains linearly related coherent structures.
#' @param sparse If `TRUE`, a sparse matrix (class `"dgCMatrix"` from the
#'   \pkg{Matrix} package) is returned. Defaults to `FALSE`.
#'
#' @return An \eqn{n \times n_b} matrix (dense or sparse) encoding the
#'   structural relationships among all series.
#'
#' @seealso [coherent_cmat()] for the corresponding zero-constraint matrix
#'   \eqn{\mathbf{C}} and [aggregate_key()] for computing cross-sectional
#'   aggregations with tsibble data sets.
#'
#' @references
#' Hyndman, R. J., & Athanasopoulos, G. (2022). *Notation for forecast
#' reconciliation*. <https://robjhyndman.com/hyndsight/reconciliation-notation.html>
#'
#' @examples
#' tsibble::tourism %>% 
#'   aggregate_key(Purpose, Trips = sum(Trips)) %>%
#'   coherent_smat()
#' 
#' @export
coherent_smat <- function(data, sparse = FALSE) {
  UseMethod("coherent_smat")
}

#' @export
coherent_smat.tbl <- function(data, sparse = FALSE) {
  if (names(data)[[ncol(data)]] != ".rows") {
    tryCatch(
      data <- key_data(data),
      error = function(e) {
        cli::cli_abort(
          "The data must be a `key_data` object, or an object with a suitable `key_data()` method.",
          call = NULL
        )
      }
    )
  }

  spos <- build_key_data_smat(data)

  if (sparse) {
    row_btm <- spos$leaf
    row_agg <- seq_len(nrow(data))[-row_btm]
    Matrix::sparseMatrix(
      i = rep(seq_along(spos$agg), lengths(spos$agg)),
      j = unlist(spos$agg),
      x = rep(1, sum(lengths(spos$agg)))
    )
  } else {
    out <- matrix(0L, nrow = nrow(data), ncol = length(spos$leaf))
    out[nrow(data)*(unlist(spos$agg)-1) + rep(seq_along(spos$agg), lengths(spos$agg))] <- 1L
    out
  }
}

#' @export
coherent_smat.default <- function(data, sparse = FALSE) {
  coherent_smat(key_data(data), sparse = sparse)
}

#' Zero-constraint matrix for coherent time series
#'
#' Constructs the \eqn{n_a \times n} zero-constraint matrix \eqn{\mathbf{C}}
#' for a set of linearly related time series, where \eqn{n_a = n - n_b}
#' is the number of aggregated series.
#'
#' Given the aggregation matrix \eqn{\mathbf{A}} (see [`coherent_smat()`]), the
#' constraint matrix is defined as
#'
#' \deqn{\mathbf{C} = [\mathbf{I}_{n_a} \;\; {-\mathbf{A}}],}
#'
#' so that \eqn{\mathbf{C}\boldsymbol{y}_t = \boldsymbol{0}_{n_a}} for all
#' coherent vectors \eqn{\boldsymbol{y}_t}. This zero-constrained representation
#' yields the mapping matrix
#'
#' \deqn{\mathbf{M} = \mathbf{I}_n -
#'   \mathbf{W}\mathbf{C}'(\mathbf{C}\mathbf{W}\mathbf{C}')^{-1}\mathbf{C},}
#'
#' which requires inverting an \eqn{n_a \times n_a} matrix rather than the
#' \eqn{n_b \times n_b} matrix in the structural form, and is therefore more
#' efficient when \eqn{n_a < n_b}.
#'
#' @inheritParams coherent_smat
#'
#' @return An \eqn{n_a \times n} matrix (dense or sparse) whose rows encode
#'   each aggregation constraint as \eqn{\mathbf{C}\boldsymbol{y}_t =
#'   \boldsymbol{0}_{n_a}}.
#'
#' @seealso [coherent_smat()] for the corresponding structural matrix
#'   \eqn{\mathbf{S}} and [aggregate_key()] for computing cross-sectional
#'   aggregations with tsibble data sets.
#' 
#' @references
#' Hyndman, R. J., & Athanasopoulos, G. (2022). *Notation for forecast
#' reconciliation*. <https://robjhyndman.com/hyndsight/reconciliation-notation.html>
#'
#' Di Fonzo, T., & Girolimetto, D. (2021). Cross-temporal forecast
#' reconciliation: Optimal combination method and heuristic alternatives.
#' *International Journal of Forecasting*. \doi{10.1016/j.ijforecast.2021.08.004}
#'
#' @export
coherent_cmat <- function(data, sparse = FALSE) {
  UseMethod("coherent_cmat")
}

#' @export
coherent_cmat.default <- function(data, sparse = FALSE) {
  S <- coherent_smat(data, sparse = sparse)
  row_btm <- apply(S, 1, sum) == 1L
  row_agg <- which(!row_btm)
  row_btm <- which(row_btm)
  if (sparse) diag <- Matrix::Diagonal

  U <- cbind(
    diag(diff(rev(dim(S)))),
    -S[row_agg,,drop = FALSE]
  )
  U[, order(c(row_agg, row_btm)), drop = FALSE]
}