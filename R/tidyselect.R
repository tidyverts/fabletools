all_keys <- function() {
  key_vars(tidyselect::peek_data())
}

all_measured <- function() {
  .data <- tidyselect::peek_data()
  setdiff(names(.data), c(key_vars(.data), index_var(.data)))
}