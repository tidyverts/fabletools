#' Multiple calls to a univariate model for each tsibble key
#' 
#' @inheritParams multi_model
#' 
#' @export
multi_univariate <- function(data, cl, env = caller_env(n=2)){
  multi_model(data, cl, key(data), env = env)
}

#' Multiple calls to a model for mass modelling
#' 
#' @param data A tsibble
#' @param cl A modelling call
#' @param keys A set of keys to nest over
#' @param env The environment to estimate batch models in
#' 
#' @importFrom dplyr bind_cols
#' 
#' @export
multi_model <- function(data, cl, keys, env = caller_env(n=2)){
  # Re-evaluate cl in environment with split data
  split_data <- split(data, group_indices(group_by(data, !!!keys)))
  map(split_data, function(x){
    eval_tidy(
      get_expr(cl),
      env = child_env(env, !!expr_text(get_expr(cl)$data) := x)
    )
  }) %>% 
    invoke("rbind", .)
}