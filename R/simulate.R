#' Generate responses from a model
#' 
#' Use a model's fitted distribution to simulate additional data with similar
#' behaviour to the response. This is a tidy implementation of 
#' `\link[stats]{simulate}`.
#' 
#' @param object A model
#' @param ... Additional optional arguments
#' 
#' @examples
#' library(dplyr)
#' library(fable)
#' UKLungDeaths <- as_tsibble(cbind(mdeaths, fdeaths), pivot_longer = FALSE)
#' UKLungDeaths %>% 
#'   model(lm = TSLM(mdeaths ~ fourier("year", K = 4) + fdeaths)) %>% 
#'   generate(UKLungDeaths, times = 5)
#' 
#' @rdname generate
#'   
#' @export
generate <- function(object, ...){
  UseMethod("generate")
}

#' @param new_data The data to be generated (time index and exogenous regressors)
#' @rdname generate
#' @export
generate.mdl_df <- function(object, new_data = NULL, ...){
  kv <- c(key_vars(object), ".model")
  mdls <- object%@%"models"
  if(!is.null(new_data)){
    object <- bind_new_data(object, new_data)
  }
  object <- gather(object, ".model", ".fit", !!!syms(mdls))
  
  # Evaluate simulations
  object$.sim <- map2(object[[".fit"]], 
                      object[["new_data"]] %||% rep(list(NULL), length.out = NROW(object)),
                      generate, ...)
  unnest(add_class(object, "lst_ts"), !!sym(".sim"), key = kv)
}

#' @param h The simulation horizon (can be used instead of `new_data` for regular
#' time series with no exogenous regressors).
#' @param times The number of replications
#' @param seed The seed for the random generation from distributions
#' 
#' @rdname generate
#' @export
generate.model <- function(object, new_data = NULL, h = NULL, times = 1, seed = NULL, ...){
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    stats::runif(1)
  if (is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  
  if(is.null(new_data)){
    new_data <- make_future_data(object$data, h)
  }
  
  if(is.null(new_data[[".rep"]])){
    new_data <- map(seq_len(times), function(rep){
      new_data[[".rep"]] <- rep
      update_tsibble(new_data, key = c(".rep", key_vars(new_data)),
                     validate = FALSE)
    }) %>% 
      invoke("rbind", .)
  }
  
  .sim <- generate(object[["fit"]], new_data = new_data, ...)
  if(length(object$transformation) > 1) abort("Imitating multivariate models is not yet supported")
  .sim[[".sim"]] <- invert_transformation(object$transformation[[1]])(.sim[[".sim"]])
  .sim
}