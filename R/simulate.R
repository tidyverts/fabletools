#' Imitate responses from a model
#' 
#' Use a model's fitted distribution to simulate additional data with similar
#' behaviour to the response. This is a tidy implementation of 
#' `\link[stats]{simulate}`.
#' 
#' @param object A model
#' @param ... Additional optional arguments
#' 
#' @examples
#' library(fable)
#' library(tsibbledata)
#' UKLungDeaths %>% 
#'   model(lm = TSLM(mdeaths ~ fourier("year", K = 4) + fdeaths)) %>% 
#'   imitate(UKLungDeaths, times = 5)
#' 
#' @rdname imitate
#'   
#' @export
imitate <- function(object, ...){
  UseMethod("imitate")
}

#' @param new_data The data to be imitated (time index and exogenous regressors)
#' @rdname imitate
#' @export
imitate.mdl_df <- function(object, new_data = NULL, ...){
  keys <- c(key(object), sym(".model"))
  mdls <- object%@%"models"
  if(!is.null(new_data)){
    object <- bind_new_data(object, new_data)
  }
  object <- gather(object, ".model", ".fit", !!!mdls)
  
  # Evaluate simulations
  object$.sim <- map2(object[[".fit"]], 
                      object[["new_data"]] %||% rep(list(NULL), length.out = NROW(object)),
                      imitate, ...)
  unnest(add_class(object, "lst_ts"), !!sym(".sim"), key = keys)
}

#' @param h The simulation horizon (can be used instead of `new_data` for regular
#' time series with no exogenous regressors).
#' @param times The number of replications
#' @param seed The seed for the random generation from distributions
#' 
#' @rdname imitate
#' @export
imitate.model <- function(object, new_data = NULL, h = NULL, times = 1, seed = NULL, ...){
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
    new_data <- make_future_data(object$index, h)
  }
  
  if(is.null(new_data[[".rep"]])){
    new_data <- map(seq_len(times), function(rep){
      new_data[[".rep"]] <- rep
      update_tsibble(new_data, key = id(!!!syms(c(".rep", key_vars(new_data)))))
    }) %>% 
      invoke("rbind", .)
  }
  
  .sim <- imitate(object[["fit"]], new_data = new_data, ...)
  .sim[[".sim"]] <- invert_transformation(object$transformation)(.sim[[".sim"]])
  .sim
}