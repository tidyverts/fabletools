#' Simulate responses from a model
#' 
#' @param object A model (typically a mable)
#' @param new_data A tsibble used to define the simulation details
#' @param ... Additional optional arguments
#' 
#' @examples
#' library(fable)
#' library(tsibbledata)
#' UKLungDeaths %>% 
#'   TSLM(mdeaths ~ fourier("year", K = 4) + fdeaths) %>% 
#'   simulate(UKLungDeaths, times = 5)
#'   
#' @export
simulate <- function(object, new_data, ...){
  UseMethod("simulate")
}

#' @export
simulate.mdl_df <- function(object, new_data, times = 1, seed = NULL, ...){
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
  
  if(is.null(new_data[[".rep"]])){
    .rep <- rep(seq_len(times), each = NROW(new_data))
    new_data <- do.call("rbind", rep(list(new_data), times))
    new_data[[".rep"]] <- .rep
    
    new_data <- key_add(new_data, !!sym(".rep"))
  }
  
  keys <- syms(key_vars(object))
  object <- bind_new_data(object, new_data)
  names(object)[names(object) == "model"] <- "object"
  object$.sim <- map2(object$object, object$new_data, simulate, ...)
  unnest(add_class(object, "lst_ts"), !!sym(".sim"), key = keys)
}