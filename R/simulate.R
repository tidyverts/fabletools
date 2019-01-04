#' Simulate responses from a model
#' 
#' @param object A model (typically a mable)
#' @param ... Additional optional arguments
#' 
#' @examples
#' library(fable)
#' library(tsibbledata)
#' UKLungDeaths %>% 
#'   model(lm = TSLM(mdeaths ~ fourier("year", K = 4) + fdeaths)) %>% 
#'   simulate(UKLungDeaths, times = 5)
#'   
#' @export
simulate <- function(object, ...){
  UseMethod("simulate")
}

#' @export
simulate.mdl_df <- function(object, new_data = NULL, h = NULL, times = 1, seed = NULL, ...){
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
    new_data <- make_future_data(object, h)
  }
  
  if(is.null(new_data[[".rep"]])){
    new_data <- map(seq_len(times), function(rep){
      new_data[[".rep"]] <- rep
      key_by(new_data, !!sym(".rep"), !!!key(new_data))
    }) %>% 
      invoke("rbind", .)
  }
  
  keys <- c(key(object), sym(".model"))
  mdls <- object%@%"models"
  object <- bind_new_data(object, new_data)
  object <- gather(object, ".model", ".fit", !!!mdls)
  
  object$.sim <- map2(object[[".fit"]], object[["new_data"]], simulate, ...)
  unnest(add_class(object, "lst_ts"), !!sym(".sim"), key = keys)
}

#' @export
simulate.model <- function(object, ...){
  simulate(object[["fit"]], ...)
}