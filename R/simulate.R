#' @export
simulate <- function(object, new_data, times = 1, seed = NULL, ...){
  UseMethod("simulate")
}

#' @export
simulate.mable <- function(object, new_data, times = 1, seed = NULL, ...){
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
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
  
  key <- syms(key_vars(object))
  object <- bind_new_data(object, new_data)
  names(object)[names(object) == "model"] <- "object"
  object$.sim <- map2(object$object, object$new_data, simulate, ...)
  unnest(object, .sim, key = key)
}