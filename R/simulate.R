#' Simulate responses from a model
#' 
#' @param object A model (typically a mable)
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
  
  keys <- key(object)
  
  if(is.null(new_data)){
    lst_fits <- nest(group_by_key(fitted(object)))
    if(is.null(h)){
      h <- map_dbl(lst_fits$data, function(.x) get_frequencies("smallest", .x)*2)
    }
    lst_fits[["new_data"]] <- map2(lst_fits$data, h,
                                   function(data, h){
                                     idx <- expr_text(index(data))
                                     future <- seq(data[[idx]][[NROW(data)]], length.out = h + 1, by = time_unit(interval(data)))[-1]
                                     build_tsibble(list2(!!idx := future), key = id(), index = idx)
                                   })
    new_data <- unnest(lst_fits, new_data, key = keys)
  }
  
  if(is.null(new_data[[".rep"]])){
    .rep <- rep(seq_len(times), each = NROW(new_data))
    new_data <- do.call("rbind", rep(list(new_data), times))
    new_data[[".rep"]] <- .rep
    
    new_data <- key_by(new_data, !!sym(".rep"), !!!key(new_data))
  }
  
  object <- bind_new_data(object, new_data)
  names(object)[names(object) == "model"] <- "object"
  object$.sim <- map2(object$object, object$new_data, simulate, ...)
  unnest(add_class(object, "lst_ts"), !!sym(".sim"), key = keys)
}