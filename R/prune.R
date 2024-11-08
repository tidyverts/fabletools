prune_aggregates <- function(.data, .spec = NULL, .predicate = NULL, ..., .keep = any){
  .spec <- enexpr(.spec)
  if(is.null(.spec)){
    kv <- syms(key_vars(.data))
    message(
      sprintf("Key structural specification not found, defaulting to `.spec = %s`",
              paste(kv, collapse = "*"))
    )
    .spec <- reduce(kv, call2, .fn = "*")
  }
  
  key_comb <- parse_agg_spec(.spec)
  
  idx <- index2_var(.data)
  intvl <- interval(.data)
  kd <- key_data(.data)
  cn <- colnames(.data)
  has_varied_index <- any(has_gaps(.data, .full = TRUE)[[".gaps"]]) && !is_ordered(.data)
  .data <- as_tibble(.data)
  kv <- unique(unlist(key_comb, recursive = FALSE))
  
  agg_dt <- map_dfr(unname(key_comb), function(x){
    gd <- group_data(group_by(.data, !!!set_names(map(x, function(.) expr(graphvec::agg_vec(!!sym(.)))), x), !!sym(idx)))
    agg_keys <- setdiff(kv, x)
    agg_cols <- rep(list(graphvec::agg_vec(NA, aggregated = TRUE)), length(agg_keys))
    gd[agg_keys] <- agg_cols
    gd[c(kv, idx, ".rows")]
  })
  
  
  # Construct graph structure
  agg_grp <- group_data(group_by(agg_dt, !!!syms(kv)))
  # Currently assume the root is sorted to be the last observation
  # search_order <- igraph::bfs(graphvec:::graphvec_to_igraph(agg_grp[kv]), nrow(agg_grp), mode = "in")$order
  # search_order <- igraph::bfs(graphvec:::graphvec_to_igraph(agg_grp[kv]), nrow(agg_grp), mode = "in", father = TRUE)
  # search_order <- as.integer(search_order)
  
  # Coloured edge breadth first search
  keep <- rep(NA, nrow(agg_grp))
  g <- graphvec:::combine_graph(agg_grp[kv])
  
  ## Find roots of g
  roots <- setdiff(unique(g$to), unique(unlist(g$from)))
  
  # Series of interest
  prune_series <- function(i){
    if(length(i) == 0) return(logical())
    cur_agg <- dplyr::new_grouped_df(.data, groups = agg_dt[unlist(agg_grp[[".rows"]][i]),])
    summarise(summarise(cur_agg, ...), .keep = {{.predicate}})$.keep
  }
  
  ## Prune
  i <- traverse(
    roots, 
    .f = function(.x, .y){
      unique(unlist(c(.x, .y)))
    },
    # Check if series satisfy .predicate and .keep
    .g = function(x) {
      # Test series which have not yet been tested
      if(any(new_x <- is.na(keep[x]))) {
        keep[x] <<- prune_series(x[new_x])
      }
      
      # Check if all series satisfy .keep test
      if(.keep(keep[x])) {
        g <- g[g$to %in% x,]
        g$from
      } else {
        NULL
      }
    },
    
    base = function(...) FALSE
  )
  
  summarise(
    dplyr::new_grouped_df(.data, groups = agg_dt[unlist(agg_grp[[".rows"]][i]),]),
    ...
  )
}