#' @importFrom ggplot2 ggplot aes geom_line guides guide_legend xlab
#' @export
autoplot.tbl_ts <- function(object, .vars = NULL, ...){
  quo_vars <- enquo(.vars)
  
  kv <- key_vars(object)
  nk <- n_keys(object)
  
  if(quo_is_null(quo_vars)){
    if(is_empty(measured_vars(object))){
      abort("There are no variables to plot.")
    }
    inform(sprintf(
      "Plot variable not specified, automatically selected `.vars = %s`",
      measured_vars(object)[1]
    ))
    y <- sym(measured_vars(object)[1])
    .vars <- as_quosures(list(y), env = empty_env())
  }
  else if(possibly(compose(is_quosures, eval_tidy), FALSE)(.vars)){
    .vars <- eval_tidy(.vars)
    object <- gather(
      mutate(object, !!!.vars),
      ".response", "value", !!!map(.vars, quo_name), factor_key = TRUE
    )
    y <- sym("value")
  }
  else{
    y <- get_expr(quo_vars)
    .vars <- as_quosures(list(y), env = empty_env())
  }
  
  aes_spec <- list(x = index(object), y = y)
  
  if(nk > 1){
    aes_spec["colour"] <- list(expr(interaction(!!!syms(kv), sep = "/")))
  }
  
  p <- ggplot(object, eval_tidy(expr(aes(!!!aes_spec)))) + 
    geom_line(...) +
    xlab(paste0(expr_text(index(object)), " [", format(interval(object)), "]"))
  
  if(nk > 1){
    p <- p + 
      guides(colour = guide_legend(paste0(kv, collapse = "/")))
  }
  
  if(length(.vars) > 1){
    p <- p + facet_wrap(vars(!!sym(".response")), scales = "free_y", 
                        ncol = length(.vars)) + ggplot2::ylab(NULL)
  }
  
  p
}

#' @importFrom ggplot2 ggplot aes geom_line guides guide_legend xlab
#' @export
autolayer.tbl_ts <- function(object, .vars = NULL, series = NULL, ...){
  quo_vars <- enquo(.vars)
  kv <- key_vars(object)
  nk <- n_keys(object)
  
  if(quo_is_null(quo_vars)){
    if(is_empty(measured_vars(object))){
      abort("There are no variables to plot.")
    }
    inform(sprintf(
      "Plot variable not specified, automatically selected `.vars = %s`",
      measured_vars(object)[1]
    ))
    y <- sym(measured_vars(object)[1])
    .vars <- as_quosures(list(y), env = empty_env())
  }
  else if(possibly(compose(is_quosures, eval_tidy), FALSE)(.vars)){
    .vars <- eval_tidy(.vars)
    object <- gather(
      mutate(object, !!!.vars),
      ".response", "value", !!!map(.vars, quo_name), factor_key = TRUE
    )
    y <- sym("value")
  }
  else{
    y <- get_expr(quo_vars)
    .vars <- as_quosures(list(y), env = empty_env())
  }
  
  aes_spec <- list(x = index(object), y = y)
  
  if(!is.null(series)){
    aes_spec$colour <- series
  }
  else if(nk > 1){
    aes_spec["colour"] <- list(expr(interaction(!!!syms(kv), sep = "/")))
  }
  if(n_keys(object) > 1){
    aes_spec["group"] <- list(expr(interaction(!!!syms(key_vars(object)), sep = "/")))
  }
  
  geom_line(eval_tidy(expr(aes(!!!aes_spec))), data = object, ...)
}

#' @importFrom ggplot2 fortify
#' @export
fortify.fbl_ts <- function(object, level = c(80, 95)){
  resp <- object%@%"response"
  dist <- object%@%"dist"
  idx <- index(object)
  kv <- key_vars(object)
  
  if(length(resp) > 1){
    object <- object %>%
      mutate(
        .response = rep(list(factor(map_chr(resp, expr_text))), NROW(object)),
        value = transpose_dbl(list2(!!!resp))
      )
  }
  
  if(!is.null(level)){
    object <- object %>% 
      mutate(
        !!!set_names(
          map(level, function(.x) expr(hilo(!!dist, !!.x))), 
          level
        )
      )
    
    object <- gather(object, ".rm", ".hilo", !!!syms(as.character(level)))
    
    if(length(resp) > 1){
      object <- unnest_tbl(object, c(".response", "value", ".hilo"))
      resp <- syms("value")
      kv <- c(kv, ".response")
    }
    else{
      object <- unnest_tbl(object, ".hilo")
    }
    kv <- c(kv, ".level")
    
    # Drop temporary col
    object[".rm"] <- NULL
  }
  else if (length(resp) > 1) {
    object <- unnest_tbl(object, c(".response", "value"))
    kv <- c(kv, ".response")
  }
  
  as_tsibble(object[setdiff(colnames(object), expr_text(dist))],
             key = kv, index = !!idx, validate = FALSE) 
}

#' @importFrom ggplot2 facet_wrap
#' @export
autoplot.fbl_ts <- function(object, data = NULL, level = c(80, 95), ...){
  fc_resp <- object%@%"response"
  fc_key <- setdiff(key_vars(object), ".model")
  has_keys <- any(duplicated(key_data(object)$.model))
  
  aes_y <- if(length(fc_resp) > 1){
    sym("value")
  }
  else{
    sym(expr_text(fc_resp[[1]]))
  }

  if (!is.null(data)){
    if(!identical(fc_key, key_vars(data))){
      abort("Provided data contains a different key structure to the forecasts.")
    }
    
    if(!is_empty(key(data))){
      data <- semi_join(data, object, by = key_vars(data))
    }
    
    if(length(fc_resp) > 1){
      data <- gather(data, ".response", "value", !!!fc_resp, factor_key = TRUE)
    }
  }

  # Change colours to be more appropriate for later facets
  fc_layer <- autolayer(object, level = level, ...)
  fc_layer$mapping$colour <- set_expr(fc_layer$mapping$colour, sym(".model"))
  
  p <- ggplot(data, aes(x = !!index(object), y = !!aes_y)) + 
    fc_layer
    
  if(!is.null(data)){
    p <- p + geom_line()
  }
  
  if(length(fc_resp) > 1){
    p <- p + 
      facet_wrap(vars(!!!syms(c(".response", fc_key))),
                 ncol = length(fc_resp), scales = "free_y") +
      ggplot2::ylab(NULL)
  } else if(has_keys){
    p <- p + facet_wrap(vars(!!!syms(fc_key)),
                        ncol = 1, scales = "free_y")
  }
  
  p
}

#' @export
autolayer.fbl_ts <- function(object, level = c(80, 95), series = NULL, ...){
  fc_key <- setdiff(key_vars(object), ".model")
  key_data <- key_data(object)
  distinct_mdls <- duplicated(key_data[[".model"]])
  data <- fortify(object, level = level)
  if(length(object%@%"response") > 1){

    resp <- sym("value")
    grp <- syms(".response")
  }
  else{
    resp <- sym(expr_text((object%@%"response")[[1]]))
    grp <- NULL
  }
  
  mapping <- aes(
    x = !!index(data),
    y = !!resp
  )
  
  if(!is.null(level)){
    mapping$level <- sym(".level")
    mapping$ymin <- sym(".lower")
    mapping$ymax <- sym(".upper")
  }
  
  if(!is_empty(fc_key)){
    grp <- c(grp, syms(fc_key))
  }
  if(!is.null(series)){
    mapping$colour <- series
    grp <- c(grp, series, syms(".model"))
  }
  else if(NROW(key_data) > 1){
    col <- c(
      if(sum(distinct_mdls) > 1) syms(fc_key) else NULL,
      if(sum(!distinct_mdls) > 1) syms(".model") else NULL
    )
    
    mapping$colour <- expr(interaction(!!!col, sep = "/"))
    grp <- c(grp, syms(".model"))
  }
  if(length(grp) > 0){
    mapping$group <- expr(interaction(!!!grp, sep = "/"))
  }
  
  geom_forecast(mapping = mapping, stat = "identity", data = data, ...)
}

#' @importFrom ggplot2 ggplot geom_line geom_rect facet_grid vars ylab labs
#' @export
autoplot.dcmp_ts <- function(object, y = NULL, scale_bars = TRUE, ...){
  method <- object%@%"method"
  idx <- index(object)
  keys <- key(object)
  n_keys <- n_keys(object)
  
  y <- enquo(y)
  if(quo_is_null(y)){
    y <- object%@%"resp"
  }
  dcmp_str <- dcmp <- (object%@%"aliases")[[as_string(get_expr(y))]]
  if(!is.null(dcmp_str)){
    dcmp_str <- expr_text(dcmp_str)
  }
  object <- object %>% 
    transmute(!!y, !!!syms(all.vars(dcmp))) %>% 
    gather(".var", ".val", !!!syms(measured_vars(.)), factor_key = TRUE)
  
  line_aes <- aes(x = !!idx, y = !!sym(".val"))
  if(n_keys > 1){
    line_aes$colour <- expr(interaction(!!!keys, sep = "/"))
  }
  
  p <- object %>% 
    ggplot() + 
    geom_line(line_aes) + 
    facet_grid(vars(!!sym(".var")), scales = "free_y") + 
    ylab(NULL) + 
    labs(
      title = paste(method%||%"A", "decomposition"), 
      subtitle = paste(c(expr_text(get_expr(y)), dcmp_str), collapse = " = ")
    )
  
  # Rangebars
  if (scale_bars) {
    xranges <- range(object[[expr_text(idx)]])
    barwidth <- pmax(1, round((1 / 64) * diff(units_since(xranges))))
    
    # Avoid issues with visible bindings
    ymin <- ymax <- center <- diff <- NULL
    
    range_data <- object %>%
      as_tibble %>% 
      group_by(!!sym(".var")) %>% 
      summarise(ymin = min(!!sym(".val"), na.rm = TRUE), ymax = max(!!sym(".val"), na.rm = TRUE)) %>% 
      mutate(
        center = (ymin + ymax) / 2,
        diff = min(ymax - ymin),
        xmin = xranges[1] - barwidth * 2, xmax = xranges[1] - barwidth,
        ymin = center - diff/2, ymax = center + diff/2
      )
    
    p <- p + geom_rect(data = range_data,
      aes(ymin = !!sym("ymin"), ymax = !!sym("ymax"),
          xmin = !!sym("xmin"), xmax = !!sym("xmax")),
      fill = "gray75", colour = "black", size = 1 / 3
    )
  }
  
  if(!is_empty(keys)){
    p <- p + guides(colour = guide_legend(paste0(map_chr(keys, expr_text), collapse = "/")))
  }
  
  p
}
