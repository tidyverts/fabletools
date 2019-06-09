blendHex <- function(mixcol, seqcol, alpha=1) {
  require_package("colorspace")
  require_package("methods")
  
  if (all(is.na(seqcol))) {
    return(mixcol)
  }
  
  # transform to hue/lightness/saturation colorspace
  seqcol <- grDevices::col2rgb(seqcol, alpha = TRUE)
  mixcol <- grDevices::col2rgb(mixcol, alpha = TRUE)
  seqcolHLS <- suppressWarnings(methods::coerce(colorspace::RGB(R = seqcol[1, ] / 255, G = seqcol[2, ] / 255, B = seqcol[3, ] / 255), structure(NULL, class = "HLS")))
  mixcolHLS <- suppressWarnings(methods::coerce(colorspace::RGB(R = mixcol[1, ] / 255, G = mixcol[2, ] / 255, B = mixcol[3, ] / 255), structure(NULL, class = "HLS")))
  
  # copy luminence
  mixcolHLS@coords[, "L"] <- seqcolHLS@coords[, "L"]
  mixcolHLS@coords[, "S"] <- alpha * mixcolHLS@coords[, "S"] + (1 - alpha) * seqcolHLS@coords[, "S"]
  mixcolHex <- suppressWarnings(methods::coerce(mixcolHLS, structure(NULL, class = "RGB")))
  mixcolHex <- colorspace::hex(mixcolHex)
  mixcolHex <- ggplot2::alpha(mixcolHex, mixcol[4, ] / 255)
  return(mixcolHex)
}

#' @rdname geom_forecast
#' @export
GeomForecast <- ggplot2::ggproto("GeomForecast", ggplot2::Geom,
                                 required_aes = c("x", "y"),
                                 optional_aes = c("ymin", "ymax", "level"),
                                 default_aes = ggplot2::aes(
                                   colour = "blue", fill = "grey60", size = .5,
                                   linetype = 1, weight = 1, alpha = 1, level = NA
                                 ),
                                 handle_na = function(self, data, params) {
                                   # TODO, add smart NA handler.
                                   data
                                 },
                                 
                                 draw_key = function(data, params, size) { 
                                   lwd <- min(data$size, min(size) / 4)
                                   # Calculate and set colour 
                                   linecol <- blendHex(data$col, "gray30", 1) 
                                   fillcol <- blendHex(data$col, "#BBBBBB", 0.7) 
                                   
                                   grid::grobTree( 
                                     grid::rectGrob( 
                                       width = grid::unit(1, "npc") - grid::unit(lwd, "mm"), 
                                       height = grid::unit(1, "npc") - grid::unit(lwd, "mm"), 
                                       gp = grid::gpar( 
                                         col = fillcol, 
                                         fill = scales::alpha(fillcol, data$alpha), 
                                         lty = data$linetype, 
                                         lwd = lwd * ggplot2::.pt, 
                                         linejoin = "mitre" 
                                       ) 
                                     ), 
                                     grid::linesGrob( 
                                       x = c(0, 0.4, 0.6, 1), 
                                       y = c(0.2, 0.6, 0.4, 0.9), 
                                       gp = grid::gpar( 
                                         col = linecol, 
                                         fill = scales::alpha(linecol, data$alpha), 
                                         lty = data$linetype, 
                                         lwd = lwd * ggplot2::.pt, 
                                         linejoin = "mitre" 
                                       ) 
                                     ) 
                                   ) 
                                 }, 
                                 
                                 draw_panel = function(data, panel_scales, coord) {
                                   line_data <- data %>% filter(data$level %in% data$level[[1]])
                                   # Intervals have been provided
                                   if(any(!is.na(data[["level"]]))){
                                     # Calculate colour
                                     data$fillcol <- blendHex(data$colour, data$level, 0.7)
                                     # Compute alpha transparency
                                     data$alpha <- grDevices::col2rgb(data$fillcol, alpha = TRUE)[4, ] / 255 * data$alpha
                                     GrobList <- lapply(
                                       split(data, data$fillcol),
                                       function(x){
                                         # Select appropriate Geom and set defaults
                                         if (NROW(x) == 1) { # Linerange
                                           GeomForecastIntervalGeom <- ggplot2::GeomLinerange$draw_panel
                                           x <- transform(x, colour = fillcol, fill = NA, size = size*2)
                                         }
                                         else { # Ribbon
                                           if(all(x[["ymin"]] == x[["ymax"]], na.rm = TRUE)){
                                             GeomForecastIntervalGeom <- ggplot2::GeomLine$draw_panel
                                             x <- transform(x, y = ymin, ymin = NA, ymax = NA, colour = fillcol, fill = NA)
                                           }
                                           else{
                                             GeomForecastIntervalGeom <- ggplot2::GeomRibbon$draw_group
                                             x <- transform(x, colour = NA, fill = fillcol)
                                           }
                                         }
                                         return(list(
                                           grob = GeomForecastIntervalGeom(x, panel_scales, coord),
                                           range = mean(x[,"ymax"] - x[,"ymin"], na.rm=TRUE)
                                         )) ## Create list pair with average ymin/ymax to order layers
                                       }
                                     )
                                     # Sort GrobList
                                     GrobList <- lapply(GrobList, function(x) x[["grob"]])[order(vapply(GrobList, FUN=function(x) x[["range"]], FUN.VALUE=numeric(1)), decreasing = TRUE)]
                                   }
                                   else{
                                     GrobList <- list()
                                   }
                                   if(NROW(line_data) > 0){
                                     # Calculate colour
                                     line_data$colour <- blendHex(line_data$colour, "gray30", 1)
                                     if (NROW(line_data) == 1) { # Point
                                       GeomForecastPointGeom <- ggplot2::GeomPoint$draw_panel
                                       pointpred <- mutate(line_data, fill = NA, size = size*2, shape = 19, stroke = 1)
                                     }
                                     else { # Line
                                       GeomForecastPointGeom <- ggplot2::GeomLine$draw_panel
                                       pointpred <- mutate(line_data, fill = NA)
                                     }
                                     GrobList <- append(GrobList, list(GeomForecastPointGeom(pointpred, panel_scales, coord)))
                                   }
                                   ggplot2:::ggname("geom_forecast", do.call(grid::grobTree, GrobList))
                                 }
)

globalVariables("y")

#' Forecast plot
#'
#' Generates forecasts from the given model and adds them to the plot.
#'
#' The aesthetics required for the forecasting to work includes forecast
#' observations on the y axis, and the \code{time} of the observations on the x
#' axis. Refer to the examples below. To automatically set up aesthetics, use
#' \code{autoplot}.
#'
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams forecast
#' @param level A vector of numbers between 0 and 100 which define the confidence 
#' range to be plotted. If \code{NULL}, confidence intervals will not be plotted, 
#' giving only the forecast line.
#' @param model The time-series model used to produce the forecast. The data
#' must be \code{y} (indicating aesthetic \code{y}), and the time index for \code{y} is determined from the
#' \code{x} aesthetic.
#' @param fc_args A list of arguments to be used in the \code{\link{forecast}} function
#' 
#' @return A layer for a ggplot graph.
#' 
#' @author Mitchell O'Hara-Wild
#' @seealso \code{\link{forecast}}, \code{\link[ggplot2]{ggproto}}
#' @examples
#'
#' \dontrun{
#' library(ggplot2)
#' library(tsibble)
#' as_tsibble(cbind(mdeaths, fdeaths)) %>%
#'  autoplot() + 
#'  geom_forecast()
#' }
#'
geom_forecast <- function(mapping = NULL, data = NULL, stat = "forecast",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, level=c(80, 95), h = NULL,
                          model = fable::ETS(y), fc_args = list(), ...) {
  if(is.null(fc_args$h)){
    fc_args$h <- h
  }
  if (stat == "forecast") {
    paramlist <- list(na.rm = na.rm, levels = level,
                      model = enexpr(model), fc_args = fc_args, ...)
    if (!inherits(mapping, "uneval")) {
      mapping <- ggplot2::aes_()
    }
    if (!is.null(level)) {
      mapping$level <- quote(..level..)
    }
  }
  else {
    paramlist <- list(na.rm = na.rm, ...)
  }
  ggplot2::layer(
    geom = GeomForecast, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = paramlist
  )
}