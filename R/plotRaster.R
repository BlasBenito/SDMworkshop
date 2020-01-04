#' Pretty plot of a raster file, and points if provided
#'
#' @description Uses the \code{\link{leaflet}} library to plot a raster file, and overlayed points if the point coordinates are provided. Alternatively, it accepts a grouping vector for the points (useful to, for example, plot the presences of different species at once).
#'
#'plotRaster(
#'x,
#'n = 100,
#'opacity = 0.5,
#'begin = 0,
#'end = 1,
#'direction = 1,
#'option = "D",
#'points.x = NULL,
#'points.y = NULL,
#'points.groups = NULL,
#'points.legend = "Points"
#')
#'
#' @param x A raster object. It can be the subset of a brick, as in \code{x[["variable_name"]]}.
#' @param n Number of colors (integer higher than 1) to use in the color palette. Argument of the \code{\link[viridis]{viridis}} function.
#' @param opacity Transparency in the range [0, 1]. If close to 1, the underlying political map will remain hidden.
#' @param begin Numeric in the range [0, 1], color at which the color palette starts.
#' @param end Numeric in the range [0, 1], color at which the color palette ends
#' @param direction Order of the colors in the color palette. Use -1 to reverse the color palette.
#' @param option A character string indicating the colormap option to use. Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
#' @param points.x Numeric vector with longitude coordinates for a set of points. Must be in the same coordinate reference ystem as \code{x}.
#' @param points.y Numeric vector with latitude coordinates for a set of points. Must be in the same coordinate reference ystem as \code{x}, and have the same length as \code{points.x}.
#' @param points.groups Numeric or character vector to group sets of points together. Must have the same length as \code{points.x} and \code{points.y}.
#' @param points.legend Quoted string with the title to be used in the point legend.
#' @param points.size Numeric, size of the points.
#' @return A leaflet plot.
#'
#' @examples
#' data(virtualSpecies)
#' data(europe2000)
#'plotRaster(
#'  x = europe2000[["bio1"]],
#'  option = "B",
#'  opacity = 0.7,
#'  points.x = virtualSpecies$observed.presence$x,
#'  points.y = virtualSpecies$observed.presence$y,
#'  points.size = 5
#')
#'
#' @author Blas Benito <blasbenito@gmail.com>
#' @export
plotRaster <- function(x, n = 100, opacity = 0.5, begin = 0, end = 1, direction = 1, option = "D", points.x = NULL, points.y = NULL, points.groups = NULL, points.legend = "Points", points.size = 10){

  require(raster)
  require(leaflet)
  require(viridis)

  #getting the values of x
  x.values <- na.omit(raster::values(x))

  #building color palette
  pal.raster <- leaflet::colorNumeric(
    palette = viridis::viridis(
      n = n,
      begin = begin,
      end = end,
      direction = direction,
      option = option
      ),
    domain = na.omit(unique(x.values)),
    na.color = "transparent"
  )

  #plotting raster only if there are no point coordinates
  if(is.null(points.x) == TRUE){

    #plotting the map
    leaflet() %>%
      addTiles() %>%
      addRasterImage(
        x,
        colors = pal.raster,
        opacity = opacity
      ) %>%
      addLegend(
        pal = pal.raster,
        values = x.values,
        title = names(x)
      )

    #there are points to plot
  } else {

    #preparing colors for the point groups
    if(is.null(points.groups) == TRUE){
      points.groups <- rep(1, length(points.x))
    }
    pal.groups <- colorFactor(
      palette = viridis::plasma(length(unique(points.groups))),
      domain = unique(points.groups),
      na.color = "transparent"
    )

    #mapa
    leaflet() %>%
      setView(
        lng = mean(points.x),
        lat = mean(points.y),
        zoom = 04
      ) %>%
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles() %>%
      addRasterImage(
        x,
        colors = pal.raster,
        opacity = 0.4
      ) %>%
      addLegend(
        pal = pal.raster,
        values = x.values,
        title = names(x),
        opacity = 1
      ) %>%
      addCircles(
        lng = points.x,
        lat = points.y,
        weight = points.size,
        radius = points.size,
        color = pal.groups(points.groups),
        stroke = TRUE,
        opacity = 1,
        fillOpacity = 0.2
      ) %>%
      addLegend(
        pal = pal.groups,
        values = unique(points.groups),
        title = points.legend,
        opacity = 1
      )

  }
}
