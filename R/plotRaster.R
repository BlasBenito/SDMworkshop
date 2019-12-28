#' Pretty plots of a raster file
#'
#' @description Uses the \code{\link[leaflet]} library to plot a raster file.
#'
#'plotRaster(
#'x,
#'n = 100,
#'opacity = 0.5,
#'begin = 0,
#'end = 1,
#'direction = 1,
#'option = "D"
#')
#'
#' @param x A raster object. It can be the subset of a brick, as in \code{x[["variable_name"]]}.
#' @param n Number of colors (integer higher than 1) to use in the color palette. Argument of the \code{\link[viridis]{viridis}} function.
#' @param opacity Transparency in the range [0, 1]. If close to 1, the underlying political map will remain hidden.
#' @param begin Numeric in the range [0, 1], color at which the color palette starts.
#' @param end Numeric in the range [0, 1], color at which the color palette ends
#' @param direction Order of the colors in the color palette. Use -1 to reverse the color palette.
#' @param option A character string indicating the colormap option to use. Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
#' @return A leaflet plot.
#'
#' @author Blas Benito <blasbenito@gmail.com>
#' @export
plotRaster <- function(x, n = 100, opacity = 0.5, begin = 0, end = 1, direction = 1, option = "D"){

  require(raster)
  require(leaflet)

  #geteting the values of x
  x.values <- na.omit(raster::values(x))

  #building color palette
  pal <- leaflet::colorNumeric(
    palette = viridis::viridis(
      n = n,
      begin = begin,
      end = end,
      direction = direction,
      option = option
      ),
    domain = x.values,
    na.color = "transparent"
  )

  #plotting the map
  leaflet() %>%
    addTiles() %>%
    addRasterImage(
      x,
      colors = pal,
      opacity = opacity
    ) %>%
    addLegend(
      pal = pal,
      values = x.values,
      title = names(x)
    )
}
