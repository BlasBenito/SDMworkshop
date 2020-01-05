#' Applies "thinning" to a set of coordinates to reduce their spatial correlation
#'
#' @description This function reduces the spatial clustering of a set of presence records. It is intended to reduce spatial autocorrelation, and reduce sampling bias, particularly at larger geographical scales. It takes as input a set of coordinates, a brick or stack of environmental variables, and a minimum distance, and returns a new set of coordinates in which the distance between adjacent points is equal or higher than the established minimum distance. This operation is named "thinning", and helps to reduce the spatial correlation of a presence dataset. This function applies thinning, but preserves the presence records representing the extremes of any of the predictive variables provided.
#'
#' @usage reduceSpatialCorrelation(xy, variables, minimum.distance = NULL, random.start = FALSE, seed = NULL, verbose = FALSE)
#'
#' @param xy A data frame with two columns with coordinates x and y. Column names are irrelevant as long as the first column represents the x coordinate, and the second column represents the y coordinate.
#' @param variables A raster brick or stack with environmental variables. Must be in the same reference system of \code{xy}.
#' @param minimum.distance Numeric, minimum distance between consecutive points in the output dataset. Defaults to the resolution of \code{variables}. The minimum distance can be extracted from the resolution of \code{variables}, as in  \code{min.dist <- xres(variables)}.
#' @param random.start Boolean, defaults to \code{FALSE}). If \code{TRUE}, the \code{xy} dataset is randomly reshuffled so the start of the thinning changes, and with that, the output dataset.
#' @param seed Integer determining a random seed. Only relevant when \code{random.start = TRUE}. Added to allow reproducibility in the generation of datasets with a random start.
#' @param verbose Boolean. If \code{FALSE} (default), all messages are supressed.
#'
#' @return A data frame with the same columns as \code{xy}, but a lower number of records.
#'
#' @examples
#' data("virtualSpecies")
#' data(europe2000)
#' xy.thinned <- reduceSpatialCorrelation(
#'   xy = virtualSpecies$observed.presence,
#'   variables = europe2000,
#'   minimum.distance = 4
#' )
#' xy.thinned
#'
#' #generating datasets with different starting points
#' #generates a different dataset with different nrow on each run
#' xy.thinned <- reduceSpatialCorrelation(
#'   xy = virtualSpecies$observed.presence,
#'   variables = europe2000,
#'   minimum.distance = 4,
#    random.start = TRUE
#' )
#' nrow(virtualSpecies$observed.presence)
#' nrow(xy.thinned)
#'
#' @author Blas Benito <blasbenito@gmail.com>
#' @export
reduceSpatialCorrelation = function(xy, variables, minimum.distance = NULL, random.start = FALSE, seed = NULL, verbose = FALSE){

  #gets only two columns of the input dataset
  if(ncol(xy) > 2){xy <- xy[, 1:2]}

  #change column names
  old.column.names <- names(xy)
  names(xy) <- c("x", "y")

  #computes minimum distance between points
  if(is.null(minimum.distance) == TRUE){minimum.distance <- raster::xres(variables)}
  min.dist = raster::xres(variables) * minimum.distance

  #extracts variable values for xy
  xy <- na.omit(
    data.frame(
      xy,
      raster::extract(
        x = variables,
        y = xy,
        df = TRUE
      )
    )
  )

  #remove column
  xy$ID <- NULL

  #generates a data frame with the extreme values of variables
  indices.extreme.values <- vector()
  for(variable in names(xy)){
    indices.extreme.values <- c(indices.extreme.values, which.min(xy[, variable]), which.max(xy[, variable]))
  }

  #removes repeated records
  indices.extreme.values <- unique(indices.extreme.values)

  #generates a dataframe with the extreme values
  xy.extreme.values <- xy[indices.extreme.values, ]

  #removes those from xy
  xy <- xy[-indices.extreme.values, ]

  #applies random reshuffling of xy
  if(random.start == TRUE){
    if(is.null(seed) == FALSE){set.seed(seed)}
    xy <- xy[sample(nrow(xy)), ]
  }

  #count rows
  row<-1

  #loops through records
  repeat{

    #gets the current record
    f <- xy[row, ]

    #generates a bounding box around the record
    ymax <- f$y + min.dist
    ymin <- f$y - min.dist
    xmax <- f$x + min.dist
    xmin <- f$x - min.dist

    #selects other records within the bounding box and removes them
    xy <- xy[!((xy$y <= ymax) & (xy$y >= ymin) & (xy$x <= xmax) & (xy$x >= xmin) & (xy$y != f$y | xy$x != f$x)), ]

    #writes message
    if(verbose == TRUE){
      print(paste("Processed rows: ", row, " out of ", nrow(xy), sep=""))
    }

    #advances the counter one position
    row <- row+1

    #stops when there are no more records left
    if(row >= nrow(xy)){break}
  }

  #adds the records with extreme values
  xy <- rbind(xy.extreme.values, xy)[, c("x", "y")]

  return(xy)

}
