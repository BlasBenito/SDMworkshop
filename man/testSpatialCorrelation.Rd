% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testSpatialCorrelation.R
\name{testSpatialCorrelation}
\alias{testSpatialCorrelation}
\title{Spatial correlation of presence records over predictive variables}
\usage{
testSpatialCorrelation(xy, variables)
}
\arguments{
\item{xy}{A data frame with coordinates x and y. Column names are irrelevant as long as the first column represents the x coordinate, and the second column represents the y coordinate.}

\item{variables}{A raster brick or stack with environmental variables. Must be in the same reference system of \code{xy}.}
}
\value{
A data frame with the following columns:
\itemize{
\item \emph{variable}: variable name.
\item \emph{observed}: observed autocorrelation, Moran I. Values higher than the random expectation are considered significant.
\item \emph{expected}: random expectation of Moran I for the given data.
\item \emph{p.value}: p-value of the Moran I analysis.
}
}
\description{
Takes as input a set of coordinates and a brick or stack of environmental variables and applies \code{\link[ape]{Moran.I}} (Gittleman and Kot 1990) to analyze the spatial correlation of each variable for the given points
}
\examples{
data("virtualSpecies")
data(europe2000)
sp.cor <- testSpatialCorrelation(
  xy = virtualSpecies$observed.presence,
  variables = europe2000
)
sp.cor

}
\references{
Gittleman, J. L. and Kot, M. (1990) Adaptation: statistics and a null model for estimating phylogenetic effects. Systematic Zoology, 39, 227–241.
}
\author{
Blas Benito <blasbenito@gmail.com>. The function \code{\link[ape]{Moran.I}} is authored by Julien Dutheil <julien.dutheil@univ-montp2.fr> and Emmanuel Paradis.
}
