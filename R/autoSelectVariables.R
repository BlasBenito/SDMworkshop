#' Automatic selection of predictive variables for species distribution modeling
#'
#' @description Applies \code{\link{biserialCorrelation}}, \code{\link{correlationDendrogram}} (with correlation threshold set to 0.5), and \code{\link{autoVIF}} to automatically select a set of non-correlated variables with the higher biserial correlation as possible.
#'
#' @usage autoSelectVariables(
#'   x,
#'   presence.column = "presence",
#'   variables = NULL,
#'   exclude.variables = NULL,
#'   plot = TRUE,
#'   label.size = 6
#')
#'
#'
#' @param x A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param presence.column Character, name of the presence column.
#' @param variables Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{presence.column} are considered.
#' @param exclude.variables Character vector, variables to exclude from the analysis.
#' @param plot Boolean, if \code{TRUE}, prints last correlation dendrogram to test the final output.
#' @param label.size Numeric, size of the dendrogram labels.
#'
#' @return A character vector with the names of the selected variables.
#'
#' @examples
#' \dontrun{
#' data(virtualSpeciesPB)
#' selected.vars <- autoSelectVariables(
#'   x = virtualSpeciesPB,
#'   presence.column = "presence",
#'   exclude.variables = c("x", "y")
#' )
#' selected.vars
#' HH::vif(virtualSpeciesPB[, selected.vars])
#' cor(virtualSpeciesPB[, selected.vars])
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>.
#'
#' @export
autoSelectVariables <- function(x, presence.column = "presence", variables = NULL, exclude.variables = NULL, plot = TRUE, label.size = 6){

  #computes biserial correlation
  bis.cor <- biserialCorrelation(
    x = x,
    presence.column = presence.column,
    variables = variables,
    exclude.variables = exclude.variables,
    plot = FALSE
    )

  #completes exclude variables
  exclude.variables <- c(exclude.variables, presence.column)

  #gets selected variables
  old.selected.variables <- bis.cor$df[bis.cor$df$p < 0.05, "variable"]

  #selectes variables by their bivariate correlation
  new.selected.variables <- autoCorrelationDendrogram(
    x = x,
    variables = old.selected.variables,
    exclude.variables = exclude.variables,
    correlation.threshold = 0.50,
    biserialCorrelation.output = bis.cor,
    plot = FALSE
  )

  #generates try.to.keep vector
  try.to.keep <- bis.cor$df[bis.cor$df$variable %in% new.selected.variables, ]$variable
  if(length(try.to.keep) == 0){
    try.to.keep <- bis.cor$df$variable
  }

  #autovif
  selected.variables <- autoVIF(
    x = x[, new.selected.variables],
    try.to.keep = try.to.keep,
    verbose = FALSE
  )

  #final plot
  if(plot == TRUE){
    correlationDendrogram(
      x = x[, selected.variables],
      automatic.selection = FALSE,
      biserialCorrelation.output = bis.cor,
      plot = TRUE,
      label.size = label.size
    )
  }

  #return output
  return(selected.variables)

}
