#' Reduces multicollinearity automatically through repeated application of \code{\link{correlationDendrogram}}.
#'
#' @description Computes the correlation between all pairs of variables in a training dataset and removes variables until there are no more variables correlated above a given threshold (0.5 Pearson correlation by default). This function requires a \code{\link{biserialCorrelation}} output, and it won't run if the argument remains \code{NULL}.
#'
#' @usage correlationDendrogram(
#'   x,
#'   variables = NULL,
#'   exclude.variables = c("x", "y", "presence"),
#'   correlation.threshold = 0.5,
#'   biserialCorrelation.output = NULL,
#'   plot = TRUE,
#'   label.size = 6
#'   )
#'
#'
#' @param x A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param variables Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{presence.column} are considered.
#' @param exclude.variables Character vector, variables to exclude from the analysis. Defaults to \code{c("x", "y", "presence")}.
#' @param correlation.threshold Numeric in the interval [0, 1], maximum Pearson correlation of the selected variables. Defaults to 0.5.
#' @param biserialCorrelation.output List, output of the function \code{\link{biserialCorrelation}}. Its R-squared scores are used to select variables.
#' @param plot Boolean, prints biserial correlation plot if \code{TRUE}.
#' @param label.size Numeric, size of the dendrogram labels.
#'
#' @return A character vector with the names of the selected variables.
#'
#' @examples
#' \dontrun{
#'data("virtualSpeciesPB")
#'
#'bis.cor <- biserialCorrelation(
#'  x = virtualSpeciesPB,
#'  exclude.variables = c("x", "y")
#')
#'
#'selected.vars <- autoCorrelationDendrogram(
#'  x = virtualSpeciesPB,
#'  variables = NULL,
#'  exclude.variables = c("x", "y", "presence"),
#'  correlation.threshold = 0.5,
#'  biserialCorrelation.output = bis.cor
#')$selected.variables
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>.
#'
#' @export
autoCorrelationDendrogram <- function(
    x,
    variables = NULL,
    exclude.variables = c("x", "y", "presence"),
    correlation.threshold = 0.5,
    biserialCorrelation.output = NULL,
    plot = TRUE,
    label.size = 6
  ){

  #checks that there is a biserial correlation output
  if(is.null(biserialCorrelation.output) == TRUE){
    stop("The argument biserialCorrelation.output is empty.")
  }

  #keeping numeric columns only and removing NA
  x <-
    x[, unlist(lapply(x, is.numeric))] %>%
    na.omit()

  #getting variables
  if(is.null(variables) == TRUE){
    variables <- colnames(x)
  }
  if(is.null(exclude.variables) == FALSE){
    if(sum(exclude.variables %in% variables) == length(exclude.variables)){
      variables <- variables[!(variables %in% exclude.variables)]
    }
  }

  #subsetting x
  if(sum(variables %in% colnames(x)) == length(variables)){
    x <- x[, variables]
  } else {
    stop("variables must be column names of x.")
  }

  #gets selected variables
  old.selected.variables <- biserialCorrelation.output$df[biserialCorrelation.output$df$p < 0.05, "variable"]

  #selects variables
  repeat{

    #computes bivariate correlation
    new.selected.variables <- correlationDendrogram(
      x = x,
      variables = old.selected.variables,
      exclude.variables = exclude.variables,
      correlation.threshold = 0.50,
      automatic.selection = TRUE,
      biserialCorrelation.output = bis.cor,
      plot = FALSE
    )$selected.variables

    if(length(old.selected.variables) == length(new.selected.variables)){
      break
    } else {
      old.selected.variables <- new.selected.variables
    }

  }

  #final plot
  if(plot == TRUE){
    correlationDendrogram(
      x = x[, new.selected.variables],
      automatic.selection = FALSE,
      biserialCorrelation.output = bis.cor,
      plot = TRUE,
      label.size = label.size
    )
  }

  return(new.selected.variables)
}
