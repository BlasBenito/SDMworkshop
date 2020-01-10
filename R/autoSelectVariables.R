#' Automatic selection of predictive variables for species distribution modeling
#'
#' @description Computes the biserial correlation between presence and background data for a set of predictors with \code{\link{biserialCorrelationPB}}, and uses the output of this function as argument \code{try.to.keep} in \code{\link{autoVIF}}. This approach should provide a reasonable outcome when modelling with presence-background data. If you have a preference on what variables to use in your modelling exercise, please use \code{\link{autoVIF}} instead.
#'
#' @usage autoSelectVariables(
#'   x,
#'   presence.column = "presence",
#'   variables = NULL,
#'   exclude.variables = NULL,
#'   verbose = TRUE
#')
#'
#'
#' @param x A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param presence.column Character, name of the presence column.
#' @param variables Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{presence.column} are considered.
#' @param exclude.variables Character vector, variables to exclude from the analysis.
#' @param verbose Boolean, defaults to TRUE. Triggers messages describing what variables are being removed.
#'
#' @return A character vector with the names of the selected variables.
#'
#' #' @examples
#' \dontrun{
#'data("virtualSpeciesPB")
#'selected.vars <- autoSelectVariables(
#'  x = virtualSpeciesPB,
#'  presence.column = "presence",
#'  variables = NULL,
#'  exclude.variables = c("x", "y"),
#'  verbose = TRUE
#')
#'selected.vars
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>.
#'
#' @export
autoSelectVariables <- function(x, presence.column = "presence", variables = NULL, exclude.variables = NULL, verbose = TRUE){

  #computes biserial correlation
  bis.cor <- biserialCorrelationPB(
    x = x,
    presence.column = presence.column,
    variables = variables,
    exclude.variables = exclude.variables,
    plot = FALSE
    )$df

  #removing vars with very low p
  bis.cor <- bis.cor[bis.cor$p < 0.1, ]

  #select ones with p-value lower than 0.05
  selected.vars <- bis.cor[bis.cor$p < 0.05, "variable"]

  #if none has p < 0.05
  if(length(selected.vars) == 0){
    selected.vars <- bis.cor[1:floor(nrow(bis.cor)/2), "variable"]
  }

  #autoVIF
  selected.vars <- autoVIF(
    x = x[, bis.cor$variable],
    try.to.keep = selected.vars,
    verbose = verbose
  )

  #return output
  return(selected.vars)

}
