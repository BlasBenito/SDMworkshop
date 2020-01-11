#' Biserial correlation analysis of presence and background data for variable selection
#'
#' @description Computes the biserial correlation between presence and background data for a set of predictors. A high biserial correlation for a given predictor indicates that the distributions of the presence and background records are separated enough in the space of predictor values to suggest that the predictor is a good candidate for a species distribution model.
#'
#' @usage biserialCorrelation(
#'   x,
#'   presence.column = "presence",
#'   variables = NULL,
#'   exclude.variables = NULL,
#'   plot = TRUE
#')
#'
#' @param x A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param presence.column Character, name of the presence column.
#' @param variables Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{presence.column} are considered.
#' @param exclude.variables Character vector, variables to exclude from the analysis.
#' @param plot Boolean, prints biserial correlation plot if \code{TRUE}.
#'
#' @return A named list with two slots named \code{plot} and \code{df}. The former contains a ggplot object with the biserial correlation analysis. The latter is a data frame with the following columns:
#' \itemize{
#'   \emph{variable}: Name of the predictive variable.
#'   \emph{R2}: R-squared of the biserial correlation.
#'   \emph{p}: p-value of the correlation analysis.
#' }
#' The output data frame is ordered, starting with the higher R2 values.
#'
#' @examples
#' data(virtualSpeciesPB)
#' cPB <- biserialCorrelation(
#'   x = virtualSpeciesPB,
#'   presence.column = "presence",
#'   variables = c("bio1", "bio5", "bio6")
#' )
#'
#' @author Blas Benito <blasbenito@gmail.com>
#' @export
biserialCorrelation <- function(x, presence.column = "presence", variables = NULL, exclude.variables = NULL, plot = TRUE){

  #getting variables
  if(is.null(variables) == TRUE){
    variables <- colnames(x)[colnames(x) != presence.column]
  }
  if(is.null(exclude.variables) == FALSE){
    variables <- variables[!(variables %in% exclude.variables)]
  }

  #subsetting x
  x <- x[, c(presence.column, variables)]

  #keeping numeric columns only and removing NA
  x <-
    x[, unlist(lapply(x, is.numeric))] %>%
    na.omit()

  #to long format
  x.long <-
    x %>%
    tidyr::pivot_longer(
    cols = variables,
    names_to = "variable",
    values_to = "value"
  ) %>%
    data.frame() %>%
    dplyr::rename(presence = 1)

  #presence to factor for easier plotting
  x.long[, presence.column] <- factor(x.long[, presence.column])

  #plotea primero
  biserial.plot <- ggplot2::ggplot(
    data = x.long,
    aes(
      x = presence,
      y = value,
      group = variable,
      color = presence
    )
  ) +
    ggplot2::geom_point(
      alpha = 0.05,
      size = 3
    ) +
    ggplot2::facet_wrap("variable", scales = "free") +
    viridis::scale_color_viridis(
      discrete = TRUE,
      direction = -1
    ) +
    ggplot2::geom_smooth(
      method = "lm",
      size = 2,
      color = viridis::viridis(1, begin = 0.5)) +
    ggplot2::guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    ggplot2::ylab("Variable") +
    ggplot2::xlab("Presence")

  #prints plot to screen
  if(plot == TRUE){
    print(biserial.plot)
  }

  #dataframe to store results
  biserial.correlation <- data.frame(
    variable = variables,
    R2 = NA,
    p = NA,
    stringsAsFactors = FALSE
  )

  #iterates through variables
  for(variable in variables){

    #computes correlation
    temp.cor <- cor.test(
      x[, presence.column],
      x[, variable]
      )

    #stores outcome
    biserial.correlation[
      biserial.correlation$variable == variable ,
      c("R2", "p")
      ] <- c(abs(temp.cor$estimate), round(temp.cor$p.value, 4))

  }

  #orders by R2
  biserial.correlation <-
    biserial.correlation %>%
    dplyr::arrange(dplyr::desc(R2))

  #resets rownames
  row.names(biserial.correlation) <- 1:nrow(biserial.correlation)

  #lista de resultados
  output.list <- list()
  output.list$plot <- biserial.plot
  output.list$df <- biserial.correlation

  return(output.list)

}
