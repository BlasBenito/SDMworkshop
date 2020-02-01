#' Plots density functions of use versus availability from a training data frame
#'
#' @description Plots the relative densities of ones (presence, a.k.a, "use") versus zeroes (background, a.k.a, "availability") for each environmental predictor in a training data frame. This plot helps to understand the relationship between use and availability in order to make informed decisions during variable selection. When for a given variable the density of use peaks over low availability it indicates that the species selects those values of a variable at a higher rate than what is expected by chance. On the other hand, variables with a very high overlap between use and availability will likely turn out to have a low predictive value during SDM fitting.
#'
#' @usage plotUseAvailability(
#'   x,
#'   presence.column = "presence",
#'   variables = NULL,
#'   exclude.variables = NULL,
#'   axis.text.size = 6,
#'   legend.text.size = 12,
#'   strip.text.size = 10
#' )
#'
#' @param x A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param presence.column Character, name of the presence column.
#' @param variables Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{presence.column} are considered.
#' @param exclude.variables Character vector, variables to exclude from the analysis.
#' @param axis.text.size Numeric, size of the axis labels.
#' @param legend.text.size Numeric, size of the legend labels.
#' @param strip.text.size Numeric, size of the panel names.
#'
#' @return A ggplot object.
#'
#' @examples
#'data("virtualSpeciesPB")
#'x <- plotUseAvailability(
#'  x = virtualSpeciesPB,
#'  presence.column = "presence",
#'  variables = NULL,
#'  exclude.variables = c("x", "y")
#')
#'
#' @author Blas Benito <blasbenito@gmail.com>
#' @export
plotUseAvailability <- function(x, presence.column = "presence", variables = NULL, exclude.variables = NULL, axis.text.size = 6, legend.text.size = 12, strip.text.size = 10){

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

  #plotea con ggplot
  plot.use.availability <- ggplot2::ggplot(
    data = x.long,
    aes(
      x = value,
      group = presence,
      fill = presence
    )
  ) +
    ggplot2::geom_density(
      alpha = 0.5,
      size = 0.2
    ) +
    ggplot2::facet_wrap("variable", scales = "free") +
    viridis::scale_fill_viridis(
      discrete = TRUE,
      direction = -1
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    theme(
      legend.position = "bottom",
      axis.text = element_text(size = axis.text.size),
      legend.text = element_text(size = legend.text.size),
      strip.text = element_text(size = strip.text.size)
      )

  return(plot.use.availability)
}
