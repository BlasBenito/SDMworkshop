#' Correlation dendrogram to help reduce multicollinearity in a training dataset.
#'
#' @description Computes the correlation between all pairs of variables in a training dataset. If a \code{\link{biserialCorrelation}} output is provided, it further selects variables automatically based on the R-squared value obtained by each variable in the biserial correlation analysis.
#'
#' @usage correlationDendrogram(
#'   x,
#'   variables = NULL,
#'   exclude.variables = c("x", "y", "presence"),
#'   correlation.threshold = 0.5,
#'   automatic.selection = TRUE,
#'   biserialCorrelation.output = NULL,
#'   plot = TRUE,
#'   label.size = 6
#'   )
#'
#'
#' @param x A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param variables Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{presence.column} are considered.
#' @param exclude.variables Character vector, variables to exclude from the analysis. Defaults to \code{c("x", "y", "presence")}.
#' @param correlation.threshold Numeric in the interval [0, 1], maximum Pearson correlation of the selected variables.
#' @param automatic.selection Boolean. If \code{TRUE}, the function provides a vector of selected variables along with the dendrogram plot. Otherwise, only the dendrogram plot is returned.
#' @param biserialCorrelation.output List, output of the function \code{\link{biserialCorrelation}}. Its R-squared scores are used to select variables.
#' @param plot Boolean, prints biserial correlation plot if \code{TRUE}.
#' @param label.size Numeric, size of the dendrogram labels.
#'
#' @return If \code{automatic.selection = TRUE}, a list with two slots named "dendrogram" (a ggplot2 object) and "selected.variables" with the dendrogram and the character vector with the selected variables. Otherwise, only returns the dendrogram.
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
#'selected.vars <- correlationDendrogram(
#'  x = virtualSpeciesPB,
#'  variables = NULL,
#'  exclude.variables = c("x", "y", "presence"),
#'  correlation.threshold = 0.5,
#'  automatic.selection = TRUE,
#'  biserialCorrelation.output = bis.cor
#')$selected.variables
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>.
#'
#' @export
correlationDendrogram <- function(
  x,
  variables = NULL,
  exclude.variables = c("x", "y", "presence"),
  correlation.threshold = 0.5,
  automatic.selection = TRUE,
  biserialCorrelation.output = NULL,
  plot = TRUE,
  label.size = 6
  ){

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

  #computes correlation matrix
  cor.matrix <-
    x %>%
    cor() %>%
    abs() %>%
    as.dist()

  #cluster (converts correlation to distance)
  temp.cluster <- hclust(1 - cor.matrix)

  #if biserialCorrelation.output == NULL
  #-------------------------------------
  if(is.null(biserialCorrelation.output) == TRUE){

    #generates cluster data
    temp.cluster.data <- ggdendro::dendro_data(temp.cluster)

    #plots cluster
      cluster.plot <- ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = ggdendro::segment(temp.cluster.data),
          aes(
            x = x,
            y = y,
            xend = xend,
            yend = yend)
        ) +
        ggplot2::geom_text(
          data = ggdendro::label(temp.cluster.data),
          aes(
            label = label,
            x = x,
            y = 0,
            hjust = 1
          ),
          size = label.size
        ) +
        ggplot2::coord_flip(ylim = c(-0.4, 1)) +
        viridis::scale_colour_viridis(direction = -1, end = 0.9)  +
        ggplot2::theme(
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(2,2,2,2), "lines"),
          axis.text.x = element_text(size = label.size * 2),
          legend.position = "bottom",
          legend.key.width = unit(2, "lines")
        ) +
        ggplot2::labs(colour = "R2") +
        ggplot2::geom_hline(
          yintercept = 1 - correlation.threshold,
          col = "red4",
          linetype = "dashed",
          size = 1,
          alpha = 0.5
        ) +
        ggplot2::scale_y_continuous(breaks = c(1 - correlation.threshold, 0, 0.25, 0.5, 0.75, 1)) +
        ggplot2::ylab("1 - correlation")

      if(plot == TRUE){
        ggplot2::theme_set(cowplot::theme_cowplot())
        print(cluster.plot)
        }

      return(cluster.plot)

  } else {

    #gets only significnant variables from biserial correlation ouotput
    selected.variables <- biserialCorrelation.output$df[biserialCorrelation.output$df$p < 0.05, "variable"]

    #automatic selection
    if(automatic.selection == TRUE){

      #table of groups
      temp.cluster.groups <- data.frame(group = cutree(temp.cluster, h = 1 - correlation.threshold))
      temp.cluster.groups$variable <- row.names(temp.cluster.groups)
      temp.cluster.groups <- temp.cluster.groups[
        order(
          temp.cluster.groups$group,
          decreasing = FALSE
        ), ]
      row.names(temp.cluster.groups) <- 1:nrow(temp.cluster.groups)

      #adds biserial correlation to cluster labels
      temp.cluster.groups$R2 <- biserialCorrelation.output$df[
        match(
          temp.cluster.groups$variable,     #cluster labels
          biserialCorrelation.output$df$variable #variables in biserial correlation output
        ), "R2"
        ]

      #gets the maximum of each group
      selected.variables <-
        temp.cluster.groups %>%
        dplyr::group_by(group) %>%
        dplyr::slice(which.max(R2)) %>%
        .$variable

    } #end of automatic selection


    #prepares cluster plotting
    temp.cluster.data <- ggdendro::dendro_data(temp.cluster)

    #gets R2
    temp.cluster.data$labels$R2 <- biserialCorrelation.output$df[
      match(
        temp.cluster.data$labels$label,     #etiquetas cluster
        biserialCorrelation.output$df$variable #variables biserialCorrelation.output
      ), "R2"
      ]

    #gets labels
    labs <- ggdendro::label(temp.cluster.data)

    #adds arrow to label if the variable is selected
    if(automatic.selection == TRUE){
      labs$label <- as.character(labs$label)
      for(i in 1:nrow(labs)){
        if(labs[i, "label"] %in% selected.variables){
          labs[i, "label"] <- paste("\u{2192} ", labs[i, "label"], sep = "")
        }
      }
    }
    labs$label <- factor(labs$label)


    #plots dendrogram
      cluster.plot <- ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = ggdendro::segment(temp.cluster.data),
          aes(
            x = x,
            y = y,
            xend = xend,
            yend = yend)
        ) +
        ggplot2::geom_text(
          data = ggdendro::label(temp.cluster.data),
          aes(
            label = labs$label,
            x = x,
            y = 0,
            colour = labs$R2,
            hjust = 1
          ),
          size = label.size
        ) +
        ggplot2::coord_flip(ylim = c(-0.4, 1)) +
        viridis::scale_colour_viridis(direction = -1, end = 0.9)  +
        ggplot2::theme(
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(2,2,2,2), "lines"),
          axis.text.x = element_text(size = label.size * 2),
          legend.position = "bottom",
          legend.key.width = unit(2, "lines")
        ) +
        ggplot2::labs(colour = "R2") +
        ggplot2::geom_hline(
          yintercept = 1 - correlation.threshold,
          col = "red4",
          linetype = "dashed",
          size = 1,
          alpha = 0.5
        ) +
        ggplot2::scale_y_continuous(breaks = c(1 - correlation.threshold, 0, 0.25, 0.5, 0.75, 1)) +
        ggplot2::ylab("1 - correlation")

      if(plot == TRUE){
        ggplot2::theme_set(cowplot::theme_cowplot())
        print(cluster.plot)
        }

    #output list
    if(automatic.selection == TRUE){
      output.list <- list()
      output.list$dendrogram <- cluster.plot
      output.list$selected.variables <- selected.variables
      return(output.list)
    } else {
      return(cluster.plot)
    }

  }

}
