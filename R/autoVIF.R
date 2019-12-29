#' Automatic variable selection with variance inflation factor (VIF) analysis
#'
#' @description Selects variables within a dataframe that are not correlated with each other, or with linear combinations of other variables, by using the variance inflation factor (VIF) criteria implemented in the \code{\link[HH]{vif}} function.
#'
#' @usage autoVIF(x)
#'
#' @param x A data frame with numeric columns.
#'
#' @return A character vector with the names of the selected variables.
#'
#' @author Blas Benito <blasbenito@gmail.com>
#' @export
autoVIF <- function(x){

  #loading libraries
  require(HH)
  require(tidyverse, warn.conflicts = FALSE)

  #keeping numeric columns only and removing NA
  x <-
    x[, unlist(lapply(x, is.numeric))] %>%
    na.omit()

  #initializing selected vars
  selected.vars <- colnames(x)

  repeat {

    #prepares vif output
    x.vif <- data.frame(
      HH::vif(xx = x[, selected.vars]),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::rename(vif = 1) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::arrange(dplyr::desc(vif))

    #if the first row contains a vif higher than 5
    if (x.vif[1, "vif"] > 5){

      #removes first variable with vif lower than 5
      x.vif <- x.vif[-1, ]

      #updates selected.vars
      selected.vars <- x.vif$variable

      #stops if there are less than 3 vars left
      if(nrow(x.vif) < 3){
        break
      }

    #breaks if all vif values are lower than 5
    } else {
      break
    }

  } #end of repeat

  return(selected.vars)

}
