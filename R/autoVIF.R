#' Automatic variable selection with variance inflation factor (VIF) analysis
#'
#' @description Selects variables within a dataframe that are not correlated with each other, or with linear combinations of other variables, by using the variance inflation factor (VIF) criteria implemented in the \code{\link[HH]{vif}} function.
#'
#' @usage autoVIF(x)
#'
#' @param x A data frame with numeric columns.
#' @param try.to.keep A character vector with the names of the variables the user would like to keep, in order of preference. If this argument is not \code{NULL}, the function first applies \code{\link[HH]{vif}} to the variables IN \code{try.to.keep}, then to the other variables available in \code{x}, and finally to the outcome of both vif analyses, always trying to remove variables not in \code{try.to.keep}. This option triggers a message on screen describing which variables are being removed on each step that can be shut down through the argument \code{verbose = TRUE}.
#' @param verbose Boolean, defaults to TRUE. Triggers messages describing the actions of the function when \code{try.to.keep} is not \code{NULL}.
#'
#' @return A character vector with the names of the selected variables.
#'
#' @examples
#'data("europe2000")
#'df <- raster::as.data.frame(europe2000[[c("bio1", "bio5", "bio6", "bio11")]])
#'selected.vars <- autoVIF(
#'  x = df,
#'  try.to.keep = c("bio5", "bio6", "bio1"),
#'  verbose = TRUE
#')
#'selected.vars
#'
#'
#'
#' @author Blas Benito <blasbenito@gmail.com>
#' @export
autoVIF <- function(x, try.to.keep = NULL, verbose = TRUE){

  #loading libraries
  require(HH)
  require(tidyverse, warn.conflicts = FALSE)

  #keeping numeric columns only and removing NA
  x <-
    x[, unlist(lapply(x, is.numeric))] %>%
    na.omit()

  #initializing selected vars
  selected.vars <- colnames(x)
  if(length(selected.vars) < 2){
    message("At least two variables are needed.")
    stop()
  }

  #message
  if(verbose == TRUE){cat("Removed variables: ")}

  #if try.to.keep is NULL
  if(is.null(try.to.keep) == TRUE){

  #computes vif
  repeat {

    #selects variables with vif lower than 5
    var.to.remove <-
      data.frame(
        HH::vif(xx = x[, selected.vars]),
        stringsAsFactors = FALSE
        ) %>%
      dplyr::rename(vif = 1) %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::arrange(dplyr::desc(vif)) %>%
      dplyr::filter(vif > 5) %>%
      dplyr::filter(vif == max(vif)) %>%
      dplyr::select(variable) %>%
      as.character()

    #if the first row contains a vif higher than 5
    if(var.to.remove != "character(0)"){

      #updates selected.vars
      if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
      selected.vars <- selected.vars[selected.vars != var.to.remove]

      #stops if there are less than 3 vars left
      if(length(selected.vars) < 2){
        break
      }

    #breaks if all vif values are lower than 5
    } else {
      break
    }

  } #end of repeat

  if(verbose == TRUE){cat("I'm done!")}
  return(selected.vars)

  #tries to keep variables in try.to.keep
  #--------------------------------------
  } else {

    #checks if try.to.keep is in names(x)
    if(sum(try.to.keep %in% colnames(x)) == length(try.to.keep)){

      #generates preference df
      preference <- data.frame(
        variable = c(try.to.keep, colnames(x)[!(colnames(x) %in% try.to.keep)]),
        preference = c(1:length(try.to.keep), rep(length(try.to.keep)+1, length(colnames(x)) - length(try.to.keep))),
        stringsAsFactors = FALSE
      )

      #computes vif on variables in try.to.keep
      #----------------------------------------
      removed.vars <- vector()

      repeat {

        #selects variables with vif lower than 5
        var.to.remove <-
          data.frame(
            HH::vif(xx = x[, try.to.keep]),
            stringsAsFactors = FALSE
            ) %>%
          dplyr::rename(vif = 1) %>%
          tibble::rownames_to_column(var = "variable") %>%
          dplyr::arrange(dplyr::desc(vif)) %>%
          dplyr::filter(vif > 5) %>%
          dplyr::inner_join(y = preference, by = "variable") %>%
          dplyr::filter(preference == max(preference)) %>%
          dplyr::filter(vif == max(vif))  %>%
          dplyr::select(variable) %>%
          as.character()

        #if the first row contains a vif higher than 5
        if(var.to.remove != "character(0)"){

          #updates try.to.keep
          if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
          removed.vars <- c(removed.vars, var.to.remove)
          try.to.keep <- try.to.keep[try.to.keep != var.to.remove]

          #stops if there are less than 3 vars left
          if(length(try.to.keep) < 2){
            break
          }

          #breaks if all vif values are lower than 5
        } else {
          break
        }

      } #end of repeat

      #computes vif on the other variables
      #----------------------------------------

      #gets new selected.vars vector by removing the already rejected
      #from try.to.keep
      selected.vars <- colnames(x)[!(colnames(x) %in% c(try.to.keep, removed.vars))]

      #end if less than 2 selected vars
      if(length(selected.vars) == 1){
        return(c(try.to.keep, selected.vars))
        stop()
      }

      #computes vif on the other variables
      repeat {

        #selects variables with vif lower than 5
        var.to.remove <-
          data.frame(
            HH::vif(xx = x[, selected.vars]),
            stringsAsFactors = FALSE
          ) %>%
          dplyr::rename(vif = 1) %>%
          tibble::rownames_to_column(var = "variable") %>%
          dplyr::arrange(dplyr::desc(vif)) %>%
          dplyr::filter(vif > 5) %>%
          dplyr::filter(vif == max(vif)) %>%
          dplyr::select(variable) %>%
          as.character()

        #if the first row contains a vif higher than 5
        if(var.to.remove != "character(0)"){

          #updates selected.vars
          if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
          selected.vars <- selected.vars[selected.vars != var.to.remove]

          #stops if there are less than 3 vars left
          if(length(selected.vars) < 2){
            break
          }

          #breaks if all vif values are lower than 5
        } else {
          break
        }

      } #end of repeat

      #vif on selected.vars and try.to.keep
      #--------------------------------------

      #gets new selected.vars vector by removing the already rejected
      #from try.to.keep
      selected.vars <- c(try.to.keep, selected.vars)

      #end if less than 2 selected vars
      if(length(selected.vars) < 2){
        return(selected.vars)
      }

      #computes vif on the other variables
      repeat {

        #selects variables with vif lower than 5
        var.to.remove <-
          data.frame(
            HH::vif(xx = x[, selected.vars]),
            stringsAsFactors = FALSE
          ) %>%
          dplyr::rename(vif = 1) %>%
          tibble::rownames_to_column(var = "variable") %>%
          dplyr::arrange(dplyr::desc(vif)) %>%
          dplyr::inner_join(y = preference, by = "variable")

        #if the first row contains a vif higher than 5
        if(max(var.to.remove$vif) > 5){

          var.to.remove <-
            var.to.remove %>%
            dplyr::filter(!(variable %in% try.to.keep)) %>%
            dplyr::filter(vif == max(vif)) %>%
            dplyr::select(variable) %>%
            as.character()

          #updates selected.vars
          if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
          selected.vars <- selected.vars[selected.vars != var.to.remove]

          #stops if there are less than 3 vars left
          if(length(selected.vars) < 3){
            break
          }

          #breaks if all vif values are lower than 5
        } else {
          break
        }

      } #end of repeat

      if(verbose == TRUE){cat("I'm done!")}
      return(selected.vars)

    #variables in try.to.keep not in colnames(x)
    } else {

      #identifies badly defined variables
      missing.vars <- try.to.keep[(try.to.keep %in% colnames(x)) == FALSE]

      #message for user
      if(length(missing.vars) == 1){
      paste("The variable ", missing.vars, "in the argument try.to.keep are not column names of x.") %>%
        message()
      } else {
      paste("The variables", paste(missing.vars, collapse = ", "), "in the argument try.to.keep are not column names of x.") %>%
          message()
      }
    }

  }

}
