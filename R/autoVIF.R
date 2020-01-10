#' Automatic variable selection with variance inflation factor (VIF) analysis
#'
#' @description Selects variables within a dataframe that are not correlated with each other, or with linear combinations of other variables, by using the variance inflation factor (VIF) criteria implemented in the \code{\link[HH]{vif}} function (Heilberger and Holland 2004).
#'
#' @usage autoVIF(
#'   x,
#'   try.to.keep = NULL,
#'   verbose = TRUE
#' )
#'
#' @param x A data frame with numeric columns.
#' @param try.to.keep A character vector with the names of the variables the user would like to keep, in order of preference. If this argument is not \code{NULL}, the function first applies \code{\link[HH]{vif}} to the variables not in \code{x} that are not in \code{try.to.keep}, then to the variables in \code{try.to.keep}, and finally to the outcome of both vif analyses, while always trying to remove variables not in \code{try.to.keep}.
#' @param verbose Boolean, defaults to TRUE. Triggers messages describing what variables are being removed.
#'
#' @return A character vector with the names of the selected variables.
#'
#' @examples
#' \dontrun{
#'data("europe2000")
#'df <- raster::as.data.frame(europe2000[[c("bio1", "bio5", "bio6", "bio11", "bio12")]])
#'selected.vars <- SDMworkshop::autoVIF(
#'  x = df,
#'  try.to.keep = c("bio5", "bio6", "bio1"),
#'  verbose = TRUE
#')
#'selected.vars
#'
#'#autoVIF can also take the output of corPB
#'#as try.to.keep argument, as follows:
#' data(virtualSpeciesPB)
#'
#' cPB <- SDMworkshop::biserialCorrelationPB(
#' x = virtualSpeciesPB,
#' presence.column = "presence",
#' variables = c("bio1", "bio5", "bio6")
#' )
#'
#' #note that cPB$df$variable is ordered from
#' #higher to lower biserial correlation
#' #higher biserial correlation is linked
#' #to higher predictive importance
#' selected.vars <- SDMworkshop::autoVIF(
#'  x = df,
#'  try.to.keep = cPB$df$variable,
#'  verbose = TRUE
#')
#'selected.vars
#'
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>. The function \code{\link[HH]{vif}} is authored by Richard M. Heiberger <rmh@temple.edu>.
#' @references Heiberger, Richard M. and Holland, Burt (2004). Statistical Analysis and Data Display: An Intermediate Course with Examples in S-Plus, R, and SAS. Springer Texts in Statistics. Springer. ISBN 0-387-40270-5.
#' @export
autoVIF <- function(x, try.to.keep = NULL, verbose = TRUE){

  #keeping numeric columns only and removing NA
  x <-
    x[, unlist(lapply(x, is.numeric))] %>%
    na.omit()

  #initializing selected vars
  selected.vars <- colnames(x)

  #removing the try.to.keep vars if available
  selected.vars <- selected.vars[!(selected.vars %in% try.to.keep)]

  #message
  if(verbose == TRUE){cat("Removed variables: ")}

  #computes vif if there's more than one variable
  if(length(selected.vars) > 1){

    #computes vif
    repeat {

      #selects variables with vif lower than 5
      var.to.remove <-
        .vif2df(x = x[, selected.vars]) %>%
        dplyr::filter(vif > 5) %>%
        dplyr::filter(vif == max(vif)) %>%
        dplyr::slice(1) %>%
        dplyr::select(variable) %>%
        as.character()

      #if the first row contains a vif higher than 5
      if(var.to.remove != "character(0)"){

        #updates try.to.keep
        if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
        selected.vars <- selected.vars[selected.vars != var.to.remove]

        #stops if there are less than 3 vars left
        if(length(selected.vars) == 1){
          break
        }

      } else {
        break
      } #end of "if(var.to.remove != "character(0)")"

    } #end of repeat

  } #end of "if(length(selected.vars) > 1)..."

  #stops if there is only one selected var
  if(is.null(try.to.keep) == TRUE){
    if(verbose == TRUE){cat("I'm done! \n")}
    return(selected.vars)
    stop()
  }

  #tries to keep variables in try.to.keep
  #--------------------------------------

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
    repeat {

      #selects variables with vif lower than 5
      var.to.remove <-
        .vif2df(x = x[, try.to.keep]) %>%
        dplyr::inner_join(y = preference, by = "variable") %>%
        dplyr::filter(preference == max(preference)) %>%
        dplyr::filter(vif == max(vif))  %>%
        dplyr::slice(1) %>%
        dplyr::select(variable) %>%
        as.character()

      #if the first row contains a vif higher than 5
      if(var.to.remove != "character(0)"){

        #updates try.to.keep
        if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
        try.to.keep <- try.to.keep[try.to.keep != var.to.remove]

        #stops if there are less than 3 vars left
        if(length(try.to.keep) == 1){
          break
        }

      } #end of "if(var.to.remove != "character(0)")"

    } #end of repeat

    #end of "if(sum(try.to.keep %in% colnames(x)) == length(try.to.keep))"
  } else {

    #identifies badly defined variables
    missing.vars <- try.to.keep[(try.to.keep %in% colnames(x)) == FALSE]

    #message for user
    if(length(missing.vars) == 1){
      paste(
        "The variable ",
        missing.vars,
        "in the argument try.to.keep are not column names of x."
        ) %>%
        message()
      stop()
    } else {
      paste(
        "The variables",
        paste(
          missing.vars,
          collapse = ", "
          ),
        "in the argument try.to.keep are not column names of x."
        ) %>%
        message()
      stop()
    }
  } #end of "identifies badly defined variables"


  #vif on selected.vars and try.to.keep
  #--------------------------------------

  #gets all available variables
  selected.vars <- c(try.to.keep, selected.vars)

  #stops if there is only one selected var
  if(length(selected.vars) == 1){
    if(verbose == TRUE){cat("I'm done!")}
    return(selected.vars)
    stop()
  }

  #computes vif
  repeat {

    #selects variables with vif lower than 5
    vif.df <-
      .vif2df(x = x[, selected.vars]) %>%
      dplyr::inner_join(y = preference, by = "variable")

    #if the first row contains a vif higher than 5
    if(max(vif.df$vif) > 5){

      #selects variable to remove
      var.to.remove <-
        vif.df %>%
        dplyr::filter(!(variable %in% try.to.keep)) %>%
        dplyr::filter(vif == max(vif)) %>%
        dplyr::slice(1) %>%
        dplyr::select(variable) %>%
        as.character()

      #updates selected.vars
      if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
      selected.vars <- selected.vars[selected.vars != var.to.remove]

      #stops if there are less than 3 vars left
      if(length(selected.vars) == 1){
        break
      }

    } else {
      break
    } #end of "if(max(vif.df$vif) > 5)..."

  } #end of repeat

  if(verbose == TRUE){cat("I'm done! \n")}
  return(selected.vars)

} #end of function


#' @export
.vif2df <- function(x){

  #turns vif output into tidy df
  df <-
    data.frame(
      HH::vif(xx = x),
      stringsAsFactors = FALSE
    ) %>%
    dplyr::rename(vif = 1) %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::arrange(dplyr::desc(vif))

  return(df)
}
