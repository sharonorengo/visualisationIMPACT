#' Create boxplot standardize with IMPACT style
#'
#' @param .data:  data that contains the statistical result to build boxplots
#' @param x: element of .data that contains the different values of the categorical data
#' @param name.y: name of value calculated
#' @param median: element of .data containing the median values
#' @param first_quantile: element of .data containing lower hinges correspond to the first quartile
#' @param third_quantile: element of .data containing upper hinges correspond to the third quartile
#' @param whisker_min: element of .data containing the value of the lower whisher. Usually calculated as 1.5*IQR smallest value from the hinge
#' @param whisker_max: element of .data containing the value of the upper whisher. Usually calculated as 1.5*IQR largest value from the hinge
#' @param outlier_min (optional): element of .data containing the most extreme value beyond the lower whisper.
#' @param outlier_max (optional): element of .data containing the most extreme value beyond the upper whisper.
#' @param sens.boxplot (optional): if sens.boxplot = "vertical" (default) boxplots are build with vertical cartesian coordinates. If sens.boxplot="horizontal" flip cartesian coordinates so that vertical becomes horizontal
#' @details Create a plot with one or multiple boxplot standardize with IMPACT colors, fonts, ... for the same numerical variable
#' @return a ggplot object contaning a boxplot
#' @export
regression_impact <- function(.data, x, y, weight = NULL){  #, add.regression.line = FALSE

  x <- enquo(x)
  y <- enquo(y)
  weight <- enquo(weight)

  # if(!is.logical(add.regression.line)){
  #   stop("Parameter add.regression.line is logical. Please enter a valid value")
  # }

  # Check and return message if empty evironnement
  stop_msg <- error_message_empty_env_regression(x, y)
  if(stop_msg != ""){
    stop(paste0("The variable(s) following does not exist in .data: ",stop_msg))
  }
  #No plot if x and y is only NA
  check_contains_only_NA(x,.data)
  check_contains_only_NA(y,.data)

  # Draw plot
  theplot <- ggplot(.data, aes(x = !!x, y = !!y)) + theme_impact()

  if (rlang::quo_is_null(weight)) {
    warning("Could not find the weight column. No weights will be add to the plot")
    theplot <- theplot + geom_point_impact()
  }
  else{
  theplot <- theplot + geom_point_impact(aes(size = !!weight ) )
    # scale_fill_reach_categorical(n=nrow(dplyr::distinct(.data,!!subset.x)),name="")
  }

  #To add later to add a line or function to draw the regression with error
  # if(add.regression.line == TRUE){
  #   theplot <- theplot + geom_smooth_impact()
  #
  # }



  return(theplot)
}


#' Create boxplot standardize with IMPACT style
#'
#' @param .data:  data that contains the statistical result to build boxplots
#' @param x: element of .data that contains the different values of the categorical data
#' @param name.y: name of value calculated
#' @param median: element of .data containing the median values
#' @param first_quantile: element of .data containing lower hinges correspond to the first quartile
#' @param third_quantile: element of .data containing upper hinges correspond to the third quartile
#' @param whisker_min: element of .data containing the value of the lower whisher. Usually calculated as 1.5*IQR smallest value from the hinge
#' @param whisker_max: element of .data containing the value of the upper whisher. Usually calculated as 1.5*IQR largest value from the hinge
#' @param outlier_min (optional): element of .data containing the most extreme value beyond the lower whisper.
#' @param outlier_max (optional): element of .data containing the most extreme value beyond the upper whisper.
#' @param sens.boxplot (optional): if sens.boxplot = "vertical" (default) boxplots are build with vertical cartesian coordinates. If sens.boxplot="horizontal" flip cartesian coordinates so that vertical becomes horizontal
#' @details Create a plot with one or multiple boxplot standardize with IMPACT colors, fonts, ... for the same numerical variable
#' @return a ggplot object contaning a boxplot
#' @export
grouped_regression_impact <- function(.data, x, subset.x, y, weight){

  x <- enquo(x)
  subset.x <- enquo(subset.x)
  y <- enquo(y)
  weight <- enquo(weight)

  # if(!is.logical(add.regression.line)){
  #   stop("Parameter add.regression.line is logical. Please enter a valid value")
  # }

  # Check and return message if empty evironnement
  stop_msg <- error_message_empty_env_regression(x, y)
  if(stop_msg != ""){
    stop(paste0("The variable(s) following does not exist in .data: ",stop_msg))
  }
  #No plot if x and y is only NA
  check_contains_only_NA(x,.data)
  check_contains_only_NA(subset.x,.data)
  check_contains_only_NA(y,.data)

  if (rlang::quo_is_null(weight)) {
    warning("Could not find the weight column. No error bars will be added to the barchart")
  }

  # Draw plot
  theplot <- ggplot(.data, aes(x = !!x, y = !!y)) + theme_impact()

  if (rlang::quo_is_null(weight)) {
    warning("Could not find the weight column. No weights will be add to the plot")
  }
  else{
    theplot <- theplot + geom_point_impact(aes(size = !!weight, colour = as.factor(!!subset.x)) )  + labs(colour = rlang::get_expr(subset.x)) +
      scale_color_reach_categorical(n=nrow(dplyr::distinct(.data,!!subset.x)),name="")
  }

  #To add later to add a line or function to draw the regression with error
  # if(add.regression.line == TRUE){
  #   theplot <- theplot + geom_smooth_impact()
  #
  # }



  return(theplot)

}

#' Use geom_bar function with pre_fill arguments
#'
#' @details stat and position arguments are predifined as "identity" and "dodge"
#' @return geom_bar function pre-fill
#' @export
geom_point_impact <- purrr::partial(ggplot2::geom_point)


#' Use geom_bar function with pre_fill arguments
#'
#' @details stat and position arguments are predifined as "identity" and "dodge"
#' @return geom_bar function pre-fill
#' @export
geom_smooth_impact <- purrr::partial(ggplot2::geom_smooth, method = lm )
