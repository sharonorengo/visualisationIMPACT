#' Check if quosure elements in parameters exist in the environment given by the user for barcharts
#'
#' @param x: element of .data that contains the different values of the categorical data
#' @param subset.x: element containing all the subset categories of x. Default value is NULL to use the function for simple barchart. If it is specified it is for grouped barchart
#' @param name.y: name of value calculated
#' @param median: element of .data containing the median values
#' @param first_quantile: element of .data containing lower hinges correspond to the first quartile
#' @param third_quantile: element of .data containing upper hinges correspond to the third quartile
#' @param whisker_min: element of .data containing the value of the lower whisher. Usually calculated as 1.5*IQR smallest value from the hinge
#' @param whisker_max: element of .data containing the value of the upper whisher. Usually calculated as 1.5*IQR largest value from the hinge
#' @param outlier_min : element of .data containing the most extreme value beyond the lower whisper.
#' @param outlier_max : element of .data containing the most extreme value beyond the upper whisper.
#' @details Check if the parameter exists in the environment for a boxplot
#' @return a string with the error message containing names of the paramaters that don't exist in environment for barchart
#' @examples
error_message_empty_env_boxplot <- function(x, subset.x=NULL, median, whisker_min, whisker_max, first_quantile, third_quantile, outlier_min, outlier_max){

  # Check and return message if empty evironnement
  stop_msg <- ""
  if( check_empty_env(x) == TRUE){ stop_msg <- paste0(stop_msg , rlang::get_expr(x)) }
  if( is.null(subset.x) == FALSE){ if(check_empty_env(subset.x) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(subset.x)) }}
  if( check_empty_env(median) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(median)) }
  if( check_empty_env(first_quantile) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(first_quantile)) }
  if( check_empty_env(third_quantile) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(third_quantile)) }
  if( check_empty_env(whisker_min) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(whisker_min)) }
  if( check_empty_env(whisker_max) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(whisker_max)) }
  if( check_empty_env(outlier_min) == TRUE & rlang::quo_is_null(outlier_min) == FALSE){ stop_msg <- paste(stop_msg , rlang::get_expr(outlier_min)) }
  if( check_empty_env(outlier_max) == TRUE & rlang::quo_is_null(outlier_max) == FALSE ){ stop_msg <- paste(stop_msg , rlang::get_expr(outlier_max)) }

  return(stop_msg)

}

#' Check if quosure elements in parameters exist in the environment given by the user for boxplots
#'
#' @param x: element of .data that contains the different values of the categorical data
#' @param subset.x: element containing all the subset categories of x. Default value is NULL to use the function for simple barchart. If it is specified it is for grouped barchart
#' @param infimum_error : column name (without quotes) of .data containing value of the lower limit for the error bars
#' @param supremum_error : column name (without quotes) of .data containing value of the upper limit for the error bars
#' @details Check if the parameter exists in the environment for a barchart
#' @return a string with the error message containing names of the paramaters that don't exist in environment for barchart
#' @examples
error_message_empty_env_barchart <- function( x, subset.x = NULL, y, infimum_error, supremum_error){

  # Check and return message if empty environment
  stop_msg <- ""
  if( check_empty_env(x) == TRUE){ stop_msg <- paste0(stop_msg , rlang::get_expr(x)) }
  if(is.null(subset.x) == FALSE){if( check_empty_env(subset.x) == TRUE){ stop_msg <- paste(stop_msg , rlang::get_expr(subset.x)) }}
  if( check_empty_env(y) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(y)) }
  if( check_empty_env(infimum_error) == TRUE & rlang::quo_is_null(infimum_error)!= TRUE){ stop_msg <- paste(stop_msg , rlang::get_expr(infimum_error)) }
  if( check_empty_env(supremum_error) == TRUE & rlang::quo_is_null(infimum_error)!= TRUE){ stop_msg <- paste(stop_msg , rlang::get_expr(supremum_error)) }

  return(stop_msg)

}

#' Check if quosure elements in parameters exist in the environment given by the user for boxplots
#'
#' @param x: element of .data that contains the x coordinates
#' @param y: element of .data that contains the y coordinates
#' @details Check if the parameter exists in the environment for a regression
#' @return a string with the error message containing names of the paramaters that don't exist in environment for regression
#' @examples
error_message_empty_env_regression <- function(x,y){
  # Check and return message if empty environment
  stop_msg <- ""
  if( check_empty_env(x) == TRUE){ stop_msg <- paste0(stop_msg , rlang::get_expr(x)) }
  if( check_empty_env(y) == TRUE ){ stop_msg <- paste(stop_msg , rlang::get_expr(y)) }

  return(stop_msg)
}



#' Check if the environment of a quoted expression is empty
#'
#' @param x: a quoted expression
#' @return logical value. TRUE if the environment is empty

check_empty_env <- function(x){
  if( rlang::is_reference(rlang::quo_get_env(x), rlang::empty_env()) == TRUE ){
    return(TRUE)
  }
  return(FALSE)
}

#' Check if the value in quoted expression are negative and if yes replace by zero
#' @parem .data: an environnement containing var
#' @param var: a quoted expression
#' @return logical value. TRUE if the environment is empty
check_and_replace_negative_value <- function(.data, var){

  if(all(is.na(rlang::eval_tidy(var,.data)))){
    stop(paste(var, "varible contains only NAs"))
  }

  newlist <-  lapply(rlang::eval_tidy(var, .data), function(x){
    if((is.null(x)) | (is.na(x))){x <- NA}else{
      if(x < 0) {x = 0}
      else {
        x = x
      }}})

  return(newlist)
}

#' Check the quoted expression contains only NA
#' @param .data: an environnement containing var
#' @param var: a quoted expression
check_contains_only_NA <- function(var,.data){
  if( all(is.na(rlang::eval_tidy(var,.data)))){
    stop("y variable contains only NA. Please enter a valid parameter.")
  }
}




