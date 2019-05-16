#' Create a barchart for average
#'
#' @param .data: data that contains the result for the barchart (percents or averages)
#' @param x: column name (without quotes) of .data that contains the different values of the categorical data.
#' @param y: column name (without quotes) .data containing for x element the y coordinates
#' @param infimum_error (optional): column name (without quotes) of .data containing value of the lower limit for the error bars
#' @param supremum_error (optional): column name (without quotes) of .data containing value of the upper limit for the error bars
#' @param sens.barchart (optional): if sens.barchart = "vertical" (default) boxplots are build with vertical cartesian coordinates. If sens.barchart="horizontal" flip cartesian coordinates so that vertical becomes horizontal
#' @param percent (optional): logical parameter. Default value is FALSE. If TRUE, y values are written as percentages
#' @param scale.percent (optional): Default value 1. A scaling factor: y,infimum_error and supremum_error will be multiply by scale.
#' @return a ggplot object containing barchart
#' @export
barchart_impact <- function(.data, x , y,
                            infimum_error=NULL ,supremum_error=NULL,
                            sens.barchart="vertical", percent = FALSE, scale.percent = 1){

  x <- enquo(x)
  y<-enquo(y)
  infimum_error <- enquo(infimum_error)
  supremum_error<-enquo(supremum_error)

  # Check and return message if empty evironnement
  stop_msg <- error_message_empty_env_barchart( x,subset.x = NULL, y, infimum_error, supremum_error)
  if(stop_msg != ""){
    stop(paste0("The variable(s) following does not exist in .data: ",stop_msg))
  }

  if(sens.barchart != "vertical" & sens.barchart != "horizontal"){
    stop("Please enter a valid value to the parameter sens.barchart: 'vertical' or 'horizontal'")
  }
  if(percent != TRUE & percent != FALSE){
    stop("Please enter a valid value to the parameter percent: TRUE or FALSE")
  }
  # No percentages > 100
  if(percent == TRUE & TRUE %in% (rlang::eval_tidy(y,.data)*scale.percent > 100) ){
    stop("One of the percentages given is greater than 100 %. Please enter valid numbers.")
  }

  #Scale percentages can be either 1 or 100
  if(scale.percent != 1 & scale.percent != 100){
    stop("Parameter scale.percent is not value. Has to be egal to 1 or 100")
  }

  #No plot if y is NA
  check_contains_only_NA(x,.data)
  check_contains_only_NA(y,.data)

  angle <- 90
  theplot <-  ggplot(.data, aes(x = !!x , y = (!!y)*scale.percent )) + geom_bar_impact( fill = reach_style_color_red() ) +
                xlab("") + ylab(rlang::get_expr(y)) + theme_impact() + theme(axis.text.x = element_text(angle=30))

  theplot <- add_stat_to_barchart(theplot, .data , x , y , supremum_error, scale.percent, percent)

  if(sens.barchart == "horizontal"){
    theplot <- theplot + coord_flip() + theme(axis.text.x = element_text(angle=0))
    angle <- 0
  }

  if (rlang::quo_is_null(infimum_error) | rlang::quo_is_null(supremum_error)) {
    warning("Could not find the min or max column. No error bars will be added to the barchart")
  }
  else{

    # Add error bar to the plot
    infimum_error_without_negative <- check_and_replace_negative_value(.data, infimum_error)
    supremum_error_without_negative <- check_and_replace_negative_value(.data,supremum_error)

    theplot <- theplot + geom_errorbar_impact(aes( x= !!x,
                                             ymin = as.numeric(infimum_error_without_negative),
                                             ymax = as.numeric(supremum_error_without_negative)))
  }

  if(percent == TRUE){
    theplot <- theplot + scale_y_percent_impact()
  }


  return(theplot)

}



#' Create a grouped barchart
#'
#' @param .data: data that contains the result for the barchart (percents or averages)
#' @param x: column name (without quotes) of .data that contains the different values of the categorical data
#' @param subset.x: element containing all the subset categories of x
#' @param y: column name (without quotes) .data containing for x element the y coordinates
#' @param result_percent: data.frame of two column where the first is the values of the independent var and the second column is the average associated to the indepedent variable value
#' @param infimum_error (optional): column name (without quotes) of .data containing value of the lower limit for the error bars
#' @param supremum_error (optional): column name (without quotes) of .data containing value of the upper limit for the error bars
#' @param sens.barchart (optional): if sens.barchart = "vertical" (default) boxplots are build with vertical cartesian coordinates. If sens.barchart="horizontal" flip cartesian coordinates so that vertical becomes horizontal
#' @param percent (optional): logical parameter. Default value is FALSE. If TRUE, y values are written as percentages
#' @param scale.percent (optional): Default value 1. A scaling factor: y,infimum_error and supremum_error will be multiply by scale.
#' @return A ggplot object containing grouped barchart
#' @export
#'
 grouped_barchart_impact <- function(.data, x , subset.x , y,
                                     infimum_error=NULL, supremum_error=NULL, sens.barchart="vertical", percent = FALSE, scale.percent = 1 ){

   x <- enquo(x)
   subset.x <- enquo(subset.x)
   y <- enquo(y)
   infimum_error <- enquo(infimum_error)
   supremum_error <- enquo(supremum_error)

   # Check and return message if empty evironnement
   stop_msg <- error_message_empty_env_barchart( x, subset.x, y, infimum_error, supremum_error)
   if(stop_msg != ""){
     stop(paste0("The variable(s) following does not exist in .data: ",stop_msg))
   }
   if(sens.barchart != "vertical" & sens.barchart != "horizontal"){
     stop("Please enter a valid value to the parameter sens.barchart: 'vertical' or 'horizontal'")
   }
   if(percent != TRUE & percent != FALSE){
     stop("Please enter a valid value to the parameter percent: TRUE or FALSE")
   }
   # No percentages > 100
   if(percent == TRUE & TRUE %in% (rlang::eval_tidy(y,.data)*scale.percent > 100) ){
     stop("One of the percentages given is greater than 100 %. Please enter valid numbers.")
   }
   #Scale percentages =can be either 1 or 100
   if(scale.percent != 1 & scale.percent != 100){
     stop("Parameter scale.percent is not value. Has to be egal to 1 or 100")
   }
   #No plot if y is NA
   check_contains_only_NA(x,.data)
   check_contains_only_NA(y,.data)
   check_contains_only_NA(subset.x,.data)


   # Create ggplot
   theplot <- ggplot(.data, aes(x = !!x,y = (!!y)*scale.percent, fill = !!subset.x)) + geom_bar_impact() +
            theme_impact() + labs( x = NULL, y = NULL) + scale_fill_reach_categorical(n=nrow(dplyr::distinct(.data,!!subset.x)),name="")

    # Add value to the plot
   theplot <- add_stat_to_barchart(theplot, .data , x , y , supremum_error, scale.percent, percent)

   # Add values
   if (rlang::quo_is_null(infimum_error) | rlang::quo_is_null(supremum_error)) {
     warning("Could not find the min or max column. No error bars will be added to the barchart")
   }
   else{
     # Add error bar to the plot
     infimum_error_without_negative <- check_and_replace_negative_value(.data, infimum_error)
     supremum_error_without_negative <- check_and_replace_negative_value(.data,supremum_error)

     theplot <- theplot + geom_errorbar_impact( aes(x=!!x,
                                                    ymin = as.numeric(infimum_error_without_negative) * scale.percent,
                                                    ymax = as.numeric(supremum_error_without_negative) * scale.percent ))

   }

   if(percent == TRUE){
     theplot <- theplot + scale_y_percent_impact()
   }

   if(sens.barchart=="horizontal"){
      theplot <- theplot + coord_flip()
   }



   return(theplot)
}



 #' Use geom_bar function with pre_fill arguments
 #'
 #' @details stat and position arguments are predifined as "identity" and "dodge"
 #' @return geom_bar function pre-fill
 #' @export
 geom_bar_impact <- purrr::partial(ggplot2::geom_bar, stat = "identity",position='dodge')







