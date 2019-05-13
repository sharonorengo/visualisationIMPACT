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
#' @return a ggplot object
#' @examples
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
  if(percent == TRUE & TRUE %in% (eval_tidy(y,.data) > scale.percent) ){
    stop("One of the percentages given is greater than 100 %. Please enter valid numbers.")
  }
  #Scale percentages =can be either 1 or 100
  if(scale.percent != 1 & scale.percent != 100){
    stop("Parameter scale.percent is not value. Has to be egal to 1 or 100")
  }

  angle <- 90
  theplot <-  ggplot(.data, aes(x = !!x , y = (!!y)*scale.percent )) + geom_bar_impact() + xlab("") + ylab(rlang::get_expr(y)) + theme_impact()

  # +geom_bar(stat = "identity")

  if(sens.barchart == "horizontal"){
    theplot <- theplot + coord_flip()
    angle <- 0
  }

  if (rlang::quo_is_null(infimum_error) | rlang::quo_is_null(supremum_error)) {
    warning("Could not find the min or max column. No error bars will be added to the barchart")
  }
  else{
    # Add error bar to the plot
    theplot <- errorbar_impact(theplot, measure = x,
                               lower_limit = infimum_error,
                               upper_limit = supremum_error,
                               text_angle = angle)
  }

  if(percent == TRUE){
    theplot <- add_percent_format(theplot, scale.percent)
  }


  return(theplot)

}



#' Create a grouped barchart
#'
#' @param .data: data that contains the result for the barchart (percents or averages)
#' @param x: column name (without quotes) of .data that contains the different values of the categorical data
#' @param y: column name (without quotes) .data containing for x element the y coordinates
#' @param result_percent: data.frame of two column where the first is the values of the independent var and the second column is the average associated to the indepedent variable value
#' @param infimum_error (optional): column name (without quotes) of .data containing value of the lower limit for the error bars
#' @param supremum_error (optional): column name (without quotes) of .data containing value of the upper limit for the error bars
#' @param sens.barchart (optional): if sens.barchart = "vertical" (default) boxplots are build with vertical cartesian coordinates. If sens.barchart="horizontal" flip cartesian coordinates so that vertical becomes horizontal
#' @param percent (optional): logical parameter. Default value is FALSE. If TRUE, y values are written as percentages
#' @param scale.percent (optional): Default value 1. A scaling factor: y,infimum_error and supremum_error will be multiply by scale.
#' @details
#' @return
#' @examples
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
   if(percent == TRUE & TRUE %in% (rlang::eval_tidy(y,.data) > scale.percent) ){
     stop("One of the percentages given is greater than 100 %. Please enter valid numbers.")
   }
   #Scale percentages =can be either 1 or 100
   if(scale.percent != 1 & scale.percent != 100){
     stop("Parameter scale.percent is not value. Has to be egal to 1 or 100")
   }
   # check out of bounds
   if(infimum_error < 0 | supremum_error > scale.percent)


   # Create ggplot
  theplot <- ggplot(.data, aes(x=!!x,y=(!!y)*scale.percent,fill=!!subset.x)) + geom_bar_impact() +
            theme_impact() + labs( x = NULL, y = NULL) + scale_fill_reach_categorical(n=nrow(dplyr::distinct(.data,!!subset.x)),name="")

   if (rlang::quo_is_null(infimum_error) | rlang::quo_is_null(supremum_error)) {
     warning("Could not find the min or max column. No error bars will be added to the barchart")
   }
   else{

     theplot <- theplot + geom_errorbar( aes(x=!!x,
                                             ymin=as.numeric(!!infimum_error)*scale.percent,
                                             ymax=as.numeric(!!supremum_error)*scale.percent),
                                         position=position_dodge(width=0.9),
                                         stat='identity',width=.1) #+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
   }

   if(percent == TRUE){
     theplot <- add_percent_format(theplot, scale.percent)
   }

   if(sens.barchart=="horizontal"){
      theplot <- theplot + coord_flip()
   }

   return(theplot)
}





#  Cannot add ggproto objects together. Did you forget to add this object to a ggplot object?
# # geom_grouped_bar_impact <- function(.data){
#    fun <- purrr::partial(ggplot2::geom_bar, stat = "identity",position='dodge')
#    result <- fun()+theme_impact()+xlab(NULL)+ylab(NULL)
#    return(result)
# }
# geom_grouped_bar_impact <- function(.data, mapping, subset.x ){
#    fun <- purrr::partial(ggplot2::geom_bar, data =  .data, mapping = mapping, stat = "identity",position='dodge')
#    result <- fun()+theme_impact()+xlab(NULL)+ylab(NULL)
#    return(result)
# }




