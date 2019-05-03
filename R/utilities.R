#' Add statistics values on boxplot
#'
#' @param theplot: ggplot
#' @param x: element of .data that contains the different values of the categorical data
#' @param whisker_min: element of .data containing the value of the lower whisher. Usually calculated as 1.5*IQR smallest value from the hinge
#' @param whisker_max: element of .data containing the value of the upper whisher. Usually calculated as 1.5*IQR largest value from the hinge
#' @param median: element of .data containing the median values
#' @return a ggplot object
add_stat_to_boxplot <- function(theplot, x, whisker_min, whisker_max, median){
  if(!is_quosure(x) | !is_quosure(whisker_min) | !is_quosure(whisker_max) | !is_quosure(median)){
    stop("x, y or group is not a quosure expression")
  }
  theplot <- theplot + geom_text(aes(x=!!x, y = !!whisker_min, label = format(round(min), nsmall=0)),size = 3,vjust = 1, hjust = 0, position = position_dodge(width=0.9))+
    geom_text(aes(x=!!x, y = !!whisker_max, label = format(round(max), nsmall=0)),size = 3,vjust = 0, hjust = 0, position = position_dodge(width=0.9)) +
    geom_text(aes(x=!!x, y = !!median, label = format(round(median), nsmall=0)),size = 3,vjust = 1, hjust=0.5, position = position_dodge(width=0.9))

  return(theplot)

}

#' Create a barchart for average
#'
#' @param theplot: ggplot
#' @param x: element of .data that contains the different values of the categorical data
#' @param y:
#' @param type.boxplot
#' @param group
#' @return a ggplot object
add_outlier_boxplot <- function(theplot, x, y, type.boxplot, group = group ){
  if(type.boxplot != "grouped" & type.boxplot != "ungrouped" ){
    stop("Type of boxplot not valide. Please enter 'grouped' or 'ungrouped' ")
  }

  if(!is_quosure(x) | !is_quosure(y) | !is_quosure(group)){
    stop("x, y or group is not a quosure expression")
  }

  if(type.boxplot == "grouped"){
    theplot <- theplot + geom_point(aes(x = !!x, y= !!y, group = !!group), color= reach_style_color_red(), position=position_dodge(width = 0.9))
  }
  else{
    theplot <- theplot + geom_point(aes(x = !!x, y=!!y), color= reach_style_color_red())

  }
  return(theplot)
}



#' Error bar standardize
#'
#' @param plot_without_errorbar:
#' @param measure:
#' @param lower_limit:
#' @param upper_limit:
#' @param text_angle
#' @return a ggplot object

errorbar_impact <- function( plot_without_errorbar, measure, lower_limit, upper_limit, text_angle){

  errorbar <- plot_without_errorbar + geom_errorbar( aes( x= !!measure,
                                                          ymin = as.numeric(!!lower_limit),
                                                          ymax = as.numeric(!!upper_limit),width=.1)
  ) +
    theme(axis.text.x = element_text(angle = text_angle, hjust = 1,vjust=0.5))


  return(errorbar)
}


#' Add percent format to a ggplot
#'
#' @param theplot: ggplot to which add percent format
#' @return a ggplot object
#'
add_percent_format <- function(theplot){
  # theplot <- theplot + scale_y_continuous(limits = c(0,1),labels = scales::percent_format())

  theplot <- theplot + scale_y_continuous(limits = c(0,100),labels = scales::percent_format(scale=1,accuracy = 0.01))
  return(theplot)
}
