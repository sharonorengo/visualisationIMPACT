#' Add statistics values on boxplot
#'
#' @param theplot: ggplot
#' @param x: element of .data that contains the different values of the categorical data
#' @param whisker_min: element of .data containing the value of the lower whisher. Usually calculated as 1.5*IQR smallest value from the hinge
#' @param whisker_max: element of .data containing the value of the upper whisher. Usually calculated as 1.5*IQR largest value from the hinge
#' @param median: element of .data containing the median values
#' @return a ggplot object
add_stat_to_boxplot <- function(theplot, x, whisker_min, whisker_max, median){
  if(!rlang::is_quosure(x)| !rlang::is_quosure(median)){
    stop("x or/and median is not a quosure expression")
  }
  theplot <- theplot + geom_text(aes(x=!!x, y = !!whisker_min, label = format(round(min), nsmall=0)),size = 3,vjust = 1, hjust = 0, position = position_dodge(width=0.9))+
    geom_text(aes(x=!!x, y = !!whisker_max, label = format(round(max), nsmall=0)),size = 3,vjust = 0, hjust = 0, position = position_dodge(width=0.9)) +
    geom_text(aes(x=!!x, y = !!median, label = format(round(median), nsmall=0)),size = 3,vjust = 1, hjust=0.5, position = position_dodge(width=0.9))

  return(theplot)

}

#' Add outliers to boxplots
#'
#' @param theplot: ggplot to which add a point
#' @param x: element that contains the x coordinates of the point to add
#' @param y: element that contains the y coordinates of the point to add
#' @param type.boxplot: can be either grouped or ungrouped to change the aesthetic mappings
#' @param group: element containing all the subset categories of x.
#' @return a ggplot object
add_outlier_boxplot <- function(theplot, x, y, type.boxplot, group = group ){
  if(type.boxplot != "grouped" & type.boxplot != "ungrouped" ){
    stop("Type of boxplot not valide. Please enter 'grouped' or 'ungrouped' ")
  }

  if(!rlang::is_quosure(x) | !rlang::is_quosure(y) | !rlang::is_quosure(group)){
    stop("x, y or group is not a quosure expression")
  }

  if(type.boxplot == "grouped"){
    theplot <- theplot + geom_point(aes(x = !!x, y= !!y, group = !!group), color= reach_style_color_red(), position = position_dodge(1))
  }
  else{
    theplot <- theplot + geom_point(aes(x = !!x, y=!!y), color= reach_style_color_red())

  }
  return(theplot)
}


#' Prefill scale_y_continuous function
#'
#' @details See more with ?scale_y_continuous
#' @export
scale_y_percent_impact <- purrr::partial(ggplot2::scale_y_continuous,
                                         limits = c(0,100),
                                         labels = scales::percent_format(scale=1,accuracy = 0.01))


#' Add the barchart values to the plot
#'
#' @param theplot: ggplot object to which add text
#' @param x: x coordinates
#' @param y: y coordinates
#' @param scale.percent : A scaling factor: y,infimum_error and supremum_error will be multiply by scale.
#' @param percent: logical variable to use percentage format or not
#' @details stat and position arguments are predifined as "identity" and "dodge"
#' @return a ggplot objet
#'
add_stat_to_barchart <- function(theplot, .data , x , y , supremum_error, scale.percent, percent){
  if(percent == TRUE){
    label.y = function(x){paste(round(x, digits = 2),"%")}
  }
  else{
    label.y = function(x){round(x, digits = 2)}
  }

  #Define y coordinates to add value of barchart on plot
  if(any(!is.na(rlang::eval_tidy(supremum_error,.data)*scale.percent)) |  rlang::quo_is_null(supremum_error) ){
    max_supremum_error <-  max(rlang::eval_tidy(supremum_error,.data)*scale.percent)
  }
  maximum_value_on_graph <- max( rlang::eval_tidy(y,.data)*scale.percent, max_supremum_error)

  # coordinate.y.text <- maximum_value_on_graph + (maximum_value_on_graph - min(rlang::eval_tidy(y,.data)*scale.percent))*0. ## to change !!!

  theplot <- theplot + geom_text(aes(x = !!x, y = maximum_value_on_graph,
                                     label = label.y(!!y*scale.percent),
                                     fontface=2), size = 5,
                                     position = position_dodge(width=0.7),
                                    colour = reach_style_color_darkgrey() )
  length_numbers <- nchar(eval_tidy(y, .data))
  if(percent == TRUE){
    length_numbers <- length_numbers + 1 #espace + signe pourcentage
  }
  attributes(theplot)$length_max_numbers <- max(length_numbers)
  return(theplot)
}



#' Use geom_bar function with pre_fill arguments
#'
#' @details stat and position arguments are predifined as "identity" and "dodge". See more with ?geom_bar
#' @return geom_bar function pre-fill
#' @export
geom_errorbar_impact <- purrr::partial(ggplot2::geom_errorbar,
                                       position=position_dodge(width=0.7),
                                       stat='identity',
                                       width=.1)




