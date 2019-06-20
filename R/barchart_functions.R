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
                            sens.barchart="vertical",
                            percent = FALSE, scale.percent = 1, size.plot = "smallFS" ){

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
  if(!is.logical(percent)){
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

  nbre_bar <- length(unique(rlang::eval_tidy(x,.data)))
  if(nbre_bar > 20){
    warning("Too many variables. It is not going to fit correclty into the plot.")
  }


  #No plot if y is NA
  check_contains_only_NA(x,.data)
  check_contains_only_NA(y,.data)

  theplot <-  ggplot(.data, aes(x = !!x , y = (!!y)*scale.percent ))

  plot_bars <- theplot + geom_bar_impact( fill = reach_style_color_red() )  + theme_impact() + theme_bar()

  plot_numbers <- add_stat_to_barchart(theplot, .data , x , y , supremum_error, scale.percent, percent) + theme_numbers()


  if (rlang::quo_is_null(infimum_error) | rlang::quo_is_null(supremum_error)) {
    warning("Could not find the min or max column. No error bars will be added to the barchart")
  }
  else{

    # Add error bar to the plot
    infimum_error_without_negative <- check_and_replace_negative_value(.data, infimum_error)
    supremum_error_without_negative <- check_and_replace_negative_value(.data,supremum_error)

    plot_bars <- plot_bars + geom_errorbar_impact(aes( x= !!x,
                                             ymin = as.numeric(infimum_error_without_negative),
                                             ymax = as.numeric(supremum_error_without_negative)))
  }

  if(percent == TRUE){
    plot_bars <- plot_bars + scale_y_percent_impact()
  }

  if(sens.barchart == "horizontal"){
    plot_bars <- plot_bars + coord_flip()
    plot_numbers <- plot_numbers + coord_flip()
    plot_labels <- theplot + coord_flip() + xlab("") + ylab(rlang::get_expr(y))+ theme_labels_horizontal(11)##taille de la police Arial Narrow

    fullplot<-grid.arrange(plot_labels,
                           plot_numbers,
                           plot_bars, ncol = 3, nrow = 1, widths=c(0.3,0.2,0.5))
  }
  else{
    plot_labels <- theplot + theme_labels_vertical(11) ##taille de la police Arial Narrow

    fullplot<-grid.arrange(plot_numbers,
                           plot_bars,
                           plot_labels, ncol = 1, nrow = 3, heights=c(0.1,0.8,0.1))
  }
  x.label.value <- dplyr::select(.data, !!x)
  x.label.value <- as.character(x.label.value[,1])
  length_labels <- nchar(x.label.value)
  max_length_label<-max(length_labels) # taille max des labels
  max_length_numbers <- attributes(plot_numbers)$length_max_numbers #cas ou n'existe pas, à renommer
  attributes(fullplot)$ggsave_parameters <- list(num_bar = nbre_bar, direction_plot = sens.barchart, max_length_label = max_length_label, max_length_numbers = max_length_numbers)

  return(fullplot)

}

# addline_format <- function(x,...){
#   num <- nchar(as.character(x))
#    gsub('\s','\n',x)
# }

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

   nbre_bar <- length(unique(rlang::eval_tidy(subset.x,.data))) * length(unique(rlang::eval_tidy(x,.data)))
  if(nbre_bar > 20){
    warning("Too many variables. It is not going to fit correclty into the plot.")
  }

   # Create ggplot
   theplot <- ggplot(.data, aes(x = !!x,y = (!!y)*scale.percent, fill = !!subset.x))


   plot_bars <- theplot + geom_bar_impact() + scale_fill_reach_categorical(n=nrow(dplyr::distinct(.data,!!subset.x)),name="") +
     theme_impact() + theme_bar()

   # Add value to the plot
   plot_numbers <- add_stat_to_barchart(theplot, .data , x , y , supremum_error, scale.percent, percent) + theme_numbers()


   # Add values
   if (rlang::quo_is_null(infimum_error) | rlang::quo_is_null(supremum_error)) {
     warning("Could not find the min or max column. No error bars will be added to the barchart")
   }
   else{
     # Add error bar to the plot
     infimum_error_without_negative <- check_and_replace_negative_value(.data, infimum_error)
     supremum_error_without_negative <- check_and_replace_negative_value(.data,supremum_error)

     plot_bars <- plot_bars + geom_errorbar_impact( aes(x=!!x,
                                                    ymin = as.numeric(infimum_error_without_negative) * scale.percent,
                                                    ymax = as.numeric(supremum_error_without_negative) * scale.percent ) )

   }

   if(percent == TRUE){
     plot_bars <- plot_bars + scale_y_percent_impact()
   }


   if(sens.barchart == "horizontal"){
     plot_bars <- plot_bars + coord_flip()
     plot_numbers <- plot_numbers + coord_flip()
     plot_labels <- theplot + coord_flip() + xlab("") + ylab(rlang::get_expr(y))+ theme_labels_horizontal(10) ##taille de la police Arial Narrow

     fullplot<-grid.arrange(plot_labels,
                            plot_numbers,
                            plot_bars, ncol = 3, nrow = 1, widths=c(0.3,0.2,0.5))
   }
   else{
     plot_labels <- theplot + xlab("") + ylab(rlang::get_expr(y)) + theme_labels_vertical(10) ##taille de la police Arial Narrow
     plot_bars <- plot_bars + theme(legend.position="top")

     fullplot<-grid.arrange(plot_bars,
                            plot_numbers,
                            plot_labels,
                            ncol = 1, nrow = 3, heights=c(0.8,0.1,0.1))
   }
   x.label.value <- dplyr::select(.data, !!x)
   x.label.value <- as.character(x.label.value[,1])
   length_labels <- nchar(x.label.value)
   max_length_label<-max(length_labels) # taille max des labels
   max_length_numbers <- attributes(plot_numbers)$length_max_numbers #cas ou n'existe pas, à renommer
   attributes(fullplot)$ggsave_parameters <- list(num_bar = nbre_bar, direction_plot = sens.barchart, max_length_label = max_length_label, max_length_numbers = max_length_numbers)

   return(fullplot)


}



 #' Use geom_bar function with pre_fill arguments
 #'
 #' @details stat and position arguments are predifined as "identity" and "dodge"
 #' @return geom_bar function pre-fill
 #' @export
 geom_bar_impact <- purrr::partial(ggplot2::geom_bar, stat = "identity",position='dodge', width = 0.7)








