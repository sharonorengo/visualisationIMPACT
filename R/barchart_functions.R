#' Create a barchart for average
#'
#' @param .data: data that contains the result for the barchart (percents or averages)
#' @param x: element of .data that contains the different values of the categorical data
#' @param y: element of .data containing for x element the y coordinates
#' @param infimum_error (optional): element of .data containing value of the lower limit for the error bars
#' @param supremum_error (optional): element of .data containing value of the upper limit for the error bars
#' @param sens.barchart (optional): if sens.boxplot = "vertical" (default) boxplots are build with vertical cartesian coordinates. If sens.boxplot="horizontal" flip cartesian coordinates so that vertical becomes horizontal
#' @param percent (optional): Default value is FALSE. If TRUE, y values are written as percentages
#' @details
#' @return a ggplot object
#' @examples
#' @export
barchart_impact <- function(.data, x , y,
                            infimum_error=NULL ,supremum_error=NULL,
                            sens.barchart="vertical", percent = FALSE){

  x<-enquo(x)
  y<-enquo(y)
  infimum_error <- enquo(infimum_error)
  supremum_error<-enquo(supremum_error)
  angle <- 90

  theplot <-  ggplot(.data, aes(x = !!x , y = !! y ))+geom_bar(stat = "identity")+
   xlab("")+ylab(rlang::get_expr(y))+theme_impact()

  if(sens.barchart == "horizontal"){
    theplot <- theplot + coord_flip()
    angle <- 0
  }

  if (rlang::quo_is_null(infimum_error) | rlang::quo_is_null(supremum_error)) {
    warning("Could not find the min or max column. None Error bars will be added to the barchart")
  }
  else{
    # Add error bar to the plot
    theplot <- errorbar_impact(theplot, measure = x,
                               lower_limit = infimum_error,
                               upper_limit = supremum_error,
                               text_angle = angle)
  }

  if(percent == TRUE){
    theplot <- add_percent_format(theplot)
  }

  return(theplot)

}


#' Create a grouped barchart
#'
#' @param .data
#' @param x
#' @param y
#' @param result_percent: data.frame of two column where the first is the values of the independent var and the second column is the average associated to the indepedent variable value
#' @param min optional:
#' @param max optional:
#' @param save.file optional:
#' @param ... Other arguments passed on to the ggsave function
#' @details
#' @return
#' @examples
#' @export
#'
 grouped_barchart_impact <- function(.data, x , subset.x , y,
                                     infimum_error=NULL, supremum_error=NULL, sens.barchart="vertical", percent = FALSE ){

   x<-enquo(x)
   subset.x<-enquo(subset.x)
   y<-enquo(y)
   infimum_error <- enquo(infimum_error)
   supremum_error <- enquo(supremum_error)

   theplot<-ggplot(.data,aes(x=!!x,y=!!y,fill=!!subset.x))+
     geom_bar(stat = "identity",position='dodge')+theme_impact()+xlab(NULL)+ylab(NULL)+
     scale_fill_reach_categorical(n=nrow(dplyr::distinct(.data,!!y)),name="")

   if (quo_is_null(infimum_error) | quo_is_null(supremum_error)) {
     warning("Could not find the min or max column. None Error bars will be added to the barchart")
   }
   else{

     theplot <- theplot + geom_errorbar( aes(x=!!x,
                                             ymin=as.numeric(!!infimum_error),
                                             ymax=as.numeric(!!supremum_error)),
                                         position=position_dodge(width=0.9),
                                         stat='identity',width=.1)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
   }


   if(percent == TRUE){
     theplot <- add_percent_format(theplot)
   }

   if(sens.barchart=="horizontal"){
      theplot <- theplot + coord_flip()
   }

   return(theplot)
}

