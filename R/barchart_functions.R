#' Create a barchart for average
#'
#' @param .data
#' @param independent.var.value
#' @param result_average: data.frame of two column where the first is the values of the independent var and the second column is the average associated to the indepedent variable value
#' @param result_min optional: 
#' @param result_max optional:
#' @param save.barchart optional:
#' @param  
#' @param 
#' @param 
#' @param
#' @param resultat_avarage: data.frame of two column where the first is the values of the independent var and the second column is the average associated to the indepedent variable value
#' @details 
#' @return 
#' @examples
#' @export
#'
barchart_average<-function(.data,independent.var.value,result_average,result_min=NULL,result_max=NULL, save.barchart=FALSE,...){
  
  independent.var.value<-enquo(independent.var.value)
  result_average<-enquo(result_average)
  result_min<-enquo(result_min)
  result_max<-enquo(result_max)
 
  theplot <-  ggplot(.data, aes(x = !!independent.var.value , y = !! result_average ))+geom_bar(stat = "identity")+
    theme_tufte()+xlab("")+ylab(get_expr(result_average))+theme(text=element_text(family="Arial Narrow")
    )

  if (quo_is_null(result_min) | quo_is_null(result_max)) {
    warning("Could not find the min or max column. None Error bars will be added to the barchart")
  }
  else{
  
  # Add error bar to the plot
  theplot <- theplot + geom_errorbar( aes(x=!!independent.var.value,
                                          ymin=as.numeric(!!result_min),
                                          ymax=as.numeric(!!result_max)),
                                      stat='identity',
                                      width=.1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
  }

  if(save.barchart==TRUE){
    filename <-"Barchart_with_average"
    ggsave(filename,device="jpeg",...)
  }
  
  return(theplot)
}


#' Create a grouped barchart for percentage
#'
#' @param .data
#' @param independent.var.value
#' @param dependent.var.value
#' @param result_percent: data.frame of two column where the first is the values of the independent var and the second column is the average associated to the indepedent variable value
#' @param result_min optional: 
#' @param result_max optional:
#' @param save.file optional:
#' @param ... Other arguments passed on to the ggsave function   
#' @details 
#' @return 
#' @examples
#' @export
#'
grouped_barchart_percent <- function(.data,independent.var.value,dependent.var.value,result_percent, result_min=NULL, result_max=NULL, save.barchart=FALSE, ... ){
  
  independent.var.value<-enquo(independent.var.value)
  dependent.var.value<-enquo(dependent.var.value)
  result_average<-enquo(result_percent)
  result_min <- enquo(result_min)
  result_max <- enquo(result_max)
  
  theplot<-ggplot(.data,aes(x=!!independent.var.value,y=!!result_percent,fill=!!dependent.var.value))+
    geom_bar(stat = "identity",position='dodge')+theme_tufte()+xlab(NULL)+ylab(NULL)+theme(text=element_text(family="Arial Narrow"))+
    scale_fill_reach_categorical(n=nrow(distinct(.data,!!dependent.var.value)),name="")+
    scale_y_continuous(limits = c(0,100),labels = scales::percent_format(scale=1,accuracy = 0.01))
  
  if (quo_is_null(result_min) | quo_is_null(result_max)) {
    warning("Could not find the min or max column. None Error bars will be added to the barchart")
  }
  else{
  
    theplot <- theplot + geom_errorbar( aes(x=!!independent.var.value,
                                            ymin=as.numeric(!!result_min),
                                            ymax=as.numeric(!!result_max)),
                                        position=position_dodge(width=0.9),
                                        stat='identity',width=.1)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  if(save.barchart==TRUE){
    filename <-"Grouped_barchart_percentages"
    ggsave(filename,device="jpeg",...)
  }
  
  return(theplot)
}




































