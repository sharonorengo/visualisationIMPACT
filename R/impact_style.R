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
theme_impact <- function(){
  family_font <- windowsFonts(Times=windowsFont("Arial Narrow"))
  style <- theme(text=element_text(family=family_font$Times, colour='black'))
  style <- style + ggthemes::theme_tufte() + theme(plot.title = element_text(family = family_font$Times,
                                                   colour = reach_style_color_darkgrey(),
                                                   size = 18,
                                                   face = 'bold',
                                                   hjust = 0.5  ))


  return(style)
}



