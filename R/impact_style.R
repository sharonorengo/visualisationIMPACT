#' IMPACT ggplot theme
#' @details works like ?theme_minimal
#' @return
#' @examples
theme_impact <- function(){
  family_font <- windowsFonts(Times=windowsFont("Arial Narrow"))
  style <- theme(text=element_text(family=family_font$Times, colour='black'), axis.text.x = element_text(angle=30))
  style <- style + ggthemes::theme_tufte() + theme(plot.title = element_text(family = family_font$Times,
                                                   colour = reach_style_color_darkgrey(),
                                                   size = 18,
                                                   face = 'bold',
                                                   hjust = 0.5  ))


  return(style)
}


theme_bar <- function(){

  style <- theme_tufte()+ theme(axis.title.x=element_blank(),
                                axis.text.x=element_blank(),
                                axis.ticks.x=element_blank(),
                                axis.title.y=element_blank(),
                                axis.text.y=element_blank(),
                                axis.ticks.y=element_blank(),
                                axis.ticks.length = unit(0, "mm")) + theme(plot.margin = unit(c(0,0,0,0), "cm"))


  return(style)

}



theme_labels <- function(fonsize){
  family_font <- windowsFonts(Times=windowsFont("Arial Narrow"))

  style <- theme_tufte() + theme(axis.text.x = element_blank(),
                                axis.ticks.x=element_blank(),
                                axis.title.x=element_blank(),
                                text =element_text(family=family_font$Times)) +
                            theme(axis.title.y=element_blank(),
                                  axis.ticks.y=element_blank(),
                                  text =element_text(family=family_font$Times)) +
                            theme(text=element_text(family=family_font$Times)) +
                            theme(axis.text.y = element_text(size  = fonsize, angle = 0,
                                    hjust = 0,
                                    vjust = 0.5,
                                    colour = "black",
                                    family = family_font$Times),
                                    axis.ticks.length = unit(0, "mm"))+
                            theme(plot.margin = unit(c(0,0,0,0), "cm"))

  return(style)

}


theme_numbers <- function(){
  family_font <- windowsFonts(Times=windowsFont("Arial Narrow"))

  style <- theme_tufte() + theme(axis.title.x=element_blank(),
                                axis.text.x=element_blank(),
                                axis.ticks.x=element_blank(),
                                axis.title.y=element_blank(),
                                axis.text.y=element_blank(),
                                axis.ticks.y=element_blank(),
                                axis.ticks.length = unit(0, "mm")) +
                            theme(text=element_text(family=family_font$Times)) +
                            theme(plot.margin = unit(c(0,0,0,0), "cm"))
  return(style)
}
