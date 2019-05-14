#' IMPACT ggplot theme
#' @details works like ?theme_minimal
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

