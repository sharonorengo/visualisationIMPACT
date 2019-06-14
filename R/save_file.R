#' Save ggplot object with fixed size
#'
#' @param ggplot_object: a ggplot object to save
#' @param filename: File name containing an extension (.jpg, .pdf, ...)
#' @param path (optional): Set by default to the current folder
#' @param ... other arguments passed to ggplot2::ggsave()
#' @export
#'
save_graph_FS <- function(ggplot_object, filename, path = "./", ... ){

  # Check if ...
  ggplot_object <- theplot
  if(is.null(ggplot_object)){ #| !ggplot2::is.ggplot(ggplot_object) objet grid.arrange -> verifier la class
    stop("Please enter a valid value to ggplot_object parameter. It has to be a ggplot object.")
  }
  if(is.null(filename) | !is.character(filename)){
    stop("Please enter a valid value to filename parameter.")
  }

  if(!("ggsave_parameters" %in% names(attributes(ggplot_object)))){
    stop("can only save ggplot objects produced with visualisationIMPACT")
  }

  ggsave_parameters<-attributes(ggplot_object)$ggsave_parameters

  if(!("num_bar" %in% names(ggsave_parameters))){
    stop("Can not find number of element in plot")
  }
  if(!("direction_plot" %in% names(ggsave_parameters))){
    stop("Can not find orientation of the plot")
  }

  num_element <- attributes(ggplot_object)$ggsave_parameters$num_bar #bar ou boxplot
  direction_plot <- attributes(ggplot_object)$ggsave_parameters$direction_plot

  ## calculer le largeur et hauteur
  list_siyze <- set_size_output(type = "FS", num_bars, direction_plot)


  heightFS <- num_element*0.75 #en cm

  ## TO DO gerer cette partie
  # params_passed<-list(...)
  # TO DO !!! ... overwrite ggsave parameters if passed through ... ???

  ggsave_parameters <- list(width = widthFS, height = heightFS, plot = ggplot_object, filename = filename, path = path, units = "cm")

  do.call(ggsave, ggsave_parameters)

  return(invisible(ggplot_object))
}


#' Set height and width parameters for saving ggplot object
#'
#' @param type: type of the output ( Factsheet or report)
#' @param num_element: Numbers of elements on the plot (number of bars or boxplots)
#' @param direction_plot: Direction of the plot. Can be vertical or horizontal
#' @export
#'
set_size_output(type , num_element, direction_plot){
  if(direction_plot != "vertical" | direction_plot != "horizontal"){
    stop("issue")
  }
  if(type !="report" | type !="FS"){
    stop("issue")
  }

  if(type == "FS"){
    if(direction_plot == "vertical"){

    }
    else{

    }

  }
  if(type == "report"){
    if(direction_plot == "vertical"){

    }
    else{

    }
  }




  list_size(height = height, width = width)
  return(list_size)
}











