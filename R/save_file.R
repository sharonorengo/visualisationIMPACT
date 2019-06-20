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
  # filename <- "test9.jpg"
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
  max_length_label <- attributes(ggplot_object)$ggsave_parameters$max_length_label
  max_length_numbers <- attributes(ggplot_object)$ggsave_parameters$max_length_numbers
  ## calculer le largeur et hauteur
  list_size <- set_size_output(type = "FS", as.numeric(num_element), direction_plot, as.numeric(max_length_label), as.numeric(max_length_numbers))

  ## TO DO gerer cette partie
  # params_passed<-list(...)
  # TO DO !!! ... overwrite ggsave parameters if passed through ... ???

  ggsave_parameters <- list(width = list_size$width, height = list_size$height, plot = ggplot_object, filename = filename, path = path, units = "cm")

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
set_size_output <- function(type , num_element, direction_plot, max_length_label, max_length_numbers){
  if(direction_plot != "vertical" & direction_plot != "horizontal"){
    stop("issue 1")
  }
  if(type !="report" & type !="FS"){
    stop("issue 2")
  }

  widthA4 <- 21
  heightA4 <- 29.7

  if(type == "FS"){
    widthFS = (widthA4-3)/2
    heightFS = (heightA4-3.7)/4

    if(direction_plot == "vertical"){
      height = heightFS
      width = num_element*0.85


      if(width >= widthA4){
        warning("The optimal width of your plot is larger than width of A4 format. You should consider to remove some categories")
        ## warning ou stop
        # améliorer la suggestion
      }

    }
    else{ ## horizontal
      width = widthFS
      if(max_length_label >= 15){ ## 15 var pour un taille de 10
        should_be <- width*0.3*max_length_label/15
        width <- should_be / 0.3 ## pour horizontal c'est le pourcentage pour la partie label

      }
      if(max_length_numbers >= 8){
        should_be <- width*0.2*max_length_numbers/8
        width <- should_be /0.2
      }
      height = num_element*0.75
      if(height >= heightA4){
        warning("The optimal height of your plot is larger than height of A4 format. You should consider to remove some categories")
        ## warning ou stop
        # améliorer la suggestion
        # faire un plot avec des width plus petit ?
      }
      if(width >= widthA4){ ## sort un plot quand même
        warning("The width is larger than Aç format width. You should consider have smaller labels.")
      }

    }

  }

  # TO DO !!!
  if(type == "report"){
    if(direction_plot == "vertical"){

    }
    else{

    }


  }


  list_size <- list(height = height, width = width)
  return(list_size)
}











