#' Create a barchart for average
#'
#' @param .data: dataframe that contains the result for the barchart
#' @param x.label: column of the dataframe .data that contains the different values of the categorical data
#' @param max_nbr_var: integer for the maximum number of variable that fit on a vertical graph
#' @param size_max_label integer for the maximum number of character of a label
#' @details
#' @return a character string "vertical" or "horizontal" as the recommended orientation of the barchart
#' @examples
#' @export
orientation_plot <- function(.data, x.label, max_nbr_var, size_max_label ){

  x.label <- enquo(x.label)

  ##Checks
  if( check_empty_env(x.label) == TRUE){ stop("The expression of the parameter x.label does not exist in .data") }
  if(class(max_nbr_var) != "numeric"){ stop("Please enter a valid value to max_nbr_var parameter (numeric)")}
  if( as.integer(max_nbr_var) != max_nbr_var){ stop("Please enter a valid number for max_nbr_var parameter")}
  if(class(size_max_label) != "numeric"){ stop("Please enter a valid value to size_max_label parameter (numeric)")}
  if( as.integer(size_max_label) != size_max_label){ stop("Please enter a valid number for size_max_label parameter")}

  max_nbr_var<-round(max_nbr_var)
  size_max_label<-round(size_max_label)

  x.label.value <- dplyr::select(.data, !!x.label)
  nbre_bars <- nrow(x.label.value)

  name_var <- names(x.label.value)
  list_size_char_label <- nchar(levels(x.label.value[[name_var]]))  ## to change

  list_logical_size <- lapply(list_size_char_label, function(x){if(x > size_max_label) return(TRUE) else return(FALSE)} ) ##nbre de caractere difini en fonction taille output

  if(nbre_bars > max_nbr_var || TRUE %in% list_logical_size ){
    #nbre doit dependre de la taille du output par exemple
    sens_barchart <- "horizontal"
  }
  else{
    sens_barchart <- "vertical"
  }
  return(sens_barchart)

}

size_window <- function(heigh, width){
  if(heigh < 8){
    nbre_var_max <- 4
  }
  #same for size character

  return(nbre_var_max)
}
