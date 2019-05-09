#' Create a barchart for average
#'
#' @param .data: dataframe that contains the result for the barchart
#' @param independent.var.value: column of the dataframe .data thta contains the different values of the categorical data
#' @param max_nbr_var: integer for the maximum number of variable that fit on a vertical graph
#' @param size_max_label integer for the maximum number of character of a label
#' @details Searches for ....
#' @return a ggplot object
#' @examples  ...
#' @export
sens_barchart <- function(.data, independent.var, max_nbr_var, size_max_label ){

  independent.var <- enquo(independent.var)

  ##Checks
  if( check_empty_env(independent.var) == TRUE){ stop("The expression of the parameter independent.var does not exist in .data") }
  if(type_of(max_nbr_var) != "double"){ stop("Please enter a valid value to max_nbr_var parameter (integer)")}
  if( as.integer(max_nbr_var) != max_nbr_var){ stop("Please enter a valid number for max_nbr_var parameter")}
  if(type_of(size_max_label) != "double"){ stop("Please enter a valid value to size_max_label parameter (integer)")}
  if( as.integer(size_max_label) != size_max_label){ stop("Please enter a valid number for size_max_label parameter")}

  independent.var.value <- dplyr::select(.data, !!independent.var)
  nbre_bars <- nrow(independent.var.value)

  name_var <- names(independent.var.value)
  list_size_char_label <- nchar(levels(independent.var.value[[name_var]]))  ## to change

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
