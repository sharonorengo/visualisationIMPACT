#' Function to decide the orientation of the plit in function of the data used
#' @param .data: dataframe that contains the result for the barchart
#' @param x.label: column of the dataframe .data that contains the different values of the categorical data
#' @param max_nbr_var: integer for the maximum number of variable that fit on a vertical graph
#' @param size_max_label integer for the maximum number of character of a label that fit on a vertical graph
#' @return a character string "vertical" or "horizontal" as the recommended orientation for the plot
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


  label_length<-nchar(unique(as.character(name_var)))
  too_long_labels<- any(  label_length > size_max_label)
  too_many_bars <- nbre_bars > max_nbr_var
  horizontal <- too_long_labels | too_many_bars

  orientation <- ifelse(horizontal,"horizontal" ,"vertical")

  return(orientation)

}

