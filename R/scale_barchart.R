#' Create a barchart for average
#'
#' @param .data: dataframe that contains the result for the barchart
#' @param independent.var.value: column of the dataframe .data thta contains the different values of the categorical data
#' @param result_average: a column of the dataframe .data containing for each categorical value the average associated
#' @param result_min optional: a column of the dataframe .data containing for each categorical value the lower limit value for the error bars
#' @param result_max optional: a column of the dataframe .data containing for each categorical value the upper limit value for the error bars
#' @param resultat_avarage: data.frame of two column where the first is the values of the independent var and the second column is the average associated to the indepedent variable value
#' @details Searches for ....
#' @return a ggplot object
#' @examples  ...
#' @export
sens_barchart <- function(.data, independent.var){

  independent.var <- enquo(independent.var)

  #si nbre de var cat > ... alors en mode horizontale
  # si une bar
  #  number_of_bars<-(length(unique(summary.statistic$dependent.var.value)))
  # plotwidth<-5+(number_of_bars*1.5)

  independent.var.value <- dplyr::select(.data, !!independent.var)
  nbre_bars <- nrow(independent.var.value)

  name_var <- names(independent.var.value)
  list_size_char_label <- nchar(levels(independent.var.value[[name_var]]))  ## to change

  list_logical_size <- lapply(list_size_char_label, function(x){if(x > 8) return(TRUE) else return(FALSE)} ) ##nbre de caractere difini en fonction taille output

  if(nbre_bars > 10 || TRUE %in% list_logical_size ){
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
