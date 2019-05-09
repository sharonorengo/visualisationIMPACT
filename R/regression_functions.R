#' Create a barchart for average
#'
#' @param .data: data that contains the result for the barchart (percents or averages)
#' @param x: element of .data that contains the different values of the categorical data
#' @param y: element of .data containing for x element the y coordinates
#' @param infimum_error (optional): element of .data containing value of the lower limit for the error bars
#' @param supremum_error (optional): element of .data containing value of the upper limit for the error bars
#' @param sens.barchart (optional): if sens.boxplot = "vertical" (default) boxplots are build with vertical cartesian coordinates. If sens.boxplot="horizontal" flip cartesian coordinates so that vertical becomes horizontal
#' @param percent (optional): Default value is FALSE. If TRUE, y values are written as percentages
#' @details
#' @return a ggplot object
#' @examples
#' @export
