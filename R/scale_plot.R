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



visualisation_barchart_percent_nogroups_FS<-function(data){
  # some parameters: make sure it's a quarter of an a4 page wide (as in the factsheets):
  A4widthincm<-27.94
  smallFSplotwdith<-(A4widthincm/4)
  # set the font size:
  fonsize=8
  # how much plot height per bar?
  heightperbarcm<-0.6

  data$prop<-data$numbers*100

  # to make sure the labels, numbers and bars behave nicely and don't overlap, I've split the plot in three, then arranging them with a grid.
  #### only bars with no labels at all
  plot_bars <- function(data){
    ggplot(data, aes(x = reorder(dependent.var.value,prop), y = prop, width=0.5)) +
      geom_bar(stat = "identity", fill= reach_style_color_red(),position = position_nudge(y = 5,x=0))+theme_tufte()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.length = unit(0, "mm"))+
      coord_flip()+
      theme(plot.margin = unit(c(0,0,0,0), "cm"))+
      ylim(c(0,100))

  }

  # font size units different in theme() and in geom_text(): factor 1/0.35
  # only labels with no bars at all
  plot_labels<-function(data){
    ggplot(data, aes(x = reorder(dependent.var.value,prop), y = prop, width=0.5)) +
      # geom_bar(stat = "identity", fill= reach_style_color_red(),position = position_nudge(y = 5,x=0))+
      theme_tufte()+
      theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(),axis.title.x=element_blank(),text =element_text(family="Arial Narrow"))+
      theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),text =element_text(family="Arial Narrow"))+
      theme(text=element_text(family="Arial Narrow"))+
      coord_flip()+
      theme(axis.text.y =
              element_text(size  = fonsize,
                           angle = 0,
                           hjust = 0,
                           vjust = 0.5,
                           colour = "black",
                           family = "Arial Narrow"),
            axis.ticks.length = unit(0, "mm"))+
      theme(plot.margin = unit(c(0,0,0,0), "cm"))

  }
  # only numbers with none of the other stuff
  plot_numbers<-function(data){
    ggplot(data, aes(x = reorder(dependent.var.value,prop), y = prop, width=0.5)) +
      # geom_bar(stat = "identity", fill= reach_style_color_red(),position = position_nudge(y = 5,x=0))+
      theme_tufte()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.length = unit(0, "mm"))+
      geom_text(aes(label=paste0(round(prop),"%")),size = fonsize * 0.352777,family="Arial Narrow",position = position_stack(vjust = 0),hjust=0)+
      coord_flip()+
      theme(plot.margin = unit(c(0,0,0,0), "cm"))
  }

  fullplot<-grid.arrange(plot_labels(data),
                         plot_numbers(data),
                         plot_bars(data), ncol=3,widths=smallFSplotwdith*c(0.4,0.1,0.5))

  # ggsave(file=filename, plot=fullplot,width =smallFSplotwdith, height=heightperbarcm*length(unique(data$dependent.var.value)),units = "cm",device = "jpg",limitsize = F)

  hg_vis<-list(ggplot=fullplot,
               ggsave_parameters=list(width =smallFSplotwdith, height=heightperbarcm*length(unique(data$dependent.var.value)),units = "cm",device = "jpg",limitsize = F)
  )
  class(hg_vis)<-"hypegrammar_visualisation"
  return(hg_vis)
  return(fullplot)
}







