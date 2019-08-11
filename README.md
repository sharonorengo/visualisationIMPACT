# visualisationIMPACT

## Install package and load fonts

``` r
# Install development version from GitHub:

install.packages("devtools")
devtools::install_github("sharonorengo/visualisationIMPACT")

#Load fonts
install.packages("extrafont")
extrafont::loadfonts(device="win")  

```

##  Orientation of your graph

The function *orientation_plot* helps you to choose what it is the best orientation for your graph: horizontal or vertical. 

You have the possibility to choose what is the maximun number of variable and maximum length of label that fit into a vertical graph. 

You can use: 
```{r, eval = F}
#For FS
senFS <- orientation_plot(.data, x.label ,max_nbr_var = 6,size_max_label = 12)

# For Report

sensReport <- orientation_plot(.data, x.label,max_nbr_var = 10,size_max_label = 20)


```


## Save graph
The function *save_graph* can only be used with a plot created by this package. 
You can choose the type of ouput: report or FS. Thsi function will save your plot in the optimal size. 
If the optimal size exceed the stardard format of a report or FS, it still save the plot but you should consider change the type of output (e.g report instead of factsheet), the orientation of the plot (e.g. horizontal instead of vertical) or plot less information. 


