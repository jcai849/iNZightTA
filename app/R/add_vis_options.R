#####################################################
#################### Additional Visualization Options 
#####################################################

# This file containts the server code that allows users to have more 
# control over the aesthetic features of the plot.

##### Additional options for visualization (change text size, number of cols ...)
shinyjs::onclick("toggle_vis",
                 shinyjs::toggle(id = "vis", anim = TRUE))

output$add_vis_options <- renderUI({
  shinyjs::hidden(
    div(id = "vis",
        ##### controlling plot height
        sliderInput("plot_height", "Plot height",
                    min = 400, max = 2000,
                    value = 1000), 
        ##### controlling text size
        sliderInput("text_size", "Text size", 
                    min = 1, max = 30, value = 15),
        ##### controlling number of columns for facet wrap
        sliderInput("n_col_facet", "Number of columns for multi-panel plots", 
                    min = 1, max = 7, value = 5)
    )
  )
})