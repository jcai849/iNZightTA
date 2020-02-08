#####################################################
#################### Readability #################### 
#####################################################

# This file contains server code that produces the readability 
# plot (data for ggplot comes from insighted() in orig_server.R, 
# calls on function books_with_samples() in server_functions.R, 
# sample texts live in a separate R file called samples.R)

############################ Readability Plot

output$flesch_plot <- renderPlot({
  if(input$what_vis == "Readability"){
    global <- reactiveValues(selectedBar = NULL)
    
    observeEvent(eventExpr = input$plot1_click, {
      global$selectedBar <- insighted()$id[round(input$plot1_click$y)]
    })
    
    output$ex <- renderText({
      req(global$selectedBar)
      insighted()[insighted()$id == global$selectedBar, "excerpt"]
    })
    colors = ifelse(!insighted()$id %in% samples$id, "red", "white")
    
    ggplot2::ggplot(insighted(), ggplot2::aes(y = reorder(id, FK), x = FK, fill = FK)) +
      ggstance::geom_barh(stat = "identity", alpha = 0.8) +
      ggplot2::geom_text(aes(y = id, x = 0.5, label = id), color= colors,
                size=5, hjust = 0) +
      ggplot2::theme(legend.position="none") +
      ggplot2::theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      ggplot2::scale_x_continuous(expand=c(0,0)) +
      ggplot2::scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
      ggplot2::labs(y = "Grade Level", x = NULL,
           title = "Comparing Flesch-Kincaid readability scores") 
  }
})
