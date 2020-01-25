#####################################################
#################### Word Tree ###################### 
#####################################################

# This file contains the server code used to create the word tree plot from googleVis. 

output$shinytest <- renderGvis({
  input$create_tree
  arts <- merged() %>% unnest_tokens(text, text, token = "sentences")
  isolate(gvisWordTree(arts, textvar = "text",
                       options = list(fontName = "Times-Roman",
                                      wordtree = sprintf("{word: '%s', type: '%s'}",
                                                         input$firstword, input$tree_type),
                                      height = "1000px", width = "800px"
                       )
  ))
})
