#####################################################
#################### Word Tree ###################### 
#####################################################

# This file contains the server code used to create the word tree plot from googleVis. 

output$shinytest <- googleVis::renderGvis({
  input$create_tree
  arts <- merged() %>% tidytext::unnest_tokens(text, text, token = "sentences")
  isolate(googleVis::gvisWordTree(arts, textvar = "text",
                       options = list(fontName = "Times-Roman",
                                      wordtree = sprintf("{word: '%s', type: '%s'}",
                                                         input$firstword, input$tree_type),
                                      height = "1000px", width = "800px"
                       )
  ))
})
