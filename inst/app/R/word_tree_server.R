#####################################################
#################### Word Tree ###################### 
#####################################################

# This file contains the server code used to create the word tree plot from googleVis. 

merge_choices_wt <- reactive({
  raw <- raw_data()
  c("", names(raw)[names(raw) != "text"])
})

filter_choices <- reactive({
  if(isTruthy(input$merge_id_grps_wt)){
    raw <- raw_data()
    var <- input$merge_id_grps_wt
    unique(raw[[var]])
  }
})

observe({
  merge_choices_wt()
  isolate(
  updateSelectizeInput(session, "merge_id_grps_wt", selected = "",
                         choices = merge_choices_wt(), server = TRUE)
  )
})

observe({
  filter_choices()
  isolate(
    updateSelectizeInput(session, "filter_wt", 
                         choices = filter_choices(), server = TRUE)
  )
})

merged_wt <- reactive({
  input$create_tree
  isolate(
  merge_id(x = imported(), source = input$import_from, groups = input$merge_id_grps_wt, 
           filter = input$filter_wt)
  )
})


output$shinytest <- googleVis::renderGvis({
  input$create_tree
  arts <- merged_wt() %>% tidytext::unnest_tokens(text, text, token = "sentences")
  isolate(googleVis::gvisWordTree(arts, textvar = "text",
                       options = list(fontName = "Times-Roman",
                                      wordtree = sprintf("{word: '%s', type: '%s'}",
                                                         input$firstword, input$tree_type),
                                      height = "1000px", width = "800px"
                       )
  ))
})
