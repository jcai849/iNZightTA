# For web importing
observeEvent(input$gather_data, {
    showNotification("Please wait... Gathering data...", duration = 5, type = "warning")
})

# For processing - lemmatisation 
observeEvent(input$prep_button, {
  if (input$lemmatise == TRUE)
  {
    showNotification("Lemmatisation may take a while...", duration = 5, type = "warning")
  }
})

# For Key Sections

observe({
  if (input$what_vis == "Key Sections" && nrow(filtered()) >= 15000){
    showModal(modalDialog(
      "The text is quite long. Calculations will take a few minutes.",
      easyClose = TRUE,
      footer = NULL
    ))
  }
})


# For word Tree
observe({
  if (input$what_vis == "Word Tree"){
    showNotification("Connecting to Google API, please wait...", duration = 5, type = "warning")
  }
})

