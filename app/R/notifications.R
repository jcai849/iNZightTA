# # If lemmatisation selected
# observeEvent(input$prep_button, {
#   if (input$lemmatise == TRUE)
#     # Save the ID for removal later
#   {
#     showNotification("Lemmatisation takes a while...", duration = NULL, type = "message")
#   }
# })


# If lemmatisation selected
observeEvent(input$gather_data, {
  if (input$lemmatise == TRUE)
  {
    showNotification("Please wait... Gathering data...", duration = 3, type = "warning")
  }
})

# For web importing 
observeEvent(input$prep_button, {
  if (input$lemmatise == TRUE)
  {
    showNotification("Lemmatisation may take a while...", duration = 4, type = "warning")
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


