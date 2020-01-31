#####################################################
#################### Pre-processed Data #################### 
#####################################################

# This file contains the server code that does the additional 
# cleaning for the raw data and the code that presents the pre-processed data
# as a data table. 

imported <- eventReactive(input$pre_process_text, {
  ############################
  if (input$import_from == "Spotify/Genius"){
    cleaned <- raw_data()[[1]] %>% clean_for_app()
  }
    
  else if (input$import_from == "Twitter"){
    cleaned <- raw_data() 
    
    # fixes the weird apostrophes
    # cleaned$text <- gsub(intToUtf8(8217), "'", cleaned$text, perl = TRUE)
    
    # converts the emojis to their description
    cleaned$text <- emoji_to_words(cleaned$text)

    # remove links
    cleaned$text <- gsub("http.+? ", "", cleaned$text, perl = TRUE)
    cleaned$text <- gsub("http.+", "", cleaned$text, perl = TRUE)
      
    # remove line breaks 
    cleaned$text <- gsub("\\n", " ", cleaned$text, perl = TRUE)
      
    # remove hashtags
    if (input$remove_hash == TRUE){
      cleaned$text <- textclean::replace_hash(cleaned$text)
    }
    
    cleaned$text <- textclean::replace_word_elongation(cleaned$text)
    
    # remove username mentions
    if (input$remove_user == TRUE){
      cleaned$text <- gsub("@\\w+", "", cleaned$text)
    }
    
    cleaned <- cleaned %>% clean_for_app()
    cleaned
  }
    
  else if (input$import_from == "The Guardian Articles"){
      
    cleaned <- raw_data() 
    
    cleaned$text <- gsub("<figcaption.+?</figcaption>|Related.+?</aside>", "", cleaned$text, perl = TRUE)
    
    cleaned <- clean_for_app(cleaned)

    cleaned$id <- iconv(cleaned$id, from = "UTF-8", to = "ASCII//TRANSLIT")
      
    cleaned <- cleaned %>%
      mutate(id = str_replace_all(id, "<.+?>", ""))
      
    # Fix the pound sign becoming ?
    cleaned$text <- gsub("[?](\\d+)", "£\\1", cleaned$text, perl = TRUE)
    
    while(sum(grepl("<.+?>", cleaned$text)) > 0){
      cleaned$text <- trimws(gsub("<.+?>|_", "", cleaned$text, perl = TRUE))
    }
    
    cleaned$text <- gsub("Join the debate.+", "", cleaned$text, perl = TRUE)
  }
  
  else if(input$import_from == "Reddit"){
    
    cleaned <- raw_data() %>% clean_for_app()
    cleaned$text <- gsub("[[(]http.+?[[)]", "", cleaned$text, perl = TRUE)
    cleaned$text <- textclean::replace_url(cleaned$text)
    df$text <- gsub("[**Extended Summary**].+", "", df$text)
    
  }
    
  else {
    # for imported text files
    cleaned <- raw_data() 
    
    #cleaned$text <- gsub('""', "", cleaned$text)
    cleaned$text <- gsub('" "', " ", cleaned$text, perl = TRUE)
    cleaned <- cleaned %>% clean_for_app()
  }
  
  cleaned
  
})

output$pre_processed_show <- DT::renderDataTable({
  imported()
}, filter = "bottom")

###################################
########## able to filter the processed data columns 
########## (used in prepped() orig_server.R, )
###################################
imported_filtered <- reactive({
  full_data <- imported()
  filtered_rows <- input$pre_processed_show_rows_all
  full_data[filtered_rows, ]
})


output$downloadData_pre_processed <- downloadHandler(
  filename = function() {
    paste("preprocessed", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(imported_filtered(), file, row.names = FALSE)
  }
)


