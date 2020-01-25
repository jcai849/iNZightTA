#####################################################
#################### Pre-processed Data #################### 
#####################################################

# This file contains the server code that does the additional 
# cleaning for the raw data and the code that presents the pre-processed data
# as a data table. 

imported <- reactive({
  ############################
  if (input$import_from == "Spotify/Genius"){
    cleaned <- raw_data()[[1]] %>% clean_for_app()
  }
    
  else if (input$import_from == "Twitter"){
    cleaned <- raw_data()
    
    cleaned$text <- gsub(intToUtf8(8217), "'", cleaned$text)
    # converts the emojis to their description
    cleaned$text <- emoji_to_words(cleaned$text)

    # remove links
    cleaned$text <- gsub("http.+? ", "", cleaned$text)
    cleaned$text <- gsub("http.+", "", cleaned$text)
      
    # remove line breaks 
    cleaned$text <- gsub("\\n", " ", cleaned$text)
      
    # remove hashtags
    cleaned$text <- textclean::replace_hash(cleaned$text)
      
    cleaned$text <- textclean::replace_word_elongation(cleaned$text)
      
    # remove username mentions
    cleaned$text <- gsub("@\\w+", "", cleaned$text)
    
    cleaned <- cleaned %>% clean_for_app()
    cleaned
  }
    
  else if (input$import_from == "The Guardian Articles"){
      
    cleaned <- raw_data() %>% clean_for_app()

    cleaned$id <- iconv(cleaned$id, from = "UTF-8", to = "ASCII//TRANSLIT")
      
    cleaned <- cleaned %>%
      mutate(
        id = str_replace_all(id, "<.+?>", ""))
      
    # Fix the pound sign becoming ?
    cleaned$text <- gsub("[?](\\d+)", "£\\1", cleaned$text)
  }
    
  else {
    # for imported text files
    cleaned <- raw_data() 
    
    #cleaned$text <- gsub('""', "", cleaned$text)
    cleaned$text <- gsub('" "', " ", cleaned$text)
    cleaned$text <- str_squish(cleaned$text)
    
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


