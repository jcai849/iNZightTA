
#####################################################
#################### Original Server #################### 
#####################################################

# This file containts mostly the server code from the original inzightta shiny app. 
# Added a button to download the data behind the visualization. Ongoing addition:
# data table below vis plot


prepped <- eventReactive(input$prep_button, {
  
  from_sw_file <- reactive({
    if (is.null(input$sw_file)) {""}
    else {readLines(input$sw_file$datapath)}
  })
  
  addl_stopwords <- reactive({
    input_sw <- unlist(strsplit(input$addl_stopwords, split = ","))
    c(input_sw, from_sw_file())
  })
  
  
  imported_filtered() %>%
    format_data(input$lemmatise, input$stopwords, input$sw_lexicon, addl_stopwords())
})

sectioned <- reactive({
  data <- prepped()
  if (isTruthy(input$section_by)){
    data <- data %>%
      section(input$section_by)}
  data})

filtered <- reactive({
  data <- sectioned()
  if (isTruthy(input$filter_var) &
      isTruthy(input$filter_pred)){
    # data <- data %>%
    #   dplyr::filter(!! dplyr::sym(input$filter_var) == input$filter_pred)}
    
    ###########################
    data <- data %>%
      dplyr::filter(str_detect(!! dplyr::sym(input$filter_var), input$filter_pred))
    ###########################
  }
  data
})

grouped <- reactive({
  data <- filtered()
  if (isTruthy(input$group_var)){
    data <- data %>%
      dplyr::group_by(!! dplyr::sym(input$group_var))}
  data})

output$table <- renderTable({
  filtered() %>% head(300)})

output$sw_lexicon <- renderUI(selectInput("sw_lexicon", "Select the Stopword Lexicon",
                                          stopwords::stopwords_getsources()))
output$vars_to_filter <- renderUI(selectInput("filter_var",
                                              "select which column to apply filtering to",
                                              c("", names(sectioned())) %||% c("")))

output$group_by <- renderUI(selectInput("group_var",
                                        "select which columns to group on",
                                        c("", names(filtered())) %||% c(""))
)
output$insight_options <- renderUI({
  switch(input$what_vis,
         "Term Frequency" = selectInput("vis_type",
                                        "Select how to Visualise it",
                                        list("Bar",
                                             "Word Cloud",
                                             "Page View",
                                             "Time Series",
                                             "Density",
                                             "Histogram")),
         "n-gram Frequency" = tagList(selectInput("vis_type",
                                                  "Select how to Visualise it",
                                                  list("Bar",
                                                       "Word Cloud",
                                                       "Page View",
                                                       "Time Series",
                                                       "Density",
                                                       "Histogram")),
                                      sliderInput("n_gram",
                                                  "n-gram count",
                                                  2, 8, 2)),
         "Key Words" = tagList(selectInput("vis_type",
                                           "Select how to Visualise it",
                                           list("Bar",
                                                "Word Cloud",
                                                "Page View",
                                                "Time Series",
                                                "Density",
                                                "Histogram")),
                               selectInput("summ_method",
                                           "Method of summary generation",
                                           list("TextRank", "LexRank"))),
         ##########################
         
         "Word Tree" = tagList(
           textInput(
             inputId = "firstword",
             label = "Input the root word."),
           
           selectizeInput("tree_type", "Tree Type",
                          c("suffix", "prefix", "double"),
                          selected = "suffix"),
           actionButton("create_tree", "Create Tree")
         ),
         
         # "Readability" = tagList(
         #   checkboxInput('groups', 'Group by chapter', value = FALSE, width = NULL)
         # ),
         ##########################
         "Term Sentiment" = tagList(selectInput("vis_type",
                                                "Select how to Visualise it",
                                                list("Page View",
                                                     "Word Cloud",
                                                     "Time Series",
                                                     "Bar",
                                                     "Density",
                                                     "Histogram")),
                                    selectInput("sent_lex",
                                                "Lexicon for Sentiment Dictionary",
                                                list("afinn", "bing",
                                                     "loughran", "nrc",
                                                     "nrc emotions", "loughran - all sentiments"))),
         "Moving Average Term Sentiment" = tagList(selectInput("vis_type",
                                                               "Select how to Visualise it",
                                                               list("Time Series",
                                                                    "Word Cloud",
                                                                    "Page View",
                                                                    "Bar",
                                                                    "Density",
                                                                    "Histogram")),
                                                   sliderInput("term_sent_lag",
                                                               "Lag Length for Calculation of Moving Average",
                                                               3,1000,250),
                                                   selectInput("sent_lex",
                                                               "Lexicon for Sentiment Dictionary",
                                                               list("afinn", "bing",
                                                                    "loughran", "nrc"))),
         "Aggregated Term Count" = tagList(selectInput("vis_type",
                                                       "Select how to Visualise it",
                                                       list("Bar",
                                                            "Word Cloud",
                                                            "Page View",
                                                            "Time Series",
                                                            "Density",
                                                            "Histogram")),
                                           selectInput("agg_var",
                                                       "Select which variable to aggregate on",
                                                       c("", names(grouped())) %||% c(""))),
         "Key Sections" = tagList(selectInput("vis_type",
                                              "Select how to Visualise it",
                                              list("Bar",
                                                   "Word Cloud",
                                                   "Page View",
                                                   "Time Series",
                                                   "Density",
                                                   "Histogram")),
                                  selectInput("summ_method",
                                              "Method of summary generation",
                                              list("TextRank", "LexRank")),
                                  selectInput("agg_var",
                                              "Select which variable to aggregate on",
                                              c("", names(grouped())) %||% c(""))),
         "Aggregated Sentiment" = tagList(selectInput("vis_type",
                                                      "Select how to Visualise it",
                                                      list("Page View",
                                                           "Word Cloud",
                                                           "Time Series",
                                                           "Bar",
                                                           "Density",
                                                           "Histogram")),
                                          selectInput("sent_lex",
                                                      "Lexicon for Sentiment Dictionary",
                                                      list("afinn", "bing",
                                                           "loughran", "nrc")),
                                          selectInput("agg_var",
                                                      "Select which variable to aggregate on",
                                                      c("", names(grouped())) %||% c(""))))})



insighted <- reactive({
  switch(input$what_vis,
         "Term Frequency" = get_term_insight(grouped(),
                                             input$what_vis),
         "n-gram Frequency" = get_term_insight(grouped(),
                                               c("n-grams", "n-gram Frequency"),
                                               input$n_gram),
         "Key Words" = get_term_insight(grouped(),
                                        input$what_vis,
                                        input$summ_method),
         
         ########################################
         
         "Readability" = books_with_samples(merged()),
         
         ########################################
         "Term Sentiment" = get_term_insight(grouped(),
                                             input$what_vis,
                                             input$sent_lex),
         "Moving Average Term Sentiment" = get_term_insight(grouped(),
                                                            input$what_vis,
                                                            input$sent_lex,
                                                            input$term_sent_lag),
         "Aggregated Term Count" = get_aggregate_insight(grouped(),
                                                         c("Bound Aggregates", input$what_vis),
                                                         input$agg_var),
         "Key Sections" = get_aggregate_insight(grouped(),
                                                c("Bound Aggregates", input$what_vis),
                                                input$agg_var,
                                                input$summ_method),
         "Aggregated Sentiment" = get_aggregate_insight(grouped(),
                                                        c("Bound Aggregates", input$what_vis),
                                                        input$agg_var,
                                                        input$sent_lex))})


output$vis_options <- renderUI({
  switch(input$vis_type,
         "Word Cloud" = tagList(sliderInput("num_terms",
                                            "Select the number of terms to visualise",
                                            3, 50, 15),
                                selectInput("wordcloud_shape",
                                            "Select the shape of the wordcloud",
                                            list("circle",
                                                 "cardioid",
                                                 "diamond",
                                                 "square",
                                                 "triangle-forward",
                                                 "triangle-upright",
                                                 "pentagon",
                                                 "star"))),
         "Page View" = tagList(sliderInput("num_terms",
                                           "Select the number of terms to visualise",
                                           3, 400, 100),
                               sliderInput("term_index",
                                           "Select the point to begin visualisation from",
                                           1, nrow(insighted()), 1),
                               selectInput("palette",
                                           "Select the colour palette type",
                                           list("Sequential", "Diverging"))),
         
         "Bar" = tagList(sliderInput("num_terms", "Select the number of terms to visualise",
                                     2,50,15),
                         checkboxInput("desc", "Sort descending")))})


output$vis_facet_by <- renderUI(tagList(selectInput("vis_facet",
                                                    "select which variable to facet on",
                                                    c("", names(grouped())) %||% c("")),
                                        checkboxInput("scale_fixed", "Scale Fixed", value=TRUE)))






visualisation <- reactive({
  switch(input$vis_type,
         "Word Cloud" = switch(input$what_vis,
                               "n-gram Frequency" = get_vis(
                                 insighted(),
                                 input$vis_type,
                                 input$what_vis,
                                 input$vis_facet,
                                 input$scale_fixed,
                                 input$num_terms,
                                 x = `n-grams`,
                                 shape = input$wordcloud_shape),
                               "Aggregated Term Count" =,
                               "Key Sections" =,
                               "Aggregated Sentiment" = get_vis(
                                 insighted(),
                                 input$vis_type,
                                 input$what_vis,
                                 input$vis_facet,
                                 input$scale_fixed,
                                 input$num_terms,
                                 x = `Bound Aggregates`,
                                 shape = input$wordcloud_shape),
                               get_vis(insighted(),
                                       input$vis_type,
                                       input$what_vis,
                                       input$vis_facet,
                                       input$scale_fixed,
                                       input$num_terms,
                                       shape = input$wordcloud_shape)),
         "Page View" = get_vis(insighted(), input$vis_type,
                               input$what_vis,
                               input$vis_facet,
                               input$scale_fixed,
                               input$num_terms,
                               input$term_index,
                               palette = input$palette),
         "Time Series" = get_vis(insighted(), input$vis_type,
                                 input$what_vis,
                                 input$vis_facet,
                                 input$scale_fixed),
         "Bar" = switch(input$what_vis,
                        "n-gram Frequency" = get_vis(insighted(),
                                                     input$vis_type,
                                                     input$what_vis,
                                                     input$vis_facet,
                                                     input$scale_fixed,
                                                     input$num_terms,
                                                     x = `n-grams`,
                                                     desc = input$desc),
                        "Aggregated Term Count" =,
                        "Key Sections" =,
                        "Aggregated Sentiment" = get_vis(insighted(),
                                                         input$vis_type,
                                                         input$what_vis,
                                                         input$vis_facet,
                                                         input$scale_fixed,
                                                         input$num_terms,
                                                         x = `Bound Aggregates`,
                                                         desc = input$desc),
                        get_vis(insighted(), input$vis_type,
                                input$what_vis,
                                input$vis_facet,
                                input$scale_fixed,
                                input$num_terms,
                                desc = input$desc)),
         "Density" = get_vis(insighted(), input$vis_type,
                             input$what_vis, input$vis_facet,
                             input$scale_fixed),
         "Histogram" = get_vis(insighted(), input$vis_type,
                               input$what_vis,
                               input$vis_facet,
                               input$scale_fixed))
})

output$plot <- renderPlot({
  visualisation()
})

output$insighted_table <- renderDT({
  if (input$what_vis %in% c("Term Frequency",
                            "Key Words", "Term Sentiment")){
    tab <- insighted() %>%
      select(id, word, tail(names(.), 2)) %>%
      filter(!is.na(!! dplyr::sym(input$what_vis))) %>%
      distinct(word, .keep_all = TRUE) 
    
    if (input$desc == FALSE) {
        tab <- tab %>% arrange(desc(!! dplyr::sym(input$what_vis))) 
    }
    else{
      tab <- tab %>% 
        arrange(!! dplyr::sym(input$what_vis)) 
    }
    
  }
  
  else if (input$what_vis %in% c("n-gram Frequency")){
    tab <- insighted() %>%
      select(id, word, tail(names(.), 2)) %>%
      filter(!is.na(!! dplyr::sym(input$what_vis))) %>%
      distinct(n-grams, .keep_all = TRUE) 
    
    if (input$desc == FALSE) {
      tab <- tab %>% arrange(desc(!! dplyr::sym(input$what_vis))) 
    }
    else{
      tab <- tab %>% 
        arrange(!! dplyr::sym(input$what_vis)) 
    }
  }
  
  else if (input$what_vis %in% c("Moving Average Term Sentiment")){
    tab <- insighted() %>%
      select(id, word, tail(names(.), 1)) 
  }
  
  else {
    tab <- insighted() ##%>%
      #select(!! dplyr::sym(input$agg_var), !! dplyr::sym(input$what_vis)) %>%
      #distinct(!! dplyr::sym(input$agg_var), .keep_all = TRUE)
  }
  
})
  
output$downloadData <- downloadHandler(
  filename = function() {
    paste(input$what_vis, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(insighted(), file, row.names = FALSE)
  }
)