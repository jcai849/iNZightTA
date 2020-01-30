text_sources = c("Upload .txt, .csv, .xlsx, or .xls file", "Project Gutenberg", "Twitter",
                 "Spotify/Genius", "The Guardian Articles", "stuff.co.nz Comments", "Reddit")


ui <- navbarPage("iNZight Text Analytics",
                 tabPanel("Processing",
                          sidebarLayout(
                            sidebarPanel(
                              useShinyjs(),
                              selectizeInput("import_from", "Retrieve text from", choices = text_sources),
                              
                              conditionalPanel(
                                condition = "input.import_from == 'Project Gutenberg'",
                                selectInput("gutenberg_work", "Please select a book.", multiple = TRUE, choices = character(0))
                              ),
                              uiOutput("side"),
                               
                              
                              tags$h4("Process"),
                              checkboxInput("lemmatise", "Lemmatise"),
                              uiOutput("sw_lexicon"),
                              checkboxInput("stopwords", "Stopwords"),
                              
                              textInput("addl_stopwords", "Additional stopwords",
                                        placeholder = "separate,the,words,with,commas"),
                              
                              a(id = "toggleAdvanced4", "Or upload stopword file(s)"), #, href = "#"),
                              shinyjs::hidden(
                                div(id = "advanced4",
                                    
                                    fileInput(inputId = "sw_file", label = ".txt files with one stopword per line", multiple = TRUE,
                                              accept = c("text/comma-separated-values,text/plain")),
                                    
                                )
                              ),
                              
                              actionButton("prep_button", "Prepare Text"),
                              tags$hr(),
                              
                              selectInput("section_by", "Section By",
                                          list("", "chapter", "part", "section", "canto", "book")),
                              
                              uiOutput("vars_to_filter"),
                              textInput("filter_pred", "value to match", "")
                              
                            ),
                            mainPanel(
                              tabsetPanel(
                                
                                tabPanel("Imported",
                                         
                                         fluidRow(
                                           downloadButton("downloadData_imported", "Download as csv"),
                                           
                                           column(width = 12,
                                                  ###################
                                                  textOutput("text"), 
                                                  ###################
                                                  
                                                  DT::dataTableOutput("imported_show")
                                                  #tableOutput("not_coll")
                                                  
                                           )
                                         )
                                ),
                                tabPanel("Pre-processed",
                                         fluidRow(
                                           downloadButton("downloadData_pre_processed", "Download as csv"),
                                           column(width = 12,
                                                  DT::dataTableOutput("pre_processed_show")
                                           )
                                         )
                                ),
                                tabPanel("Processed",
                                         downloadButton('downloadprocessed', 'Download'),
                                         
                                         tableOutput("table")
                                         
                                )
                                
                              )
                            ))),
                 
                 tabPanel("Visualisation",
                          sidebarLayout(
                            sidebarPanel(selectInput("what_vis",
                                                     "Select what you want to Visualise",
                                                     c("Term Frequency",
                                                       "Term Frequency-Inverse Document Frequency", 
                                                       "n-gram Frequency",
                                                       "Key Words",
                                                       ###########
                                                       "Readability",
                                                       "Word Tree",
                                                       ###########
                                                       "Term Sentiment",
                                                       "Moving Average Term Sentiment",
                                                       "Aggregated Term Count",
                                                       "Key Sections",
                                                       "Aggregated Sentiment")),
                                         
                                         #####
                                         conditionalPanel(
                                           condition = "!(input.what_vis == 'Word Tree'||input.what_vis == 'Readability')",
                                           uiOutput("group_by")
                                         ),
                                         
                                         conditionalPanel(
                                           condition = "!(input.what_vis == 'Readability')",
                                           uiOutput("insight_options")
                                         ),
                                         
                                         
                                         conditionalPanel(
                                           condition = "!(input.what_vis == 'Word Tree'||input.what_vis == 'Readability')",
                                           uiOutput("vis_options"),
                                           uiOutput("vis_facet_by"),
                                           downloadButton("downloadData", "Download data used in visualization")
                                         ), 
                                         
                                         ##### controlling plot height
                                         sliderInput("plot_height", "Plot height",
                                                     min = 400, max = 2000,
                                                     value = 1000), 
                                         ##### controlling text size
                                         sliderInput("text_size", "Text size", 
                                                     min = 5, max = 30, value = 15),
                                         ##### controlling number of columns for facet wrap
                                         sliderInput("n_col_facet", "Number of columns for multi-panel plots", 
                                                     min = 1, max = 7, value = 5)
                            ),
                            
                            mainPanel(
                              conditionalPanel(
                                condition = "input.what_vis == 'Word Tree'",
                                addSpinner(htmlOutput("shinytest"), spin = "fading-circle", color = "#000000")
                              ),
                              conditionalPanel(
                                condition =  "input.what_vis == 'Readability'",
                                plotOutput("flesch_plot",
                                           dblclick = dblclickOpts(
                                             id = "plot1_click"), height = "1000px"
                                ),
                                
                                verbatimTextOutput("ex")
                              ),
                              conditionalPanel(
                                condition =  "!(input.what_vis == 'Word Tree'||input.what_vis == 'Readability')",
                                #plotOutput("plot", height = "1000px"),
                                uiOutput("plot.ui"), 
                                DTOutput("insighted_table")
                              )
                              
                            ))),
                 #################
                 tabPanel("Keywords in Context",
                          
                          sidebarLayout(sidebarPanel(textInput("disp_words", "Keywords", value = ""),
                                                     
                                                     selectInput("disp_valuetype",
                                                                 "Type of Pattern Matching",
                                                                 list("glob", "regex",
                                                                      "fixed")),
                                                     selectInput("scale",
                                                                 "Scale",
                                                                 list("absolute",
                                                                      "relative")),
                                                     numericInput("window", "Window", value = 5, min = 1, max = 10),
                                                     
                                                     checkboxInput("disp_case_insensitive", "Case Insensitive",
                                                                   value = TRUE, width = NULL),
                                                     sliderInput("plot_height2", "Plot height",
                                                                 min = 400, max = 2000,
                                                                 value = 1000),
                                                     actionButton("create_kwic", "Create lexical dispersion plot"),
                                                     tags$hr(),
                                                     
                                                     ##### For keywords in context
                                                     actionButton("add", "Add Points"),
                                                     actionButton("delete", "Delete Points"),
                                                     # conditionalPanel("input.import_from == 'Project Gutenberg'",
                                                     #                  checkboxInput('groups', 'Group by chapter', value = FALSE, width = NULL)),
                                                     checkboxInput('line_num', 'See doc name', value = FALSE, width = NULL)
                                                     
                          ),
                          mainPanel(
                            # plotOutput("plot2",
                            #            dblclick = "plot_dblclick",
                            #            brush = brushOpts(
                            #              id = "plot_brush",
                            #              resetOnNew = TRUE
                            #            ),
                            #            height = "800px"
                            # ),
                            uiOutput("plot2.ui"),
                            DT::dataTableOutput("keyword_table")
                          ))
                 ),
                 
                 add_busy_spinner(spin = "fading-circle")
)
