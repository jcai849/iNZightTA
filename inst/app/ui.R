text_sources = c("Upload .txt, .csv, .xlsx, or .xls file", "Project Gutenberg", "Twitter",
                 "Spotify/Genius", "The Guardian Articles", "stuff.co.nz Comments", "Reddit")


ui <- navbarPage("iNZight Text Analytics",
                 tabPanel("Processing",
                          sidebarLayout(
                            sidebarPanel(
                              shinyjs::useShinyjs(),
                              selectizeInput("import_from", "Retrieve text from", choices = text_sources),
                              
                              conditionalPanel(
                                condition = "input.import_from == 'Project Gutenberg'",
                                selectInput("gutenberg_work", "Select work(s)", multiple = TRUE, choices = character(0))
                              ),
                              uiOutput("side"),
                              tags$h4("Process"),
                              checkboxInput("lemmatise", "Lemmatise"),
                              checkboxInput("expand_contractions", "Expand contractions"),
                              conditionalPanel(
                                condition = "input.import_from == 'Twitter'",
                                checkboxInput("remove_hash", "Remove hashtags"), 
                                checkboxInput("remove_user", "Remove user mentions"),
                              ),
                              uiOutput("sw_lexicon"),
                              checkboxInput("stopwords", "Remove stopwords"),
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
                              conditionalPanel(
                                condition = "input.import_from == 'Project Gutenberg'||input.import_from == 'Upload .txt, .csv, .xlsx, or .xls file'", 
                                selectInput("section_by", "Section By",
                                          list("", "chapter", "part", "section", "canto", "book"))
                              ),
                              
                              uiOutput("vars_to_filter"),
                              textInput("filter_pred", "value to match", "")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Imported",
                                         fluidRow(
                                           downloadButton("downloadData_imported", "Download as csv"),
                                           
                                           column(width = 12,
                                                  # for the console messages
                                                  textOutput("text"), 
                                                  ###################
                                                  DT::dataTableOutput("imported_show")
                                                  #tableOutput("not_coll")
                                           )
                                         )
                                ),
                                tabPanel("Processed",
                                         #downloadButton('downloadprocessed', 'Download'),
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
                                                       "Word Tree",
                                                       ###########
                                                       "Term Sentiment",
                                                       "Moving Average Term Sentiment",
                                                       "Aggregated Term Count",
                                                       "Key Sections",
                                                       "Aggregated Sentiment")),
                                         
                                         #####
                                         conditionalPanel(
                                           condition = "!(input.what_vis == 'Word Tree')",
                                           uiOutput("group_by")
                                         ),
                                         
                                         
                                           uiOutput("insight_options"),
                                           uiOutput("senti_choices"),
                                           uiOutput("senti_choices2"),
                                           ################################
                                           ################################
                                           
                                           conditionalPanel(
                                             condition = "input.what_vis == 'Word Tree'", 
                                             selectizeInput("merge_id_grps_wt", "Group data by", choices = character(0)),
                                             selectizeInput("filter_wt", "View only for", choices = character(0)), 
                                             actionButton("create_tree", "Create Tree")
                                             ), 
                                           
                                           
                                           ################################
                                           ################################
                                           conditionalPanel(
                                             condition = "input.what_vis == 'Aggregated Sentiment'", 
                                             checkboxInput("scale_senti", "Scale Aggregated Sentiment Scores")
                                           ),
                                        
                                         
                                         
                                         conditionalPanel(
                                           condition = "!(input.what_vis == 'Word Tree')",
                                           uiOutput("vis_options"),
                                           uiOutput("vis_facet_by"),
                              
                                           a(id = "toggle_vis", "Additional visualization options", href = "#"),
                                           
                                           uiOutput("add_vis_options"), 
                                           tags$hr(),
                                           
                                           downloadButton("downloadData", "Download data used in visualization")
                                         )
                            ),
                            
                            mainPanel(
                              conditionalPanel(
                                condition = "input.what_vis == 'Word Tree'",
                                shinyWidgets::addSpinner(htmlOutput("shinytest"), spin = "fading-circle", 
                                                         color = "#000000")
                              ),
                              conditionalPanel(
                                condition =  "!(input.what_vis == 'Word Tree')",
                                uiOutput("plot.ui"), 
                                DT::DTOutput("insighted_table"),
                                actionButton("subset_data", "Subset Data"),
                                actionButton("restore_data", "Restore Data"),
                              )
                            ))),
                 #################
                 tabPanel("Keywords in Context",
                          sidebarLayout(sidebarPanel(textInput("disp_words", "Keyword(s) or Key Phrase(s)", 
                                                               placeholder = "love,thousand pounds"),
                                                     
                                                     selectInput("disp_valuetype",
                                                                 "Type of Pattern Matching",
                                                                 list("glob", "regex",
                                                                      "fixed")),
                                                     uiOutput("quant"), 
                                                    
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
                                                     selectInput("merge_id_grps", "Group text by", 
                                                                 choices = NULL, selected = "id"), 
                                                     actionButton("create_kwic", "Create lexical dispersion plot"),
                                                     tags$hr(),
                                                     
                                                     ##### For keywords in context
                                                     
                                                     actionButton("add", "Add Points"),
                                                     actionButton("delete", "Delete Points"),
                                            
                                                     checkboxInput('line_num', 'See doc name', value = FALSE, width = NULL),
                                                     downloadButton("download_kwic_data", "Download KWIC table")
                                                     
                          ),
                          mainPanel(
                            uiOutput("plot2.ui"),
                            DT::dataTableOutput("keyword_table")
                          ))
                 ),
                 tabPanel("Getting keys and tokens", 
                          fluidPage(
                            navlistPanel(
                              #"Getting keys and tokens",
                              tabPanel("Twitter",
                                       includeMarkdown(system.file("app/R/help_files/twitter_token.rmd", package = "inzightta"))),
                              tabPanel("Spotify/Genius",
                                       includeMarkdown(system.file("app/R/help_files/spot.rmd", package = "inzightta"))
                              ),
                              tabPanel("The Guardian Articles",
                                       includeMarkdown(system.file("app/R/help_files/guardian.rmd", package = "inzightta"))
                              ), 
                              tabPanel("stuff.co.nz comments",
                                       includeMarkdown(system.file("app/R/help_files/stuff.rmd", package = "inzightta"))
                              )
                            )
                          )
                          ), 
                 shinybusy::add_busy_spinner(spin = "fading-circle")
                 )
