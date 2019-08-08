library(shiny)
library(inzightta)

ui <- fluidPage(
  # App title ----
  titlePanel("Uploading Files"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
      ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
      tableOutput("contents")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    test1 <- reactive({inzightta::import_files(input$file1$datapath)})
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    test1()
  })
}

# Create Shiny app ----
shinyApp(ui, server)
