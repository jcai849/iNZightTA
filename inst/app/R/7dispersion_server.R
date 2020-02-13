#####################################################
#################### Key Words in Context 
#################### & Lexical Dispersion Plot
#####################################################

# This file contains the code in server that groups the text by id and collapses them together, also
# creates the lexical dispersion plot in Key Words in Context tab. 

merge_choices <- reactive({
  raw <- raw_data()
  c(input$section_by, names(raw)[names(raw) != "text"])
})

observe({
  updateSelectizeInput(session, "merge_id_grps", 
                       choices = merge_choices(), server = TRUE)
})

######### Create reactive object merged text for Dispersion Plot 
merged_disp <- reactive({
  merge_id(x = imported(), source = input$import_from, groups = input$merge_id_grps)
})


############################ Dispersion Plot

observeEvent(input$create_kwic, {
  ins <- eventReactive(input$create_kwic,{
    get_kwic(merged = merged_disp(), patt = input$disp_words, window = input$window,
             value = input$disp_valuetype,
             case_ins = input$disp_case_insensitive)
  })
  
  ####
  new.pts <- reactive({
    dt <- tryCatch({
      textplot_xray(ins(), scale = input$scale)[[1]]
    }, 
    error = function(e){
      return(plot_exception("Keywords not found"))
    })
    
    # add a column with the whole retrieved word
    dt$actual <- ins()$keyword
 
    brushedPoints(dt, input$plot_brush)
  })
  
  points <- data.table::data.table()
  makeReactiveBinding("points")
  
  observeEvent(input$add, {
    points <<- rbind(points, new.pts())
  })
  
  observeEvent(input$delete, {
    if (dim(points)[1] > 0) {
      points <<- points[!new.pts(), on = colnames(points)]
    }
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
  output$plot2 <- renderPlot({
    if (!is.null(ranges$x) & !is.null(ranges$y)){
      p2 <- tryCatch({
        textplot_xray(ins(), scale = input$scale)[[2]] +
          ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      }, 
      error = function(e){
        return(plot_exception("Keywords not found"))
      })
      
      if (dim(points)[1] > 0) {
        p2 <- p2 + ggplot2::geom_point(aes(position, yvar), data = points, color = "red") +
          ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      }
    }
    else{
      p2 <- tryCatch({ 
        textplot_xray(ins(), scale = input$scale)[[2]]
      }, 
      error = function(e){
        return(plot_exception("Keywords not found"))
      })
      
      if (dim(points)[1] > 0) {
        p2 <- p2 + ggplot2::geom_point(aes(position, yvar), data = points, color = "red")
      }
    }
    return(p2)
  })
  
  output$plot2.ui <- renderUI({
    plotOutput("plot2",
               dblclick = "plot_dblclick",
               brush = brushOpts(
                 id = "plot_brush",
                 resetOnNew = TRUE
               ),
               height = input$plot_height2
    )
  })
  
  output$keyword_table <- DT::renderDataTable({
    df <- data.frame(points)
    if (input$line_num == TRUE){
      data.frame("doc" = df$docname, "pre" = df$pre, "keyword" = df$actual, "post" = df$post)
    }
    else {
      data.frame("pre" = df$pre, "keyword" = df$actual, "post" = df$post)
    }
  }, filter = "top")
  
  output$download_kwic_data <- downloadHandler(
    filename = function() { paste0("kwic.csv") },
    content = function(file) {
      write.table(ins(), file, sep = ",", na = "", row.names = FALSE)
    }
  )
})

# output$quant <- renderUI({HTML(paste0("<small>", 
#                                       'Examples of pattern matching <a href = "https://quanteda.io/reference/kwic.html" target="_blank">here</a>.',
#                                       "</small>", "<br></br>"))})


