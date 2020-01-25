#####################################################
#################### Key Words in Context 
#################### & Lexical Dispersion Plot
#####################################################

# This file contains the code in server that groups the text by id and collapses them together, also
# creates the lexical dispersion plot in Key Words in Context tab. 


######### Create reactive object merged text. Both for Dispersion Plot and for Readability
merged <- reactive({
  merge_id(x = imported_filtered(), source = input$import_from, groups = input$groups)
})


############################ Dispersion Plot

observeEvent(input$create_kwic, {
  ins <- eventReactive(input$create_kwic,{
    get_kwic(merged = merged(), patt = input$disp_words, window = input$window,
             value = input$disp_valuetype,
             case_ins = input$disp_case_insensitive)
  })
  
  # output$view <- renderDataTable({
  #   kwic <- ins()
  #   data.frame("pre" = kwic$pre, "keyword" = kwic$keyword, "post" = kwic$post)
  # })
  ####
  new.pts <- reactive({
    dt <- tryCatch({
      textplot_xray(ins(), scale = input$scale)[[1]]
    }, 
    error = function(e){
      return(plot_exception("Keywords not found"))
    })
  
    brushedPoints(dt, input$plot_brush)
  })
  
  points <- data.table()
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
          coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      }, 
      error = function(e){
        return(plot_exception("Keywords not found"))
      })
      
      if (dim(points)[1] > 0) {
        p2 <- p2 + geom_point(aes(position, yvar), data = points, color = "red") +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
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
        p2 <- p2 + geom_point(aes(position, yvar), data = points, color = "red")
      }
    }
    return(p2)
  })
  
  output$keyword_table <- DT::renderDataTable({
    df <- data.frame(points)
    if (input$line_num == TRUE){
      data.frame("doc" = df$docname, "pre" = df$pre, "keyword" = df$keyword, "post" = df$post)
    }
    else {
      data.frame("pre" = df$pre, "keyword" = df$keyword, "post" = df$post)
    }
  }, filter = "top")
})