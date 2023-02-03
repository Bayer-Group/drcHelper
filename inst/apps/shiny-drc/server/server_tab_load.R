get_data <- reactive({
  file <- input$upload_data
  example <- input$sample_data
  
  if (is.null(file) & example == 'none') {
    return(NULL)
  }
  
  if (!is.null(file)) {
    df <- read.csv(file$datapath, 
                   header = input$header, 
                   sep = input$sep)
  } 
  
  if ( example != 'none') {
    df <- switch(example, 
                 selenium = selenium)
  }
  df
  })


# extract column names
get_vars <- reactive({
  df <- get_data()
  if (is.null(df)) {
    return(NULL)
  }
  vars <- names(df)
  names(vars) <- vars
  return(vars)
})



output$data_table <- renderDataTable({
  get_data()
  },
  options = list(pageLength = 10, searching = FALSE)
  )
