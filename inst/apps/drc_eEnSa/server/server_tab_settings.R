# UI Elements -------------------------------------------------------------
output$conc = renderUI({
  vars <- get_vars()
  selectInput('conc', 'Concentration', vars, selected = 'conc')
})

output$y = renderUI({
  vars <- get_vars()
  selectInput('y', 'Response', vars, selected = 'dead', 
              selectize = TRUE)
})

output$total = renderUI({
  vars <- get_vars()
  selectInput('total', 'Total (binomial data)', vars, selected = 'total')
})

observe({
  toggleState(id = "total", condition = input$type == 'binomial')
})


output$group = renderUI({
  vars <- get_vars()
  vars <- c(None = '__none__', vars)
  selectInput('group', 'Group (optional)', vars, selected = 'type')
})



# Reactive Functions ------------------------------------------------------


get_pdata <- reactive({
  df <- get_data()
  
  # transform if binomial
  if (input$type == 'binomial') {
    df[['y_trans']] <- df[[input$y]] / df[[input$total]]
  } else {
    df[['y_trans']] <- df[[input$y]]
  }
  
  if (input$group != '__none__') {
    df[[input$group]] <- factor(df[[input$group]])
  }
  df
})


# prepare data form plotting with ggplot
prep_data <- reactive({
  df <- get_pdata()
  
  # shift 0 xvalues
  if (min(df[[input$conc]]) == 0) {
    newmin <- min(df[[input$conc]] + sort(unique(df[[input$conc]]))[2] / 10)
    df[df[[input$conc]] == 0, input$conc] <- newmin
  }
  
  df
})


plot_raw <- reactive({
  
  df <- prep_data()
  brks <- get_breaks(df[[input$conc]])
  
  p <-  ggplot(df, aes_string(x = input$conc, y = 'y_trans')) +
    scale_x_log10(breaks = brks) +
    theme_edi() +
    labs(y = 'Response', x = 'Concentration')
  
  if (input$group != '__none__') {
    p <- p + geom_point(aes_string(col = input$group))
  } else {
    p <- p + geom_point()
  }
  p
})

build_plotly <- reactive({
  df <- get_pdata()
  
  # manually control tooltips (remove conc onlog-scale, use raw scale)
  p <- plotly_build(plot_raw())
  p$x$data[[1]]$text <- paste("Concentration:", df[[input$conc]], "<br>",
                              "Response:", round(df[['y_trans']], 2), "<br>")
  
  p
})

# Plot output -------------------------------------------------------------


output$plot_settings <- renderPlotly({
  print(build_plotly())
})
