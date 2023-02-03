
# allow uploading files up to 10MB
options(shiny.maxRequestSize = 10*1024^2) 
library("markdown")

shinyServer(function(input, output, session) {
  source(file.path("server", "server_tab_load.R"), local = TRUE)$value
  source(file.path("server", "server_tab_settings.R"), local = TRUE)$value
  source(file.path("server", "server_tab_results.R"), local = TRUE)$value
  # source(file.path("server", "server_tab_report.R"), local = TRUE)$value
})


# input <- NULL
# input$sample_data <- 'selenium'
# input$y <- 'dead'
# input$type <- 'binomial'
# input$total <- 'total'
