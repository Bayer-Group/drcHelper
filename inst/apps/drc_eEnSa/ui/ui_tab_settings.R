tabPanel(
  title = '2. Settings',
  id = 'tab_settings',
  value = 'tab_settings',
  icon = icon('wrench'),
  
  sidebarLayout(
    sidebarPanel(
      h3("Select columns"),
      uiOutput('conc'),
      uiOutput('y'),
      selectInput('type', 'Response type', 
                  c("binomial", "continuous", "Poisson", "quantal", "event")),
      uiOutput('total'),
      uiOutput('group')
    ),
    mainPanel(
      plotlyOutput('plot_settings')
    )
  )
)
