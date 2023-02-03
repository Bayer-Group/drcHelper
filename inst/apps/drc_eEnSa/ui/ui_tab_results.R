tabPanel(
  title = '3. Model',
  id = 'tab_results',
  value = 'tab_results',
  icon = icon('play'),
  
  sidebarLayout(
    sidebarPanel(
      h3('Select Model'),
      selectInput('fct', 'Model', 
                  c(`auto: Select by lowest AIC` = 'auto', select_mod)),
      h3('Select Model Output'),
      textInput('ecx', 'ECx (separated by comma): ',
                '10, 50'),
      radioButtons('type_resp', 'ECx Type', c(Absolute = 'absolute', Relative = 'relative'),
        selected = 'relative'
      ),
      radioButtons('reference', 'Reference Level', c(Control = 'control', Upper = 'upper'),
                   selected = 'control'
      )
    ),
    mainPanel(
      plotlyOutput('model_plot'), 
      br(),
      h3('Effective Concentrations'),
      tableOutput(outputId = 'model_ecx'), 
      h3('Model Details'), 
      verbatimTextOutput(outputId = 'model_summary'),
      verbatimTextOutput(outputId = 'debug')
    )
  )
)
