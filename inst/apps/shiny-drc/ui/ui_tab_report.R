tabPanel(
  title = '4. Report',
  id = 'tab_report',
  value = 'tab_report',
  icon = icon('book'),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton('downloadReport')
    ),
    mainPanel(
    )
  )
)
