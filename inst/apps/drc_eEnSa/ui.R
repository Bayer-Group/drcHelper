tagList(
  useShinyjs(),
  navbarPage(
      tags$head(tags$style(HTML('.navbar-static-top {background-color: green;}',
                                '.navbar-default .navbar-nav>.active>a {background-color: green;}'))),
      
      title = 'Dose-Response',
      id = 'main_nav',
      inverse = TRUE,
      fluid = FALSE,
      collapsible = TRUE
      
      # tabs
      , source(file.path('ui/ui_tab_landing.R'), local = TRUE)$value
      , source(file.path('ui/ui_tab_load.R'), local = TRUE)$value
      , source(file.path('ui/ui_tab_settings.R'), local = TRUE)$value 
      , source(file.path('ui/ui_tab_results.R'), local = TRUE)$value
      # , source(file.path('ui/ui_tab_report.R'), local = TRUE)$value
      , source(file.path('ui/ui_tab_readme.R'), local = TRUE)$value
  )
)
