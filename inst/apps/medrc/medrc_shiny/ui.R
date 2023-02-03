library(shiny)
library(medrc)
library(multcomp)
library(xtable)
library(msm)

shinyUI(fluidPage(
  ###################
  navbarPage("medrc", 
             ##################################
             tabPanel("Uploading File",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput('file1', 'Choose CSV File', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                          tags$hr(),
                          checkboxInput('header', 'Header', TRUE),
                          radioButtons('sep', 'Column Separator', c(Comma=',', Semicolon=';', Tab='\t'), ','),
                          radioButtons('dec', 'Decimal Separator', c(Dot='.', Comma=','), '.'),
                          radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"')
                        ),
                        mainPanel(
                          tableOutput('contents')
                        )
                        )
                      ),
            ##################################################### 
            tabPanel("Select Variables",
                     fluidRow(
                       column(3, wellPanel(
                         uiOutput("choose_response"),
                         uiOutput("choose_dose"),
                         tags$hr(),
                         uiOutput("choose_fixed"),
                         uiOutput("choose_random")
                       )),
                       column(3, wellPanel(
                         htmlOutput("resp_summary"),
                         htmlOutput("dose_summary"),
                         htmlOutput("fixed_summary"),
                         htmlOutput("random_summary")
                       )),
                       column(3, wellPanel(
                         checkboxInput("logcheck", label = "logarithmic x-axis", value = FALSE),
                         plotOutput("doseresponseplot")
                       ))     
                       )
                     ),
            ##################################
            tabPanel("Dose-Response Model",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("model", h4("Choose model"), 
                                     choices  = c("", "LL.2", "LL.3", "LL.4", "LL.5", "L.3", "L.4", "L.5", "W1.2", "W1.3", "W1.4", "W2.2", "W2.3", "W2.4", "LN.2", "LN.3", "LN.4", "BC.4", "BC.4", "AR.2", "AR.3", "EXD.2", "EXD.3", "MM.2", "MM.3"),
                                     selected = ""),
                         uiOutput("radio_fixed"),
                         uiOutput("radio_random"),
                         tags$hr(),
                         actionButton("action", "Go!", icon=icon("refresh"))
                       ),
                       mainPanel(  
                         htmlOutput("modelout")
                       ))
            ),
            ##################################
            tabPanel("Predicted Curves",
                   mainPanel(
                     uiOutput("logcheckrandom"),
                     plotOutput("prediction")
            )),
            ##################################
            tabPanel("Effective Dose",
                     sidebarLayout(
                       sidebarPanel(
                         checkboxGroupInput("EDcheck", h4("Choose ED levels:"), choices = c(1,5,10,25,50,75,90,95,99)),
                         tags$hr(),
                         numericInput("EDconflevel", h5("Confidence level"), value=0.95, min=0, max=1, step=0.001),
                         checkboxInput("EDmulti", h5("Multiplicity Adjustment"), value=TRUE),
                         checkboxInput("EDcilog", h5("Estimate confidence limits on log-scale"), value=FALSE),
                         uiOutput("marginalizeED"),
                         tags$hr(),
                         actionButton("actionED", "Go!", icon=icon("refresh"))
                       ),
                      mainPanel(
                        htmlOutput("EDout")
                      )
                     )
            ),
            ##################################
            tabPanel("Benchmark Dose",
                     sidebarLayout(
                       sidebarPanel(
                         checkboxGroupInput("BMDcheck", h4("Choose benchmark response levels:"), choices = c(1,2,5,10,15,25,50)),
                         tags$hr(),
                         numericInput("BMDconflevel", h5("Confidence level"), value=0.95, min=0, max=1, step=0.001),
                         numericInput("BMDbackground", h5("Probability of an abnormal response"), value=0.05, min=0, max=1, step=0.001),
                         selectInput("BMDdef", label=h5("BMD Definition"), choices=list("additional"="additional", "extra"="extra"), selected="additional"),
                         checkboxInput("BMDcilog", h5("Estimate confidence limits on log-scale"), value=FALSE),
                         uiOutput("marginalizeBMD"),
                         tags$hr(),
                         actionButton("actionBMD", "Go!", icon=icon("refresh"))
                       ),
                       mainPanel(
                         htmlOutput("BMDout")
                       )
                     )
            )            
)))