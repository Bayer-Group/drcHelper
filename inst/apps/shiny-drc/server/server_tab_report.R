utput$report <- downloadHandler(
  filename = function() {
    paste0('drc_report.', switch(input$report_format, 
                                 PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  content = function(file) {
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    render(tempReport, 
           output_format = switch(input$report_format,
                                  PDF = pdf_document(), 
                                  HTML = html_document(), 
                                  Word = word_document()
           ),
           output_file = file)
  }
)
