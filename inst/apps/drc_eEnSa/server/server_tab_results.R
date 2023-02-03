# Auto-select model -------------------------------------------------------

auto_fct <- reactive({
  pdata <- get_pdata()
  fla <- as.formula(paste0('y_trans ~ ', input$conc))
  
  # set vars to NULL
  if (input$group == '__none__') {
    curveid <- NULL
  } else {
    curveid <- pdata[[input$group]]
  }
  
  if (input$type == 'binomial') {
    wght <-  pdata[[input$total]]
  } else {
    wght <- NULL
  }
  
  # fit model
  mod <- drm(fla, 
             curveid = curveid,
             weights = wght,
             data = pdata,
             fct = LL.2(),
             type = input$type
  )
  # fit all models
  all_mods <- mselect(mod, fctList = list(LL.2(), LL.3(), LL.3u(), LL.4(), LL.5(),
                                          W1.2(),  W1.3(), W1.4(), W2.2(), W2.3(), W2.4(),
                                          BC.4(), BC.5(),
                                          LL2.2(), LL2.3(), LL2.3u(), LL2.4(), LL2.5(),
                                          AR.2(), AR.3(),
                                          MM.2(), MM.3()))
  
  rownames(all_mods)[which.min(all_mods[ , 2])]
})



# Fit model ---------------------------------------------------------------

model <- reactive({
  pdata <- get_pdata()
  fla <- as.formula(paste0('y_trans ~ ', input$conc))
  
  # set vars to NULL
  if (input$group == '__none__') {
    curveid <- NULL
  } else {
    curveid <- pdata[[input$group]]
  }
  
  if (input$type == 'binomial') {
    wght <-  pdata[[input$total]]
  } else {
    wght <- NULL
  }
  
  # select fct
  if (input$fct == 'auto') {
    fctt <- paste0(auto_fct(), '()')
  } else {
    fctt <- paste0(input$fct, '()')
  }
  
  # fit model
  mod <- drm(fla, 
             curveid = curveid,
             weights = wght,
             data = pdata,
             fct = eval(parse(text = fctt)),
             type = input$type
  )
  mod
})


# get model predictions
get_pred <- reactive({
  df <- prep_data()
  
  # new dose levels as support for the line
  pconc <- exp(seq(log(min(df[[input$conc]] + sort(unique(df[[input$conc]]))[2] / 10)), 
                   log(max(df[[input$conc]])), 
                   length = 100))
  
  if (input$group != '__none__') {
    newdata <- expand.grid(pconc ,
                           unique(df[[input$group]]))
    names(newdata) <- c(input$conc, 'curveid')
    newdata[['curveid']] <- factor(newdata[['curveid']])
  } else {
    newdata <- expand.grid(pconc)
    names(newdata) <- input$conc
  }
  
  # predictions and confidence intervals
  pm <- predict(model(), newdata = newdata, interval = "confidence")
  # new data with predictions
  newdata$p <- pm[ , 1]
  newdata$pmin <- pm[ , 2]
  newdata$pmax <- pm[ , 3]
  
  newdata
})


plot_model <- reactive({
  df <- get_pdata()
  brks <- get_breaks(df[[input$conc]])
  pdat <- get_pred()

  p <-  ggplot() +
    scale_x_log10(breaks = brks) +
    theme_edi() +
    labs(y = 'Response', x = 'Concentration')

  if (input$group != '__none__') {
    p <- p +
      geom_point(data = df, aes_string(x = input$conc, y = 'y_trans', 
                                       col = input$group,
                                       show.legend = FALSE)) +
      geom_ribbon(data = pdat, aes_string(x = input$conc,
                                             ymin = 'pmin', ymax = 'pmax',
                                             fill = 'curveid'),
                  alpha = 0.2, 
                  show.legend = FALSE) +
      geom_line(data = pdat, aes_string(x = input$conc, y = 'p',
                                           col = 'curveid')
                , show.legend = FALSE) 

  } else {
    p <- p + geom_point(data = df, aes_string(x = input$conc, y = 'y_trans')) +
      geom_ribbon(data = pdat, aes_string(x = input$conc,
                                             ymin = 'pmin', ymax = 'pmax'),
                  alpha = 0.2) +
      geom_line(data = pdat, aes_string(x = input$conc, y = 'p'),
                color = 'steelblue')
  }
  
  p
})


build_plotly2 <- reactive({
  df <- get_pdata()
  pdat <- get_pred()
  
  p <- plot_model()
  
  pp <- plotly_build(p)
  
  if (input$group != '__none__') {
    n_levs <- length(unique(df[[input$group]]))
    levs <- unique(df[[input$group]])
    # fix hoover for points
    for (i in 1:n_levs) {
      # pp$x$data[[i]]$text <- paste("Concentration:", df[[input$conc]][df[[input$group]] == levs[i]], "<br>",
      #                              "Response:", round(df[['y_trans']][df[[input$group]] == levs[i]], 2), "<br>")
      pp$x$data[[i]]$text <- NULL
    }
    for (i in n_levs+(1:n_levs)) {
      pp$x$data[[i]]$text <- NULL
    }
    # fix hoover for lines
    for (i in 2*n_levs+(1:n_levs)) {
      pp$x$data[[i]]$text <- paste("Concentration:", round(pdat[[input$conc]][pdat[['curveid']] == i-2*n_levs], 2), "<br>",
                                   "Upper CI:",  round(pdat[['pmax']][pdat[['curveid']] == i-2*n_levs], 2), "<br>",
                                   "Modeled Response:", round(pdat[['p']][pdat[['curveid']] == i-2*n_levs], 2), "<br>",
                                   "Lower CI:",  round(pdat[['pmin']][pdat[['curveid']] == i-2*n_levs], 2), "<br>")
    }
  } else {
    # manually control tooltips (remove conc onlog-scale, use raw scale)
    pp$x$data[[1]]$text <- paste("Concentration:", df[[input$conc]], "<br>",
                                 "Response:", round(df[['y_trans']], 2), "<br>")
    
    pp$x$data[[2]]$text <- NULL # remove tooltip for CI
    pp$x$data[[3]]$text <- paste("Concentration:", round(pdat[[input$conc]], 2), "<br>",
                                 "Upper CI:",  round(pdat[['pmax']], 2), "<br>",
                                 "Modeled Response:", round(pdat[['p']], 2), "<br>",
                                 "Lower CI:",  round(pdat[['pmin']], 2), "<br>")
  }

  pp
})



# get ECx -----------------------------------------------------------------
get_ecx <- reactive({
  df <- get_pdata()
  ecx <- numextractall(input$ecx)
  eddf <- data.frame(ED(model(), ecx, 
                        interval = 'delta', 
                        type = input$type_resp,
                        reference = input$reference))
  
  if (input$group != '__none__') {
    eddf[['group']] = rep(unique(df[[input$group]]), each = length(ecx))
    eddf[['ECx']] = rep(ecx, length(unique(input$group)))
    eddf <- eddf[ , c('ECx', 'group', 'Estimate', 'Std..Error', 'Lower', 'Upper')]
    names(eddf) <- c('ECx', 'Group', 'Estimate', 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
  } else {
    eddf[['ECx']] = ecx
    eddf <- eddf[ , c('ECx', 'Estimate', 'Std..Error', 'Lower', 'Upper')]
    names(eddf) <- c('ECx', 'Estimate', 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
  }
  eddf
})

output$model_ecx <- renderTable({
  get_ecx()
},
digits = 3)


# Plot --------------------------------------------------------------------

output$model_plot <- renderPlotly({
 print(build_plotly2())
})


output$model_summary <- renderPrint({
  summary(model())
})

output$debug <- renderPrint({
  p <- plot_model()
  pp <- plotly_build(p)
  # str(pp$x$data)
  sapply(pp$x$data, function(y) y$mode)
  head(get_pred())
})
