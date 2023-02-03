library(shiny)
library(medrc)
library(multcomp)
library(xtable)
library(msm)

shinyServer(function(input, output) {
  dat <- reactive({
    inFile <- input$file1    
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, dec=input$dec, quote=input$quote)
  })  
  output$contents <- renderTable({
    dat()
  })
  output$choose_response <- renderUI({
    df <- dat()
    if(is.null(df))  return()
    colnames <- c("", colnames(df))
    selectInput("response", h5("Choose response variable"), 
                       choices  = colnames,
                       selected = NULL)
  })
  output$choose_dose <- renderUI({
    df <- dat()
    if(is.null(df))  return()
    colnames <- c("", colnames(df))
    selectInput("dose", h5("Choose dose variable"), 
                choices  = colnames,
                selected = "")
  })
  
  output$choose_fixed <- renderUI({
    df <- dat()
    if(is.null(df))  return()
    colnames <- c("Single fixed effect curve", colnames(df))
    selectInput("fixed", h5("Choose fixed effects variable"), 
                choices  = colnames,
                selected = "Single fixed effect curve")
  })  
  
  output$choose_random <- renderUI({
    df <- dat()
    if(is.null(df))  return()
    colnames <- c("No random effects", colnames(df))
    selectInput("random", h5("Choose random effects variable"), 
                choices  = colnames,
                selected = "No random effects")
  }) 
  
  ddd <- reactive({
    df <- dat()
    if (is.null(df) | is.null(input$response) | is.null(input$dose))  return()
    if (input$response == "") return()
    if (input$dose == "") return()
    if (input$response == input$dose) return()
    fixed <- if (input$fixed == "Single fixed effect curve") NULL else input$fixed
    random <- if (input$random == "No random effects") NULL else input$random
    df <- df[, c(input$response, input$dose, fixed, random)]
    if (input$fixed != "Single fixed effect curve") df[,fixed] <- as.factor(df[,fixed])
    if (input$random != "No random effects") df[,random] <- as.factor(df[,random])
    return(df)
  })
  
  output$resp_summary <- renderText({
    df <- ddd()
    if (is.null(df)) return()
    resptext <- if (!is.numeric(df[,input$response])) "Needs to be numeric!" else input$response
    paste("<strong>Response:</strong>", resptext)
  })
  output$dose_summary <- renderText({
    df <- ddd()
    if (is.null(df)) return()
    dosetext <- if (!is.numeric(df[,input$dose])) "Needs to be numeric!" else input$dose
    paste("<strong>Dose:</strong>", dosetext)
  })  
  output$fixed_summary <- renderText({
    df <- ddd()
    if (is.null(df)) return()
    if (input$fixed == "Single fixed effect curve") return("No fixed effects selected.") else {
      fixedtext <- input$fixed
      flev <- levels(df[,input$fixed])
      if (length(flev) > 10) flev <- c(flev[1:10], "...")
      print(paste("<strong>Fixed effects</strong>:", input$fixed, "<br/>with levels:", paste(flev, collapse=",")))
    }
  })   
  output$random_summary <- renderText({
    df <- ddd()
    if (is.null(df)) return()
    if (input$random == "No random effects") return("No random effects selected.") else {
      randomtext <- input$random
      rlev <- levels(df[,input$random])
      if (length(rlev) > 10) rlev <- c(rlev[1:10], "...")
      print(paste("<strong>Random effects</strong>:", input$random, "<br/>with levels:", paste(rlev, collapse=",")))
    }
  })  
  
  output$doseresponseplot <- renderPlot({
    df <- ddd()
    if (is.null(df)) return()
    if (input$fixed == "Single fixed effect curve" & input$random == "No random effects"){
      dp <- df[,c(1,2)]
      names(dp) <- c("response", "dose")
      if (input$logcheck){
        dp$dose[dp$dose == 0] <- sort(unique(dp$dose))[2]-(sort(unique(dp$dose))[2]-sort(unique(dp$dose))[1])/2
        return(ggplot(dp, aes(x=dose, y=response)) + geom_point() + ylab(input$response) + xlab(input$dose) + coord_trans(x="log"))
      } else {
        return(ggplot(dp, aes(x=dose, y=response)) + geom_point() + ylab(input$response) + xlab(input$dose))
      }
    }
    if (input$fixed != "Single fixed effect curve"){
      dp <- df[,c(1,2,3)]
      names(dp) <- c("response", "dose", "group")
      if (input$logcheck){
        dp$dose[dp$dose == 0] <- sort(unique(dp$dose))[2]-(sort(unique(dp$dose))[2]-sort(unique(dp$dose))[1])/2
        return(ggplot(dp, aes(x=dose, y=response, colour=group)) + geom_point() + ylab(input$response) + xlab(input$dose) + scale_colour_discrete(input$fixed) + coord_trans(x="log"))
      } else {
        return(ggplot(dp, aes(x=dose, y=response, colour=group)) + geom_point() + ylab(input$response) + xlab(input$dose) + scale_colour_discrete(input$fixed))
      }
    }    
    if (input$fixed == "Single fixed effect curve" & input$random != "No random effects"){
      dp <- df[,c(1,2,3)]
      names(dp) <- c("response", "dose", "group")
      if (input$logcheck){
        dp$dose[dp$dose == 0] <- sort(unique(dp$dose))[2]-(sort(unique(dp$dose))[2]-sort(unique(dp$dose))[1])/2
        return(ggplot(dp, aes(x=dose, y=response, colour=group)) + geom_point() + ylab(input$response) + xlab(input$dose) + scale_colour_discrete(input$random) + coord_trans(x="log"))
      } else {
        return(ggplot(dp, aes(x=dose, y=response, colour=group)) + geom_point() + ylab(input$response) + xlab(input$dose) + scale_colour_discrete(input$random))
      }
    }
  })
  
  output$radio_fixed <- renderUI({
    df <- dat()
    if(is.null(df) || input$model == "" || input$fixed == "Single fixed effect curve") return() 
    c("", "LL.2", "LL.3", "LL.4", "LL.5", "L.3", "L.4", "L.5", "W1.2", "W1.3", "W1.4", "W2.2", "W2.3", "W2.4", "LN.2", "LN.3", "LN.4", "BC.4", "BC.4", "AR.2", "AR.3", "EXD.2", "EXD.3", "MM.2", "MM.3")
    choice <- switch(input$model, 
                     "LL.2" = c("b", "e"),
                     "LL.3" = c("b", "d", "e"),
                     "LL.4" = c("b", "c", "d", "e"),
                     "LL.5" = c("b", "c", "d", "e", "f"), 
                     "L.3"  = c("b", "d", "e"),
                     "L.4"  = c("b", "c", "d", "e"),
                     "L.5"  = c("b", "c", "d", "e", "f"),   
                     "W1.2" = c("b", "e"),
                     "W1.3" = c("b", "d", "e"),
                     "W1.4" = c("b", "c", "d", "e"),
                     "W2.2" = c("b", "e"),
                     "W2.3" = c("b", "d", "e"),
                     "W2.4" = c("b", "c", "d", "e"),    
                     "LN.2" = c("b", "e"),
                     "LN.3" = c("b", "d", "e"),
                     "LN.4" = c("b", "c", "d", "e"),
                     "BC.4" = c("b", "d", "e", "f"),
                     "BC.5" = c("b", "c", "d", "e", "f"), 
                     "AR.2" = c("d", "e"),
                     "AR.3" = c("c", "d", "e"),   
                     "EXD.2" = c("d", "e"),
                     "EXD.3" = c("c", "d", "e"),  
                     "MM.2" = c("d", "e"),
                     "MM.3" = c("c", "d", "e") 
    )
    checkboxGroupInput("fixedcheck", h5(paste("Fixed", input$fixed, "effects")), choices = choice)
  }) 
  
  output$radio_random <- renderUI({
    df <- dat()
    if (is.null(df) || input$model == "" || input$random == "No random effects") return() 
    choice <- switch(input$model, 
                     "LL.2" = c("b", "e"),
                     "LL.3" = c("b", "d", "e"),
                     "LL.4" = c("b", "c", "d", "e"),
                     "LL.5" = c("b", "c", "d", "e", "f"), 
                     "L.3"  = c("b", "d", "e"),
                     "L.4"  = c("b", "c", "d", "e"),
                     "L.5"  = c("b", "c", "d", "e", "f"),   
                     "W1.2" = c("b", "e"),
                     "W1.3" = c("b", "d", "e"),
                     "W1.4" = c("b", "c", "d", "e"),
                     "W2.2" = c("b", "e"),
                     "W2.3" = c("b", "d", "e"),
                     "W2.4" = c("b", "c", "d", "e"),    
                     "LN.2" = c("b", "e"),
                     "LN.3" = c("b", "d", "e"),
                     "LN.4" = c("b", "c", "d", "e"),
                     "BC.4" = c("b", "d", "e", "f"),
                     "BC.5" = c("b", "c", "d", "e", "f"), 
                     "AR.2" = c("d", "e"),
                     "AR.3" = c("c", "d", "e"),   
                     "EXD.2" = c("d", "e"),
                     "EXD.3" = c("c", "d", "e"),  
                     "MM.2" = c("d", "e"),
                     "MM.3" = c("c", "d", "e")     
    )
    checkboxGroupInput("randomcheck", h5(paste("Random", input$random, "effects")), choices = choice)
  }) 
  
  mod <- reactive({
    if (input$action == 0) return()
    isolate({
      df <- ddd()
      if (is.null(df) || input$model == "") return()
      drform <- paste(input$response, "~", input$dose)
      ## no fixed, no random
      if (input$fixed == "Single fixed effect curve" & input$random == "No random effects"){
        m <- eval(parse(text=paste("drm(", drform, ", fct=", input$model, "(), data=df)", sep="")))
        return(m)
      }  
      if (input$fixed != "Single fixed effect curve" & input$random == "No random effects"){
        let <- eval(parse(text=paste(input$model, "()$names", sep="")))
        cin <- let %in% input$fixedcheck
        pm <- character(length=length(let))
        pm[cin] <- paste("~", input$fixed, "-1", sep="")
        pm[!cin] <- "~1"
        pmods <- paste("list(", paste(pm, collapse=","), ")")
        m <- eval(parse(text=paste("drm(", drform, ", fct=", input$model, "(), curveid=",input$fixed ,", data=df, pmodels=", pmods,")", sep="")))
        return(m)
      }    
      if (input$fixed == "Single fixed effect curve" & input$random != "No random effects"){
        ra <- paste(paste(input$randomcheck, collapse="+"), "~ 1|", input$random)
        m <- eval(parse(text=paste("medrm(", drform, ", fct=", input$model, "(), data=df, random=", ra,")", sep="")))
        return(m)
      } 
      if (input$fixed != "Single fixed effect curve" & input$random != "No random effects"){
        fi <- paste(paste(input$fixedcheck, collapse="+"), "~", input$fixed)
        ra <- paste(paste(input$randomcheck, collapse="+"), "~ 1|", input$random)
        m <- eval(parse(text=paste("medrm(", drform, ", fct=", input$model, "(), curveid=",fi ,", data=df, random=", ra,")", sep="")))
        return(m)
      }       
    })
  })
  
  output$modelout <- renderPrint({
    mm <- mod()
    if (is.null(mm)) return()
    isolate({
      wmod <- switch(input$model, 
                     LL.2 = "2 parameter log-logistic model",
                     LL.3 = "3 parameter log-logistic model",
                     LL.4 = "4 parameter log-logistic model",
                     LL.5 = "5 parameter log-logistic model",
                     L.3  = "2 parameter logistic model",
                     L.4  = "2 parameter logistic model",
                     L.5  = "2 parameter logistic model",
                     W1.2 = "2 parameter Weibull function",
                     W1.3 = "3 parameter Weibull function",
                     W1.4 = "4 parameter Weibull function",
                     W2.2 = "2 parameter Weibull function (type 2)",
                     W2.3 = "3 parameter Weibull function (type 2)",
                     W2.4 = "4 parameter Weibull function (type 2)",
                     LN.2 = "2 parameter log-Normal model",
                     LN.3 = "3 parameter log-Normal model",
                     LN.4 = "4 parameter log-Normal model",
                     BC.4 = "4 parameter Brain-Cousens hormesis model",
                     BC.5 = "5 parameter Brain-Cousens hormesis model",
                     AR.2 = "2 parameter asymptotic regression model",
                     AR.3 = "3 parameter asymptotic regression model",
                     EXD.2 = "2 parameter exponential decay model",
                     EXD.3 = "3 parameter exponential decay model",
                     MM.2 = "2 parameter Michaelis-Menten model",
                     MM.3 = "3 parameter Michaelis-Menten model"
                       )
      cat(paste("<h2>", wmod,"</h2><br/>"))
      cat(paste("log-likelihood: ", round(as.numeric(logLik(mm)), 3), "<br/>"))
      cat(paste("AIC: ", round(as.numeric(AIC(mm)), 3), "<br/>"))
      if (input$random == "No random effects"){
        gg <- glht(mm)
      } else {
        gg <- glht(mm$fit)
      }
      sx <- summary(gg)
      pq <- sx$test
      ci <- confint(gg, calpha=univariate_calpha())
      mtests <- cbind(pq$coefficients, pq$sigma, ci$confint[,2:3])
      colnames(mtests) <- c("Estimate", "Standard Error", "lower 2.5% limit", "upper 97.5% limit")
      cat("<h3>Estimated Coefficients:</h3>")
      xtab <- xtable(mtests)
      digits(xtab)[c(2,3,4,5)] <- 3
      print(xtab, type="html")
      if (inherits(mm, "medrc")){
        vc <- VarCorr(mm)
        vctab <- data.frame(matrix(as.numeric(vc), nrow=nrow(vc)))
        rownames(vctab) <- rownames(vc)
        colnames(vctab) <- colnames(vc)
        xvctab <- xtable(vctab)
        digits(xvctab)[c(2,3)] <- 3
        cat("<br/><h3>Variance Components:</h3>")
        print(xvctab, type="html")
      } else {
        cat(paste("<br/>Residual standard error: ", round(summary(mm)$rseMat[1,1], 3), " with ", summary(mm)$rseMat[1,2], " degrees of freedom"))
      }
    })
  })
  
  output$logcheckrandom <- renderUI({
    if (is.null(mod()) || input$random == "No random effects") return() 
    checkboxInput("logcheckrandom", label = "logarithmic x-axis", value = FALSE)
  }) 
  
  output$prediction <- renderPlot({
    if (is.null(mod())) return() 
    if (input$random == "No random effects"){
      plot(mod())
    } else {
      ra <- if (input$fixed == "Single fixed effect curve") TRUE else FALSE
      plot(mod(), logx=input$logcheckrandom, ndose=100, ranef=ra)
    }
  })
  
  output$marginalizeED <- renderUI({
    df <- dat()
    if (is.null(df) || input$model == "" || input$random == "No random effects") return() 
    checkboxInput("marginalED", h5("Calculate marginalized estimates"), value=TRUE)
  }) 
  
  ed <- reactive({
    if (input$actionED == 0) return()
    isolate({
      if (input$random == "No random effects"){
        return(ED(mod(), respLev=as.numeric(input$EDcheck), display=FALSE))
      }
      if (input$marginalED == FALSE){
        return(ED(mod(), respLev=as.numeric(input$EDcheck), display=FALSE))
      } else {
        return(EDmarg(mod(), respLev=as.numeric(input$EDcheck), display=FALSE))
      }
    })
  })
  
  output$EDout <- renderPrint({
    edc <- ed()
    if (is.null(edc)) return()
    isolate({
      edm <- edc$EDmultcomp
      
      if (input$EDcilog == TRUE){
        ## log-scale CI      
        est <- edm$coef
        vest <- edm$vcov
        lest <- log(est)
        lvest <- eval(parse(text=paste("deltamethod(list(", paste(paste("~log(", paste("x", seq(1,length(lest), by=1), sep=""), ")"), collapse=","), "), est, vest, ses=FALSE)")))
        names(lest) <- names(est)
        dimnames(lvest) <- dimnames(vest)
        edm$coef <- lest
        edm$vcov <- lvest
      }
      
      ged <- glht(edm)
      if (input$EDmulti == TRUE){
        edci <- confint(ged, level=input$EDconflevel)
      } else {
        edci <- confint(ged, level=input$EDconflevel, calpha=univariate_calpha())
      }
      edest <- rbind(edci$confint[,1:3])
      if (input$EDcilog == TRUE) edest <- exp(edest) # back transformation from log-scale
      colnames(edest) <- c("Estimate", "lower 2.5% limit", "upper 97.5% limit")
      cat("<h3>Estimated Effective Dose Levels:</h3>")
      xtabed <- xtable(edest)
      digits(xtabed)[c(2,3,4)] <- 3
      print(xtabed, type="html")        
    })
  }) 
  
  
  output$marginalizeBMD <- renderUI({
    df <- dat()
    if (is.null(df) || input$model == "" || input$random == "No random effects") return() 
    checkboxInput("marginalBMD", h5("Calculate marginalized estimates"), value=TRUE)
  }) 
  
  
  bmd <- reactive({
    if (input$actionBMD == 0) return()
    isolate({
      bmdint <- if (input$BMDcilog == TRUE) "tfls" else "delta"
      if (input$random == "No random effects"){        
        return(BMD(mod(), respLev=as.numeric(input$BMDcheck), interval=bmdint, level=input$BMDconflevel, background=input$BMDbackground, bmd=input$BMDdef, display=FALSE))
      }
      if (input$marginalBMD == FALSE){
        return(BMD(mod(), respLev=as.numeric(input$BMDcheck), interval=bmdint, level=input$BMDconflevel, background=input$BMDbackground, bmd=input$BMDdef, display=FALSE))
      } else {
        return(BMDmarg(mod(), respLev=as.numeric(input$BMDcheck), interval=bmdint, level=input$BMDconflevel, background=input$BMDbackground, bmd=input$BMDdef, display=FALSE)[[1]])
      }
    })
  })
 
  output$BMDout <- renderPrint({
    bmdc <- bmd()
    if (is.null(bmdc)) return()
    isolate({      
      cat("<h3>Estimated Benchmark Dose Levels:</h3>")
      xtabmd <- xtable(bmdc)
      digits(xtabmd)[c(2,3,4,5)] <- 3
      print(xtabmd, type="html")        
    })
  }) 
  
  
})