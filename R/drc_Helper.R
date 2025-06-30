#' adding ECx estimation and interval to the model output plot
#'
#' @param p model plot as ggplot2 output
#' @param object fitted drc object
#' @param EDres EDresults corresponding to respLev
#' @param trend "Decrease" or "Increase"
#' @param endpoint ErC or EbC or EyC or EC or LD
#' @param respLev reponse levels
#' @param textAjust.x  where to label ECx
#' @param textAjust.y where to label ECx
#' @param useObsCtr whether to use observed control mean
#' @param d0 to be used control mean.
#' @param textsize label text size
#' @param lineheight errorbar height
#' @param xmin when confidence intervals very wide including even negative values.
#' @param ... additional inputs passed into ED functions.
#'
#' @return ggplot object with added ECx CIs.
#' @export
#'
#' @examples
#' \dontrun{
#' data("dat_medium")
#' dat_medium <- dat_medium %>% mutate(Treatment=factor(Dose,levels=unique(Dose)))
#' dat_medium$Response[dat_medium$Response < 0] <- 0
#' mod <- drm(Response~Dose,data=dat_medium,fct=LN.4())
#' p1 <- plot.modList(list(mod))
#' addECxCI(p1,object=mod,EDres=NULL,trend="Decrease",endpoint="EC", respLev=c(10,20,50),
#' textAjust.x=0.01,textAjust.y=0.3,useObsCtr=FALSE,d0=NULL,textsize = 4,lineheight = 0.5,xmin=0.012)+
#' ylab("Response Variable [unit]") + xlab("Concentration [µg a.s./L]")
#' }
addECxCI <- function(p = NULL, object, EDres = NULL, trend = "Decrease", endpoint = "ErC", respLev = c(10, 20, 50),
                     textAjust.x = 0.1, textAjust.y = 0.05, useObsCtr = FALSE, d0 = NULL, textsize = 2, lineheight = 1, xmin = 0.05, ...) {
  if (respLev[1] < 1) respLev <- respLev * 100

  if (!inherits(object, "try-error")) {
    type <- object$type
    coefs <- coef(object)
    num <- length(coefs)
    if ("c:(Intercept)" %in% names(coefs)) cVal <- coefs["c:(Intercept)"] else cVal <- 0 ## switch(response,Increase=0,Decrease=1)
    if ("d:(Intercept)" %in% names(coefs)) d <- coefs["d:(Intercept)"] else d <- 1
    # For 3 parameter models, if setting the 4th parameter being 0, then there is no need to change ECx estimation unless the observed control average should be used instead of the estimated control mean.
    ## As shown in the equation below, c=0 as assumed, d is the estimated control mean.
    ## EC_true x=EC_software {((d-(1-x)· mean(y_0 ))⁄((d-c)})
    if (d < cVal) {
      d1 <- d
      d <- cVal
      cVal <- d1
    }
    if (type == "continuous" || type == "Poisson") {
      if (trend == "Increase") {
        ## for example, mortality can increase, however,
        ## the x% effect should be compared to the maximal survival probabilities.
        ## which means, (1-cVal)=max survival probability.
        ## There should be only reduction or inhibition.
        x.relative <- (respLev - cVal * 100) / (d - cVal)
        ### need to think about it!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        warning("If not inhibition or reduction, be careful! ")
      } else {
        if (!useObsCtr) d0 <- d
        x.relative <- d0 * respLev / (d - cVal)
      }
    } else {
      if (type == "binomial") {
        if (num > 2) {
          warning("need more study")
          x.relative <- respLev
        } else {
          x.relative <- respLev
        }
      }
    }
  } else {
    stop("No need to plot as error occured in the model fitting already ")
  }


  if (is.null(EDres)) EDres <- ED.plus(object, trend = trend, respLev = respLev, ...)
  ## browser()
  if (is.null(p)) p <- plot.modList(object, ...)
  if (trend == "Decrease") {
    Effect <- ((1 - respLev / 100) * d)
  } else {
    warning("If not inhibition or reduction, be careful! ")
    Effect <- cVal + respLev / 100 * d ## note, for survival, d=1, for continuous, it is a different topic!!!!
  }
  EDdat <- data.frame(EDres,
                      Effect = Effect,
                      text = paste0(endpoint, respLev)
  )
  EDdat$Lower <- ifelse(EDdat$Lower > xmin, EDdat$Lower, xmin)
  p <- p + geom_point(data = EDdat, aes(x = .data$Estimate, y = .data$Effect), size = 3, col = "red") +
    geom_text(data = EDdat, aes(x = .data$Estimate + textAjust.x, y = .data$Effect + textAjust.y, label = text), size = textsize, col = "darkgreen") +
    geom_errorbarh(data = EDdat, aes(x = .data$Estimate, y = .data$Effect,
                                     xmin = .data$Lower, xmax = .data$Upper, height = lineheight), lty = 2)
  return(p)
}

#' Calculating ED following the regulatory ED definition.
#'
#' @param object fitted model using drc::drm
#' @param respLev response level, the x of ECx.
#' @param maxEff Not used for now. maximum effect.
#' @param trend Increase of Decrease, whether the dose response is decreasing or increasing compared control.
#' @param range using Percentage, not used for now.
#' @param CI methods to calculate the confidence intervals, either "inv", "bmd-inv",(bmd) or "delta" or "fls".
#' @param ... other parameters for ED function
#'
#' @return Back calculated regulatory ECx
#' @export ED.plus
#' @rdname ED.plus
#' @importFrom bmd bmd
#'
#' @examples
#' \dontrun{
#' data("dat_medium")
#' dat_medium <- dat_medium %>% mutate(Treatment=factor(Dose,levels=unique(Dose)))
#' dat_medium$Response[dat_medium$Response < 0] <- 0
#' mod <- drm(Response~Dose,data=dat_medium,fct=LL.3())
#' fctList <- list(LN.4(),LL.4(),W1.3(),LL2.2())
#' res <- mselect.plus(mod,fctList = fctList )
#' modList <- res$modList
#' mod <-modList[[1]]
#' edres <- ED.plus(mod,c(10,50),trend="Decrease")
#' }
ED.plus <- function(object, respLev, maxEff = TRUE, trend = "Increase", range = "Percentage", CI = c("delta", "inv", "bmd-inv"), ...) {
  ## Note that this might not be suitable for models with fixed c or d parameters, where
  CI <- match.arg(CI)

  if (!inherits(object, "try-error")) {
    if (CI == "delta") {
      type <- object$type
      coefs <- coef(object)
      num <- length(coefs)
      if (grepl("log(ED50) as parameter", object$fct$text, fixed = TRUE)) {
        if (CI == "delta") interval <- "fls"
        if (CI == "inv") interval <- "bmd-inv"
        ## browser()
      } else {
        interval <- CI
      } ## could be "delta" or "inv"
      if ("c:(Intercept)" %in% names(coefs)) cVal <- coefs["c:(Intercept)"] else cVal <- 0 ## switch(response,Increase=0,Decrease=1)
      if ("d:(Intercept)" %in% names(coefs)) d <- coefs["d:(Intercept)"] else d <- 1
      # For 3 parameter models, if setting the 4th parameter being 0, then there is no need to change ECx estimation unless the observed control average should be used instead of the estimated control mean. As shown in the equation below, c=0 as assumed, d is the estimated control mean. EC_true x=EC_software {((d-(1-x)· mean(y_0 ))⁄((d-c)})
      if (d < cVal) {
        print("check this case: using decay model to fit growth or vice versa.") ## using decay model to fit growth or vice versa.
        d1 <- d
        d <- cVal
        cVal <- d1
        # browser()
      }
      if (type == "continuous" || type == "Poisson") {
        if (trend == "Increase") {
          x.relative <- (respLev - cVal * 100) / (d - cVal)
        } else {
          ## x.relative <- d-(1-respLev/100)*y0/(d-c)
          x.relative <- d * respLev / (d - cVal)
        }
      } else {
        if (type == "binomial") {
          if (num > 2) {
            warning("need more tests!!!, survival as response should be fine, but not mortality as response")
            x.relative <- respLev
          } else {
            x.relative <- respLev
          }
        }
      }
      ## Add a step to check if x.relative is out of range!
      id <- (x.relative > 100)
      if (all(id)) {
        res <- matrix(NA, length(respLev), 4) #
        colnames(res) <- c("Estimate", "Std. Error", "Lower", "Upper")
      } else {
        res <- drc::ED(object, respLev = x.relative[!id], display = FALSE, interval = interval, ...)
        if (sum(id) > 0) {
          tmp <- matrix(NA, (sum(id)), ncol(res))
          ## browser()
          res <- rbind(res, tmp)
        }
      }
    } else {
      requireNamespace("bmd", quietly = TRUE)
      # tmp <- lapply(cross2(respLev,modList),function(l){
      #   x <- l[[1]]
      #   y <- l[[2]]
      #   res <- try(bmd(y,x/100,backgType="modelBased",def="relative",interval="inv",display=F))
      #   if(!inherits(res,"try-error")){
      #     return(c(res$Results[1],res$SE,res$interval))
      #   }else{
      #     return(rep(NA,4))
      #     }
      #
      # })
      tmp <- lapply(respLev, function(l) {
        x <- l[[1]]
        y <- object
        if (object$type == "binomial") {
          ## browser()
          res <- try(bmd::bmd(y, x / 100, backgType = "modelBased", def = "excess", interval = "delta", display = F),silent = TRUE)
        } else {
          res <- try(bmd::bmd(y, x / 100, backgType = "modelBased", def = "relative", interval = "inv", display = F),silent = TRUE)
        }
        if (!inherits(res, "try-error")) {
          return(c(res$Results[1], res$SE, res$interval))
        } else {
          return(rep(NA, 4))
        }
      })
      res <- plyr::ldply(tmp)
      colnames(res) <- c("Estimate", "Std. Error", "Lower", "Upper")
    }
  } else {
    res <- matrix(NA, length(respLev), 4) #
    colnames(res) <- c("Estimate", "Std. Error", "Lower", "Upper")
  }

  rownames(res) <- paste("EC", respLev)
  return(res)
}


#' Get Model Name
#'
#' @param fname a character string contains the name of the fitted model.
#'
#' @return the model name
#' @importFrom drc getMeanFunctions
#' @export
#'
#' @examples
#' getModelName("LL.2")
#'
getModelName <- function(fname = NULL) {
  if (any(grepl("EXD", fname))) { ## for now just one character
    noParm <- stringr::str_split(fname, stringr::fixed("."))[[1]][2]
    ModelName <- paste0(noParm, "-parameter exponential decay model")
  } else {
    ModelName <- getMeanFunctions(fname = fname)
    ModelName <- paste0(ModelName[[1]]$noParm, "-parameter ", ModelName[[1]]$text)
  }
  return(ModelName)
}
#' Deprecated helper function for ED calculation
#'
#' @rdname ED.plus
#' @details
#' Due to old ECxHelper development where ED.plus is defined as ED.ZG
#' @seealso [ED.plus()] for the public version of usage
#' @export ED.ZG
#' @keywords Deprecated
ED.ZG <- ED.plus

#' added functionality for mselect
#'
#' @param object a fitted object of class 'drc'.
#' @param fctList a list of dose-response functions to be compared.
#' @param nested logical. TRUE results in F tests between adjacent models (in 'fctList'). Only sensible for nested models.
#' @param sorted character string determining according to which criterion the model fits are ranked, default is IC.
#' @param linreg logical indicating whether or not additionally polynomial regression models (linear, quadratic, and cubic models) should be fitted (they could be useful for a kind of informal lack-of-test consideration for the models specified, capturing unexpected departures).
#' @param icfct function for supplying the information criterion to be used. AIC and BIC are two options.
#' @param respCol name of the response column
#' @param doseCol name of the dose column
#' @param data data used
#' @param type type of models, binomial, continuous, etc.
#' @param additionalReliability additional reliability need to be calculated
#' @importFrom stats AIC BIC
#' @return a model comparison object with class drcComp
#' @export
#' @rdname mselect.plus
#'
#' @examples
#' \dontrun{
#' data("dat_medium")
#' dat_medium <- dat_medium %>% mutate(Treatment=factor(Dose,levels=unique(Dose)))
#' dat_medium$Response[dat_medium$Response < 0] <- 0
#' mod <- drm(Response~Dose,data=dat_medium,fct=LL.3())
#' fctList <- list(LN.4(),LL.4(),W1.3(),LL2.2())
#' res <- mselect.plus(mod,fctList = fctList )
#' modList <- res$modList
#' }
mselect.plus <- function(object = NULL, fctList = NULL, nested = FALSE,
                         sorted = c("IC", "Res var", "Lack of fit", "no"),
                         linreg = FALSE, icfct = AIC, respCol = "effect",
                         doseCol = "dose", data = NULL, type = "continuous",
                         additionalReliability = c("EFSA")) {
  sorted <- match.arg(sorted)
  if (!is.logical(nested)) {
    stop("'nested' argument takes only the values: FALSE, TRUE")
  }
  contData <- identical(object$type, "continuous")

  mc <- match.call()

  ## If continuos data, we need to calculate ResVar, if nested, we need to do an ANOVA step.
  fctNames <- sapply(fctList, function(f) f$name)

  if (is.null(object)) {
    if (is.null(data)) {
      stop("No Data Input, please check your input parameters")
    }
    modList <- lapply(fctList, function(fct) {
      try(drc::drm(data[["response"]] ~ data[["dose"]], fct = fct, type = type, control = drc::drmc(errorm = T)), silent = T)
    })
  } else {
    modList <- lapply(fctList, function(fct) {
      try(update(object, fct = fct),
          silent = TRUE
      )
    })
    modList[[length(fctList) + 1]] <- object
    ## fctList[[length(fctList)+1]] <- mod$fct possbile bug
    fctList[[length(fctList) + 1]] <- object$fct ## not necessary!!
  }
  class(modList) <- "modList"
  Comparison <- plyr::laply(modList, function(tempObj) {
    if (!inherits(tempObj, "try-error")) {
      criteria <- c(logLik(tempObj), icfct(tempObj), drc::modelFit(tempObj)[2, 5])
      if (contData) {
        tryRV2 <- try(summary(tempObj)$resVar, silent = TRUE)
        if (inherits("tryRV2", "try-error")) {
          tryRV2 <- NA
        }

        criteria <- c(criteria, tryRV2)
      }
    } else {
      print("Model not Converged, Please consult a statistician.")
      if (contData) criteria <- rep(NA, 4) else criteria <- rep(NA, 3)
    }
    return(criteria)
  })


  rownames(Comparison) <- sapply(fctList, function(f) f$name)
  cnames <- c("logLik", "IC", "Lack of fit")
  if (contData) {
    cnames <- c(cnames, "Res var")
  }

  colnames(Comparison) <- cnames
  if (linreg) {
    if (is.null(object)) {
      object <- modList[[1]]
    }
    drcData <- as.data.frame(object$data[, c(2, 1)])
    names(drcData) <- c("yVec", "xVec")
    linFitList <- list(lm(yVec ~ xVec, data = drcData), lm(yVec ~
                                                             xVec + I(xVec * xVec), data = drcData), lm(yVec ~
                                                                                                          xVec + I(xVec * xVec) + I(xVec * xVec * xVec), data = drcData))
    linModMat <- matrix(unlist(lapply(linFitList, function(listObj) {
      c(logLik(listObj), icfct(listObj), NA, (summary(listObj)$sigma)^2)
    })), 3, 4, byrow = TRUE)
    rownames(linModMat) <- c("Lin", "Quad", "Cubic")
    colnames(linModMat) <- cnames[1:4]

    Comparison <- rbind(Comparison, linModMat)
  }
  names(modList) <- rownames(Comparison)
  if("EFSA" %in% additionalReliability){
    EFSA <- NA ## a placeholder right now
  }
  if("BMD GD" %in% additionalReliability){
    BMD <- NA
  }
  if (sorted != "no") {
    res <- list(Comparison = Comparison[order(Comparison[, sorted]), ], modList = modList[order(Comparison[, sorted])], EFSA = EFSA)
  } else {
    res <- (list(Comparison = Comparison, modList = invisible(modList), EFSA = EFSA))
  }

  class(res) <- "drcComp"
  return(res)
}

#' Deprecated helper function for mselect
#' @rdname mselect.plus
#' @details
#' Due to old ECxHelper development where mselect.plus is defined as mselect.ZG
#'
#' @export mselect.ZG
#' @keywords Deprecated
mselect.ZG <- mselect.plus


#' Print Method for drcComp Objects
#'
#' This function provides a custom print method for objects of class `drcComp`.
#' It prints the `Comparison` and `EFSA` components of the object.
#'
#' @param x An object of class `drcComp`.
#' @param ... Additional arguments (currently not used).
#'
#' @return The function prints the `Comparison` and `EFSA` components
#'  of the object by default.
#' @export
print.drcComp <- function(x, ...) {
  x <- x[c("Comparison", "EFSA")]
  NextMethod()
}


#' Plot a list of models together.
#'
#' @param x a list of drc models with modList class.
#' @param respLev to be calculated ECx levels, for example, c(10,20,50).
#' @param data data used to fit the set of models
#' @param xmin minimum value of xaxis, should be greater than 0 to
#'  fit the log scale.
#' @param xmax maximum value of xaxis, can be missing.
#' @param scale could be "logx", "logy", "logxy", "orig".
#' @param npts number of points used for plotting the prediction
#'  confidence bands
#' @param plot_respLev logical, whether to add the estimated ECx with CIs
#' @param xbreaks breaks in x-axis labeling
#' @param ymin minimum of y
#' @param ymax max of y
#' @param ... addition parameters to be passed into plotting functions or
#' user input when the use does not want to use the
#' data provided in the modList
#'
#' @return a ggplot object
#' @export plot.modList
#'
#' @examples
#' \dontrun{
#' data("dat_medium")
#' dat_medium <- dat_medium %>% mutate(Treatment=factor(Dose,levels=unique(Dose)))
#' dat_medium$Response[dat_medium$Response < 0] <- 0
#' mod <- drm(Response~Dose,data=dat_medium,fct=LL.3())
#' fctList <- list(LN.4(),LL.4(),W1.3(),LL2.2())
#' res <- mselect.plus(mod,fctList = fctList )
#' modList <- res$modList
#' p <- plot.modList(modList[1:3])
#' p
#' }
plot.modList <- function(x, respLev = NULL, data = NULL,
                         xmin, xmax, scale = c("logx", "logy", "logxy", "orig"),
                         npts = 100, plot_respLev = FALSE, xbreaks = NULL,
                         ymin = NULL, ymax = NULL, ...) {
  modList <- x
  if(!is.list(modList)) modList <- list(modList)
  if (is.null(data)) {
    mod1 <- modList[[1]]
    i <- 1
    while (inherits(mod1, "try-error")) {
      mod1 <- modList[[i + 1]]
      i <- i + 1
    }
    data <- mod1$origData
    responseName <- mod1$dataList$names$orName
    doseName <- mod1$dataList$names$`dName`
    dose <- mod1$dataList$`dose`
    response <- mod1$dataList$origResp
  } else {
    print("need user input of responseName and doseName")
    if (missing(doseName)) stop("need user input of responseName and doseName!")
  }
  ## fctNames <- names(modList)
  ## Calculate Prediction and Confidence Bands
  ex1 <- data.frame(data)
  ex1$conc0 <- pull(data, doseName)
  if (missing(xmin)) xmin <- min(ex1$conc0[ex1$conc0 > 0]) / 10
  ex1$conc0[ex1$conc0 == 0] <- xmin
  ex1$response <- pull(data, responseName)
  if (missing(xmax)) xmax <- max(ex1[, doseName]) * 1.1
  if (is.null(xbreaks)) {
    DoseData <- data.frame(Dose = exp(seq(log(xmin), log(xmax), length = npts)))
  } else {
    DoseData <- data.frame(Dose = xbreaks)
  }
  names(DoseData) <- doseName
  predData <- plyr::ldply(modList, function(tempObj) {
    if (!inherits(tempObj, "try-error")) {
      pm <- predict(tempObj,
                    newdata = DoseData,
                    interval = "confidence", ...
      )
      newdata <- DoseData
      newdata$p <- pm[, 1]
      newdata$pmin <- pm[, 2]
      newdata$pmax <- pm[, 3]
      newdata$Model <- tempObj$fct$name
      if (!is.null(ymin)) newdata$pmin[newdata$pmin < ymin] <- ymin
      if (!is.null(ymax)) newdata$pmax[newdata$pmax > ymax] <- ymax
    } else {
      newdata <- NULL
    }
    return(newdata)
  })
  predData$Model <- factor(predData$Model, levels = unique(predData$Model))
  requireNamespace("ggplot2", quietly = TRUE)
  p <- ggplot(ex1, aes_(x = ~conc0, y = as.name(responseName))) +
    geom_ribbon(data = predData, aes_(x = as.name(doseName),
                                      y = ~p, ymin = ~pmin,
                                      ymax = ~pmax, col = ~Model,
                                      fill = ~Model),
                alpha = 0.2) +
    xlab(doseName) +
    ylab(responseName) +
    geom_line(data = predData, aes_(x = as.name(doseName), y = ~p,
                                    col = ~Model)) +
    geom_point()

  if (!is.null(respLev) & plot_respLev) {
    y0 <- mean(response[dose == 0]) ## calculate the control mean
    p <- p + geom_hline(yintercept = y0 * (100 - respLev) / 100, lty = 2)
  }
  scale <- match.arg(scale)
  if (scale == "logx") p <- p + scale_x_log10()
  if (scale == "logy") p <- p + scale_y_log10()
  if (scale == "logxy") p <- p + scale_x_log10() + scale_y_log10()
  return(p)
}

#' Plot the ECx estimation and confidence intervals from the list of models.
#'
#' @param edList ECx lists from fitted set of models
#' @param fctNames fct function names
#' @param ... additional parameters
#'
#' @return a ggplot object
#' @export plot_edList
plot_edList <- function(edList, fctNames, ...) {
  if (!is.data.frame(edList)) {
    edResTab <- plyr::ldply(lapply(edList, function(x) {
      x <- as.data.frame(x)
      if (ncol(x) == 3) {
        x$`Std. Error` <- rep(NA, nrow(x))
        x <- x[, c(1, 4, 2, 3)]
      }
      x$ED <- rownames(x)
      rownames(x) <- NULL
      x
    }))
    if (is.null(names(edList))) {
      edResTab$Model <- fctNames
    } else {
      edResTab$Model <- names(edList)
    }
  } else {
    ## Need to check if edRes Tab
    edResTab <- edList
    edResTab$Model <- edResTab$.id
  }
  edResTab$Model <- factor(edResTab$Model, levels = unique(edResTab$Model))
  if (is.null(edResTab$Rating)) {
    plotED <- ggplot(edResTab, aes(x = .data$ED, y = .data$Estimate, col = .data$Model)) +
      geom_pointrange(aes(ymin = .data$Lower, ymax = .data$Upper),
                      position = position_dodge(width = 0.4)
      ) #+scale_y_log10()
  } else {
    edResTab$Rating <- factor(edResTab$Rating, levels = c("Excellent", "Good", "Fair", "Poor", "Bad"))

    plotED <- ggplot(edResTab, aes(x = .data$EC, y = .data$Estimate,
                                   col = .data$Model, shape = .data$Rating)) +
      geom_pointrange(aes(ymin = .data$Lower, ymax = .data$Upper),
                      position = position_dodge(width = 0.4)
      ) #+scale_y_log10()
  }
  return(plotED)
}
## Steepness and overlap
#' Steepness and overlap calcuation
#'
#' @param mod fitted object from drm model fitting
#' @param obj Calculated ED 10, 20, 50 object if available.
#' mod should set to be NULL in this case
#' @param trend "Decrease" or "Increase"
#' @param CI method to calculate CI, either "inv" (bmd) or "delta" or "fls".
#' @param ... other parameters that will be passed into ED.plus
#'
#' @return a table with certainty of potection level and steepness of the models
#' @export
#'
#' @examples \dontrun{
#'  datTn<- subset(oecd201,Time==72)
#'  mod <- drm(Yield~Concentration,data=datTn,fct=LL.3())
#'  calcSteepnessOverlap(mod = mod, trend = "Decrease")
#' }
calcSteepnessOverlap <- function(mod = NULL, obj = NULL, trend = "Decrease", CI = "inv", ...) { # nolint: line_length_linter.
  # |Overlapping  Conditions                  |Certainty of the Protection Level | # nolint
  # |-----------------------------------------|----------------------------------| # nolint
  # |EC$_{10}$ < EC$_{20,low}$                | High                             | # nolint
  # |EC$_{20,low}$ < EC$_{10}$ < EC$_{50,low}$| Medium                           | # nolint
  # |EC$_{10}$ > EC$_{50,low}$                | Low                              | # nolint
  res <- rep(NA, 2)
  if (!is.null(mod) & !inherits(mod, "try-error")) {
    if (inherits(mod, "drc") ) obj <- try(ED.plus(mod, c(10, 20, 50), trend = trend, CI = CI, ...),silent = TRUE)
    obj <- as.data.frame(obj)
    steep <- obj$Estimate[1] / obj$Estimate[3] # the Ratio between EC 10 and EC 50
    if (!is.na(steep)) {
      if (steep < 0.33) {
        res[2] <- "Shallow"
      } else {
        if (steep < 0.66) res[2] <- "Medium" else res[2] <- "Steep"
      }
    } else {
      res[2] <- "Not Defined"
    }
    if (nrow(obj) != 3) {
      print("Without EC50, not able to calculate Steepness")
    } else {
      if (!is.na(obj$Lower[2]) & !is.na(obj$Estimate[1])) {
        if (obj$Estimate[1] < obj$Lower[2]) {
          res[1] <- "High"
        } else {
          if (!is.na(obj$Lower[3])) {
            if (obj$Estimate[1] < obj$Lower[3]) res[1] <- "Medium" else res[1] <- "Low"
          } else { ## cannot compare due to missing EC 50 lower limits.
            ## Assuming that EC50 cannot be estimated since it is extrapolated
            res[1] <- "Medium (missing LCI of EC50)"
          }
        }
      } else {
        res[1] <- "Not Defined"
      }
    }
    return(res)
  } else {
    return(res)
  }
}

#' ECx calculation together with normalized width proposed by EFSA SO.
#'
#' @param modList list of models
#' @param respLev response levels or ECx for example, 10, 20, 50
#' @param trend "Decrease" or "Incrrease".
#' @param ... optional to pass to ED function.
#'
#' @return ED result table
#' @export mselect.ED
#' @importFrom purrr map2
#'
mselect.ED <- function(modList, respLev = c(10, 20, 50),
                       trend = "Decrease", ...) {
  edList <- lapply(modList, function(obj) {
    ED.plus(obj,
            respLev,
            trend = trend, ...) })
  ## map2(modList,edList,function(x,y){
  ## predict(x,newdata=data.frame(Concentration=y[,1]))})
  ## Check if Prediction Make sense

  NW <- lapply(edList, calcNW)
  edRes <- purrr::map2(edList, NW, cbind)


  edResTab <- plyr::ldply(lapply(edRes, function(x) {
    x <- as.data.frame(x)
    if (ncol(x) == 3) {
      x$`Std. Error` <- rep(NA, nrow(x))
      x <- x[, c(1, 4, 2, 3)]
    }
    x$EC <- rownames(x)
    rownames(x) <- NULL
    x
  }))
  return(edResTab)
}


#' Calculate Normalized Width for Effect Concentration Estimates
#'
#' @description
#' Calculates the normalized width of confidence intervals for effect concentration
#' (EC) estimates by dividing the interval width by the estimate value. Also
#' provides a rating for the calculated normalized width.
#'
#' @param x A data frame containing EC estimates, typically output from ED
#'   function. Must contain columns 'Upper', 'Lower', and 'Estimate'.
#' @param ED Character string indicating the source of EC estimates. Must be either
#'   "ZG" (default) or "drc".
#'
#' @return A data frame with two columns:
#'   \itemize{
#'     \item NW: Normalized width calculated as (Upper - Lower) / Estimate
#'     \item Rating: Categorical rating of the normalized width
#'   }
#'   Row names are preserved from input for "ZG" or modified to "EC_x" format
#'   for "drc".
#'
#' @export
#'
#' @examples
#' # Example with ZG format
#' ec_data <- data.frame(
#'   Lower = c(1.2, 2.3),
#'   Upper = c(1.8, 3.1),
#'   Estimate = c(1.5, 2.7),
#'   row.names = c("EC10", "EC50")
#' )
#' calcNW(ec_data, ED = "ZG")
#'
#' # Example with drc format
#' drc_data <- data.frame(
#'   Lower = c(1.2, 2.3),
#'   Upper = c(1.8, 3.1),
#'   Estimate = c(1.5, 2.7),
#'   row.names = c("Dose:1:10", "Dose:1:50")
#' )
#' calcNW(drc_data, ED = "drc")
#'
#' @seealso \code{\link{ECx_rating}} for the rating system used
calcNW <- function(x, ED = "ZG") {
  x <- as.data.frame(x) ## x is the ED object from ED function.
  out <- as.data.frame((x$Upper - x$Lower) / x$Estimate)
  if (ED == "ZG") newRowNames <- rownames(x)
  if (ED == "drc") newRowNames <- paste0("EC_", do.call(c, lapply(strsplit(row.names(x), ":"), function(y) y[3]))) # nolint: line_length_linter.
  out <- cbind(out, NA)
  out[, 2] <- ECx_rating(out[, 1])
  row.names(out) <- newRowNames
  names(out) <- c("NW", "Rating")
  return(out)
}

#' Rate Effect Concentration Estimates Based on Normalized Width
#'
#' @description
#' Assigns qualitative ratings to normalized width values of confidence intervals
#' based on predefined thresholds.
#'
#' @param x Numeric vector of normalized width values to be rated
#'
#' @return Character vector of ratings with the following categories:
#'   \itemize{
#'     \item "Excellent" for NW < 0.2
#'     \item "Good" for 0.2 <= NW < 0.5
#'     \item "Fair" for 0.5 <= NW < 1
#'     \item "Poor" for 1 <= NW < 2
#'     \item "Bad" for NW >= 2
#'     \item "Not defined" for NA or NaN values
#'   }
#'
#' @export
#'
#' @examples
#' # Basic examples
#' ECx_rating(c(0.1, 0.3, 0.7, 1.5, 2.5, NA))
#'
#' # Example with typical normalized widths
#' nw_values <- c(0.15, 0.45, 0.95, 1.8, 2.5)
#' ECx_rating(nw_values)
ECx_rating <- function(x) {
  ## normalised width (NW) of CI
  ## NW   |  Rating # nolint: commented_code_linter.
  ## ----------------
  ## <0.2 | Excellent
  ## <0.5 | Good
  ## <1   | Fair
  ## <2   | Poor
  ## >2   | Bad
  outRating <- NULL
  for (i in x) {
    if (is.nan(i) || is.na(i)) {
      outRating <- c(outRating, "Not defined")
    } else {
      if (i < 0.2) {
        outRating <- c(outRating, "Excellent")
      } else if (i < 0.5) {
        outRating <- c(outRating, "Good")
      } else if (i < 1) {
        outRating <- c(outRating, "Fair")
      } else if (i < 2) {
        outRating <- c(outRating, "Poor")
      } else if (i >= 2) {
        outRating <- c(outRating, "Bad")
      } else {
        outRating <- c(outRating, "Not defined")
      }
    }
  }
  return(outRating)
}

#' Compare Dose-Response Models Using Multiple Criteria
#'
#' @description
#' Performs comprehensive comparison of dose-response models using multiple
#' criteria including model fit, certainty of protection, and steepness.
#'
#' @param modRes Optional. Result object containing model comparison information.
#'   If provided, modList parameter is ignored.
#' @param modList Optional. List of fitted dose-response models to compare.
#'   Required if modRes is NULL.
#' @param trend Character string specifying the expected trend direction.
#'   Must be "Decrease" or "Increase".
#' @param CI Character string specifying the confidence interval method.
#'   One of "delta", "inv", or "bmd-inv". Defaults to "delta".
#' @param ... Additional arguments passed to internal functions
#'
#' @return A data frame containing model comparison results with columns:
#'   \itemize{
#'     \item Original comparison metrics
#'     \item Certainty_Protection: Measure of protection certainty
#'     \item Steepness: Model steepness evaluation
#'     \item No Effect p-val: P-value for test against no-effect model
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("dat_medium")
#' dat_medium <- dat_medium %>% mutate(Treatment=factor(Dose,levels=unique(Dose)))
#' dat_medium$Response[dat_medium$Response < 0] <- 0
#' mod <- drm(Response~Dose,data=dat_medium,fct=LL.3())
#' fctList <- list(LN.4(),LL.4(),W1.3(),LL2.2())
#' res <- mselect.plus(mod,fctList = fctList )
#' modList <- res$modList
#'
#' # Compare models using existing comparison results
#' drcCompare(modRes = res, trend = "Decrease")
#' }
drcCompare <- function(modRes = NULL, modList = NULL, trend = "Decrease",
                       CI = c("delta", "inv", "bmd-inv"), ...) { # nolint
  CI <- match.arg(CI)
  if (!is.null(modRes)) {
    ## take the comarison matrix from modRes
    modList <- modRes$modList
    comparison <- modRes$Comparison
  } else {
    if (is.null(modList)) stop("Need the model output list from previous step!") else{
      ## use modList directly
      stop("A direct model comparison has not yet implemented, this is a placeholder.")
    }
  }
  # A significance test is provided for the comparison of the dose-response model considered and the simple linear regression model with slope 0 (a horizontal regression line corresponding to no dose effect) # nolint: line_length_linter.
  # The likelihood ratio test statistic and the corresponding degrees of freedom and p-value are reported. # nolint: line_length_linter.
  a <- plyr::ldply(modList, function(obj) if (inherits(obj, "try-error")) c(`Chi-square test` = NA, Df = NA, `p-value` = NA) else drc::noEffect(obj)) # nolint: line_length_linter.
  Overlap <- plyr::ldply(modList, calcSteepnessOverlap, CI = CI)
  names(Overlap) <- c(".id", "Certainty_Protection", "Steepness")
  CompRes <- cbind(comparison, Overlap[, -1])
  CompRes <- cbind(CompRes, a[, "p-value"])
  colnames(CompRes)[ncol(CompRes)] <- "No Effect p-val"
  return(CompRes)
}

#' Helper function for scaling x-axis
#'
#' @param x a vector of numeric
#' @param a a vector of nummeric
#'
#' @return log(x+a)
#' @export
#'
#' @examples logxp(0, 1)
logxp <- function(x, a) {
  log(a + x)
}

#' inverse transformation of logxp
#'
#' @param x a numeric vector
#' @param a a numeric vector
#'
#' @return inverse log of x and a
#' @export
#'
#' @examples invlogxp(3, 1)
invlogxp <- function(x, a) {
  exp(x) - a
}
## Example definition of a new transformation in ggplot
## logxp_trans <- function(a=0.001) scales::trans_new("logxp",
## transform=function(x)logxp(x,a=a),
## inverse=function(x)invlogxp(x,a=a))


