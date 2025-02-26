## Functions for preliminary assessment of dose response data

#' Preliminary Plot 1 for Dose Response Data
#'
#' This function generates a scatter plot of response against nominal dose.
#'
#' @param testdata A data frame containing the dose and response data.
#' @param ylab A string for the y-axis label. Default is "Response".
#' @param xlab A string for the x-axis label. Default is "Test Concentration [nominal, mg a.s./L]".
#' @param title A string for the plot title. Default is "Measured Variable".
#' @return A ggplot object.
#' @import ggplot2
#' @export
prelimPlot1 <- function(testdata,ylab="Response",xlab="Test Concentration [nominal, mg a.s./L]",
                        title="Measured Variable"){
  p <- ggplot(testdata,aes(x=as.character(Dose),y=Response))+geom_point() +
    ylab(ylab) + xlab(xlab)+
    ggtitle(title)
  return(p)
}

#' Preliminary Plot 2 for Dose Response Data
#'
#' This function generates a scatter plot of response against dose on a log1p scale.
#'
#' @param testdata A data frame containing the dose and response data.
#' @param ylab A string for the y-axis label. Default is "Response".
#' @param xlab A string for the x-axis label. Default is "Test Concentration [nominal, mg a.s./L]".
#' @param title A string for the plot title. Default is "Measured Variable".
#' @return A ggplot object.
#' @import ggplot2
#' @import scales
#' @export
prelimPlot2 <- function(testdata,ylab="Response",xlab="Test Concentration [nominal, mg a.s./L]",
                        title="Measured Variable",dosecol="Dose"){
  ilog1p <- function(x) {
    exp(x) - 1
  }
  doses <- sort(unique(testdata[,dosecol]))
  p <- ggplot(testdata,aes(x=Dose,y=Response))+geom_point() + ylab(ylab) +
    xlab(xlab)+ggtitle(title)+
    scale_x_continuous(breaks=doses,trans = scales::trans_new("log1p",log1p,ilog1p))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(p)
}


#' Preliminary Plot 3 for Dose Response Data
#'
#' This function generates a scatter plot of response against nominal dose.
#'
#' @param testdata A data frame containing the dose and response data.
#' @param ylab A string for the y-axis label. Default is "Response".
#' @param xlab A string for the x-axis label. Default is "Test Concentration [nominal, mg a.s./L]".
#' @param title A string for the plot title. Default is "Measured Variable".
#' @param a the quantile for corresponding CI for mean. default is qnorm(0.975).
#' @return A ggplot object.
#' @import ggplot2
#' @import dplyr
#' @export
prelimPlot3 <- function(testdata,ylab="Response",xlab="Test Concentration [nominal, mg a.s./L]",
                        title="Measured Variable",a=1.96){
  p <- prelimPlot1(testdata = testdata,ylab=ylab,xlab=xlab,title=title)
  datsum <- testdata %>% group_by(Dose) %>%
    summarise(mean=mean(Response), SE=sd(Response)/sqrt(length(Response))) %>%
    mutate(Lower=mean-a*SE,Upper=mean+a*SE)
  p <- p + geom_point(data = datsum,aes(as.numeric(as.factor(Dose))+0.15,y=mean),position=position_dodge(width = 0.9),col = "red",size = 3,shape = 24, fill = "pink")+
    geom_errorbar(data = datsum, aes(x=as.numeric(as.factor(Dose))+0.15,y=mean,ymin = Lower, ymax = Upper),col="red",position=position_dodge(width = 0.9),width = .1)
  return(p)
}


#' Preliminary Summary of Dose Response Data
#'
#' This function calculates the mean response, standard deviation, percent inhibition, and coefficient of variation for each dose level.
#'
#' @param testdata A data frame containing the dose and response data.
#' @return A data frame summarizing the mean, standard deviation, percent inhibition, and coefficient of variation for each dose.
#' @import dplyr
#' @export
prelimSummary <- function(testdata){
  ctr <- testdata %>% filter(Dose == 0)
  ctr0 <- mean(ctr$Response)
  sres <- testdata %>% group_by(Dose)%>% dplyr::summarize(Mean=mean(Response),SD=sd(Response)) %>% mutate(`% Inhibition` = - ((Mean-ctr0)/ctr0)*100, CV=SD/Mean*100)
  ## sres %>% knitr::kable(.,digits = 3)
  return(sres)
}
