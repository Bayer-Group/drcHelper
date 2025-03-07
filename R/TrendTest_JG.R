## Note this script is adapted from the archived version of StatCharrms developed by Joe Swintek [aut, cre],
## Kevin Flynn[ctb] and Jon Haselman [ctb]. StatCharrms_0.90.1.tar.gz	2017-05-08 15:23	964K(https://cran.r-project.org/src/contrib/Archive/StatCharrms/StatCharrms_0.90.1.tar.gz)
## Original package licence is License: CC0, which means no copyrights reserved. However,
## the authors and the archive link are kept in this file so the source can be traced back.

## Unit Testing: test_TrendTeset.R


#' Testing Monotonicity
#'
#' The function is adapted from the archived version of StatCharrms developed by
#' Joe Swintek et al with CC0 license. It is not updated anymore and included
#' for validation purpose. There are other ways to perform a trend test.
#' This function tests whether a dose-response relationship follows a monotonic
#' trend (consistently increasing or decreasing) by analyzing both linear and
#' quadratic components of the relationship.
#'
#' @param Data Data frame
#' @param Treatment name of the treatment variable
#' @param Response name of the response variable
#'
#' @return monotonicity table
#' @importFrom stats contrasts<-
#' @export
#' @details
#' The test first applies a rank transformation to the response variable using
#' the rankTransform function, which implements Blom's method (equivalent to
#' SAS PROC RANK with NORMAL=BLOM). Next it creates linear and quadratic
#' contrasts using the getLineContrast and getQuadContrast functions. Then it
#' fits an ANOVA model using the transformed response variable and extracts
#' the summary statistics from the ANOVA model.
#'
#' Interpretations:
#' If only the linear component is significant: Strong evidence for monotonicity.
#' If both linear and quadratic components are significant: The relationship is
#' monotonic but with curvature. If only the quadratic component is significant:
#' The relationship is likely non-monotonic (e.g., U-shaped). If neither
#' component is significant: No clear dose-response relationship detected
#'
#' Note that contrasts<- need to be imported from stats.This is a function
#' that is used to set contrasts on factors.
#'
#' @examples
#' \dontrun{
#' x <- c(106, 114, 116, 127, 145,110, 125, 143, 148, 151,
#' 136, 139, 149, 160, 174)
#' g <- gl(3,5)
#' levels(g) <- c("0", "I", "II")
#' monotonicityTest(data.frame(treatment_var = g,response_var=x),
#' "treatment_var", "response_var")
#' mock_data <- data.frame(
#' treatment_var = factor(rep(c("Control", "Dose1", "Dose2", "Dose3"), each = 10)),
#' response_var = c(rnorm(10, mean = 5), rnorm(10, mean = 7),
#' rnorm(10, mean = 8), rnorm(10, mean = 10))
#' )
#' monotonicityTest(mock_data, "treatment_var", "response_var")
#' }
monotonicityTest <-function(Data,Treatment,Response){
  #' @export
  #This is the test for monotonicity as done in the SAS Version
  #it sets up linear and Quadratic contrast for an ANOVA
  #Uses .stdEndEnv$WeightVar
  Data <- as.data.frame(Data)
  #Transform data for rank response
  Data<-rankTransform(Data,Response)


  #Form Contrasts
  LineContrast<-getLineContrast(Data,Treatment)
  QuadContrast<-getQuadContrast(Data,Treatment)
  Contrasts <- cbind(LineContrast,QuadContrast)
  colnames(Contrasts)<-c('Line','QuadContrast')

  #ANOVA
  Data[ ,Treatment]<-as.factor(Data[ ,Treatment])
  contrasts(Data[ ,Treatment])<-Contrasts
  AnovaTable<-aov(Data[ ,'TransformedResponse']~as.factor(Data[ ,Treatment]))
  # if (is.null(.stdEndEnv$WeightVar)==FALSE){  #if there is a weight and the data is not averaged
  #   if (length(.stdEndEnv$WeightVar)==length(Data[ ,Response])){
  #     AnovaTable<-aov(Data[ ,'TransformedResponse']~as.factor(Data[ ,Treatment]), weight=.stdEndEnv$WeightVar )
  #   }
  # }
  #
  #gather information and clean the table
  CAnova<-summary.lm(AnovaTable)
  MonocityTable<-as.data.frame(CAnova$coefficients[2:3,3:4])
  rownames(MonocityTable)<-NULL
  MonocityTable<-cbind(c('Linear','Quadratic'),MonocityTable,'.')
  colnames(MonocityTable)[1]<-'Test'
  colnames(MonocityTable)[4]<-'Significance'
  MonocityTable$Significance<-as.character(MonocityTable$Significance)

  MonocityTable$Significance[MonocityTable[ ,'Pr(>|t|)']<0.05]<-'*'
  MonocityTable$Significance[MonocityTable[ ,'Pr(>|t|)']<0.01]<-'**'
  MonocityTable$Significance[MonocityTable[ ,'Pr(>|t|)']<0.001]<-'***'

  MonocityTable[ ,'Pr(>|t|)']<-round(MonocityTable[ ,'Pr(>|t|)'],4)
  if (length(which(MonocityTable[ ,'Pr(>|t|)']<10^-4))>0){
    MonocityTable[which(MonocityTable[ ,'Pr(>|t|)']<10^-4),'Pr(>|t|)']<-'<0.0001'
  }
  MonocityTable[ ,'t value']<-round(MonocityTable[ ,'t value'],2)

  return(MonocityTable)
}

#' Calculate Linear Contrast for Treatment Levels
#'
#' This function calculates the contrast coefficients used to test for a linear relationship
#' across treatment levels.
#'
#' @param Data A data frame containing the treatment variable
#' @param Treatment The name of the treatment variable in the data frame
#'
#' @return A numeric vector of contrast coefficients for linear trend testing
#' @export
#'
#' @examples
#' test_data <- data.frame(Dose = factor(c(0, 1, 2, 3, 0, 1, 2, 3)))
#' getLineContrast(test_data, "Dose")
getLineContrast <-
  function(Data,Treatment){
    #' @export
    #This attains the contrast used to test a linear relationship
    K<-nlevels(as.factor(Data[ ,Treatment]))

    #Contrasts
    switch (K,
            return(0),
            return(c(-1,1)),
            return(c(-1,0,1)),
            return(c(-3,-1,1,3)),
            return(c(-2,-1,0,1, 2)),
            return(c(-5, -3, -1,1,3,5)),
            return(c(-3,-2, -1,0,1,2,3)),
            return(c(-7,-5,-3,-1,1,3,5,7)),
            return(c(-4,-3,-2,-1,0,1,2,3,4)),
            return(c(-9,-7,-5,-3,-1, 1,3, 5, 7,9)),
    )
    return()
  }

#' Calculate Quadratic Contrast for Treatment Levels
#'
#' This function calculates the contrast coefficients used to test for a quadratic relationship
#' across treatment levels.
#'
#' @param Data A data frame containing the treatment variable
#' @param Treatment The name of the treatment variable in the data frame
#'
#' @return A numeric vector of contrast coefficients for quadratic trend testing
#' @export
#'
#' @examples
#' test_data <- data.frame(Dose = factor(c(0, 1, 2, 3, 0, 1, 2, 3)))
#' getQuadContrast(test_data, "Dose")
getQuadContrast <-
  function(Data,Treatment){
    #' @export
    #This attains the contrast used to test a quadratic relationship

    K<-nlevels(as.factor(Data[ ,Treatment]))
    #Contrasts
    switch (K,
            return(0),
            return(c( 0,0)),
            return(c( 1,-2,1)),
            return(c(1,-1,-1,1)),
            return(c(2,-1,-2,-1,2)),
            return(c(5,-1,-4,-4,-1,5)),
            return(c(5,0,-3,-4,-3,0,5)),
            return(c(7,1,-3,-5,-5,-3,1,7)),
            return(c(28,7,-8,-17,-20,-17,-8,7,28)),
            return(c(6,2,-1,-3,-4,-4,-3,-1,2,6)),
    )
    return()
  }
#' Rank Transform Data Using Blom's Method
#'
#' This function performs a rank transformation on the data using Blom's method,
#' which is equivalent to the SAS PROC RANK with NORMAL=BLOM and TIES=MEAN options.
#' The transformation applies the formula (r-3/8)/(n+1/4)
#' where r is the rank and n is the sample size.
#'
#' @param Data A data frame containing the variable to be transformed
#' @param VecName The name of the variable in the data frame to be transformed
#'
#' @return A data frame with an additional column 'TransformedResponse' containing the rank-transformed values
#' @export
#'
#' @examples
#' test_data <- data.frame(Value = c(5, 2, 7, 2, 9, 3))
#' transformed_data <- rankTransform(test_data, "Value")
rankTransform <-
  function(Data,VecName){
    #' @export
    #This function will rank transform the data the same way the SAS code below does

    #proc rank data=sorted out=assumtst normal=blom
    #ties=mean;
    #var Counts;
    #by generation;
    #ranks rank_EGGS;
    #run;

    #Data- data set to be modified
    #VecName Name of Variable  to be modified
    #returns a data set with TransformedResponse as the rank transformed responce


    Vector<-Data[ ,VecName];

    #This handles the rank transform
    n<-length(Vector)
    r.i<-rank(Vector, na.last = TRUE, ties.method = c("random"))
    y.i<-(r.i-3/8)/(n+1/4)
    Data$y.i.norm<-qnorm(y.i)  #this will be for output
    Data<-Data[with(Data, order(Data$y.i.norm)), ]


    Repeats<-xtabs(~as.factor(Data[ ,VecName]))
    #This Block of code is for finding ties and will break if no ties exist
    if(max(Repeats)>1){ #Check for ties
      Repeats<-Repeats[-which(Repeats==1)] #Remove non-ties
      Names<-dimnames(Repeats)
      for (e in Names[[1]]){  #For every number that has a tie
        e<-as.numeric(e)
        Data[which(Data[ ,VecName]==e),'y.i.norm']<-mean(Data[which(Data[ ,VecName]==e),'y.i.norm']) #Average
      }
    }
    colnames(Data)[which(colnames(Data)=='y.i.norm')]<-'TransformedResponse' #rename column

    return(Data)
  }

