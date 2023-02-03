#' Testing Monotonicity
#'
#' @param Data Data frame
#' @param Treatment name of the treatment variable
#' @param Response name of the response variable
#'
#' @return monotonicity table
#' @export
#'
#' @examples
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

