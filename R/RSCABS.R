## Note this script is adapted from the archived version of RSCABS developed by Joe Swintek [aut, cre],
## Kevin Flynn[ctb].	RSCABS_0.9.5.tar.gz	2020-05-01 08:50	(https://cran.r-project.org/src/contrib/Archive/RSCABS/)
## Original package licence is License: CC0, which means no copyrights reserved. However,
## the author and the archive link are kept in this file so the source can be traced back.



#' Run RSCABS test
#'
#' Runs the Rao-Scott adjusted Cochran-Armitage trend test by slices (RSCABS)
#' analysis.The function is adapted from the archived version of RSCABS developed by
#' Joe Swintek et al with CC0 license. It is not updated anymore and included
#' for validation purpose. It is provided in this package a different function
#' to perform the same task.
#'
#' @param Data A standard data set in the tall format.  Every row indicates an organism.
#' The data set must contain columns for the treatment level and every tested histological endpoint.
#' @param Treatment The name of the column that contains the information about the treatment level.
#' Increasing values indicate higher treatments.
#' @param Replicate The name of the column that contains the information about the replicate structure.
#' If the  replicate is not specified this will default to running "CA" as the test type.
#' @param Effects The endpoint to be tested.  Defaults to all columns that have integers less then 20.
#' The analysis assumes that higher scores indicate a worse outcome.
#' @param test.type Indicate the type of analysis to be performed.  Use "RS" to select the Rao-Scott
#' adjustment to the Cochran-Armitage test and "CA" to ignore the adjustment.
#'
#' @return a table of the test results for each treatment and injury score.
#' @export
#' @references Green, John W. and Springer, Timothy A. and Saulnier, Amy N. and Swintek, Joe,
#' (2014) Statistical analysis of histopathological endpoints. Environmental Toxicology and Chemistry, 33(5), 1108-1116
#' @author Joe Swintek
#' @examples \dontrun{
#'## Not run:
#'#Take the subset corresponding to F0-females of 16 weeks of age
#'data(exampleHistData)
#'exampleHistData.sub<-exampleHistData[which(exampleHistData$Generation=='F2' &
#'              exampleHistData$Genotypic_Sex=='Female' & exampleHistData$Age=='16_wk' ),  ]
#'  #Run RSCABS
#'  eampleResults<-runRSCABS(exampleHistData.sub,'Treatment',
#'                           'Replicate',test.type='RS')
#' }
runRSCABS <- function(Data,Treatment,Replicate='',Effects='',test.type='RS'){
  #This function will produce a table of step-down Cochran-Armitage trend tests with possible Rao-Scott adjustment by slices
  #It will Run the test on every effect in the Effect list
  #' @export
  #Turn Replicate and Treatment into factors

  Data[ ,Treatment]<-as.factor(Data[ ,Treatment])
  if ( identical(Replicate,'') == FALSE && identical(test.type,'CA')==FALSE){
    Data[ ,Replicate]<-as.factor(Data[ ,Replicate])
  }else{
    test.type='CA'
    Replicate<-'FalseReplicateFill'
    Data$FalseReplicateFill<-as.factor(1:length(Data[ ,Treatment])) #Needed so R data structure do not change type.
  }


  if (test.type != 'RS' & test.type != 'CA'){
    message('Error: Invalid test type')
    return()
  }
  #Remove all non-whole numbers


  #Default for effect every column name that is not a Treatment, Replicate, and has a 0 < K.max < 20
  #turn off warnings
  options(warn = -1)

  if (Effects ==''){
    Effects<-colnames(Data)
    cind <- unlist(lapply(Data, is.numeric))
    Effects<-colnames(Data)[cind]
    Maxes<-apply(Data[,cind],2,max,na.rm=TRUE)
    keepInd <- which(Maxes>0 & Maxes < 20)
    Effects<- Effects[keepInd]
  }

  #Changed to account for just 1 effect
  if (length(Effects)>1){
    Data[ ,Effects]<-apply(Data[ ,Effects],2,convert2Score)
  }else{
    Data[ ,Effects]<-convert2Score(Data[ ,Effects])
  }

  options(warn=0) #turn on warnings


  #Need to remove factors,done 01.2025,ZG
  #Prep Data

  Data.Prep<-sapply(c('&Fill#',Effects),prepDataRSCABS,Data=Data,Treatment=Treatment,Replicate=Replicate)

  Results.Raw<-sapply(c('&Fill#',Effects),stepKRSCABS,Data=Data.Prep,Treatment=Treatment,Replicate=Replicate,test.type=test.type)
  Results<-do.call("rbind", lapply(Results.Raw, data.frame, stringsAsFactors = FALSE))


  if (length(which(is.finite(Results[  ,'Statistic'])==FALSE))>0){
    Results<-Results[-which(is.finite(Results[  ,'Statistic'])==FALSE), ]
  }
  rownames(Results)<-{}

  return(Results)
}




#' Runs the kth slice of RSCABS
#'
#' @param x.i.j matrix containing the number of observed "successes" for replicate i on treatment j.
#' @param n.i.j matrix containing the number of observations for replicate i on treatment j.
#' @param m.i matrix of number units in each treatment/replicate combination.
#' @param TestK The kth severity score being tested.
#' @param test.type Indicate the type  of analysis to be performed.
#' Use "RS" to select the Rao-Scott adjustment to the Cochran-Armitage test
#' and "CA" to ignore the adjustment.
#'
#' @return Returns a list with the following values:
#' \item{Response}{
#'   The endpoint that is being tested.
#' }
#' \item{Treatment}{
#'   The treatment level.
#' }
#' \item{R-Score}{
#'   The severity score from the histology.
#' }
#' \item{Statistic}{
#'   The test statistic corresponding to that row's endpoint treatment level and R-Score.
#' 	}
#' 	\item{P-Value}{
#' 		The corresponding p-value
#' 	}
#' 	\item{Signif}{
#' 		The significance flag where the cutoffs for stars and dot are 0, 1e-04, 0.001, 0.01,0.05,1.
#' 	}
#' @export
#'
#' @author Joe Swintek
RSCABK <-
  function(x.i.j,n.i.j,m.i,TestK,test.type){
    #This function is called for the detailed results functions
    #This is just for 1 slice

    K=1
    #Variable names are the same as in Rao and Scott 1992

    x.i<-colSums(x.i.j)  #Number of successful observations
    n.i<-colSums(n.i.j)  #Total Number in a treatment

    p.i.hat=x.i/n.i


    #Calculate r.ij.sum.sq
    #this uses the expanded form of sigma(r.ij) squared
    r.ij.sum.sq<-mat.or.vec(length(m.i)[1],K)

    p.i.hat.k<-apply(t(p.i.hat),2,rep,dim(n.i.j)[1])  #p.hat

    if(test.type =='CA'){
      d.i<-1  #No adjustment
    }
    if(test.type =='RS'){
      #Calculate the Rao-Scott adjustment
      x.ij.sum.sq<-colSums(x.i.j^2)
      cross<- -2*colSums(x.i.j*n.i.j*p.i.hat.k)
      last.sum.sq<-colSums((n.i.j*p.i.hat.k)^2)
      r.ij.sum.sq<-x.ij.sum.sq+cross+last.sum.sq;

      v.i <- m.i/(m.i - 1)/n.i^2 *r.ij.sum.sq
      d.i <- n.i * v.i/(p.i.hat * (1 - p.i.hat))



      d.i[which(is.na(d.i))]<-1    #Ignores the correction when incidence is 0% or 100%
      d.i[which(is.nan(d.i))]<-1   #Ignores the correction when incidence is 0% or 100%
      d.i[which(d.i<1)]<-1         #Cf Rao and Scott Clustered Poisson paper;
    }

    #apply adjustment
    x.i.new <- x.i/d.i
    n.i.new <- n.i/d.i

    scores<-matrix(1:length(x.i.new)-1)
    p.hat <- sum(x.i.new)/sum(n.i.new)
    mean.score <- sum(scores * n.i.new)/sum(n.i.new)
    var.scores <- sum(n.i.new * (scores - mean.score)^2)

    RS <- (sum(x.i.new * scores) - p.hat * sum(n.i.new * scores))/sqrt(p.hat * (1 - p.hat) * var.scores) #the statistic value
    #P-Value
    p.val <- pnorm(abs(RS), lower.tail = TRUE)
    p.val<-1-p.val
    if (is.finite(p.val)==FALSE){
      p.val<-1
    }
    #Adds marks for significance
    Sig<-rep('.',length(p.val))
    Sig[which(p.val<=.05)]<-'*'
    Sig[which(p.val<=.01)]<-'**'
    Sig[which(p.val<=.001)]<-'***'

    return (list('Treatment'=max(scores)+1,'R-Score'=TestK,'Statistic' = RS, 'P-Value' = p.val,'Signif'=Sig))
  }


#' Convert Values to Scores
#'
#' Converts any non-positive numbers to NA.
#'
#' @param Dvec A numeric vector to be converted.
#'
#' @return A numeric vector with non-positive values set to NA.
convert2Score <-
  function(Dvec){
    #This Function will convert any object that is not zero or a positive number to NA
    #' @export
    oldw <- getOption("warn")
    options(warn=-1)  #Turn off warnings
    Dvec<-as.numeric(as.character(Dvec))  #Will induce a a warning
    Dvec[which(Dvec<0)]<-NA
    options(warn=oldw) #Turn on warnings
    return(Dvec)
  }



#' Prepares data for an RSCABS analysis
#'
#' @param Effect The endpoint being converted.
#' @param Data The tall formatted data set.
#' @param Treatment The name of the treatment variable.
#' @param Replicate The name of the replicate variable.
#'
#' @return Returns a list containing:
#' \item{x.i.j}{
#'   matrix containing the number of observed "successes" for replicate i on treatment j.
#' }
#' \item{n.i.j}{
#'   matrix containing the number of observations for replicate i on treatment j.
#' }
#' \item{m.i}{
#'   matrix of number replicates in each treatment-replicate combination.
#' }
#' \item{K.max}{
#'   The maximum severity score for the endpoint.
#' }
#' @export
#'
#' @author Joe Swintek
prepDataRSCABS <-
  function(Effect='',Data={},Treatment='',Replicate=''){
    #Data Transform from a list of individuals to matrix format
    #This will take Clustered Data and convert it to by
    #' @export

    if(Effect=='&Fill#'){ #ensures the correct data structure output
      return()
    }
    if(length(which(colnames(Data)==Effect))==0){
      print(paste(Effect,' is not in data set. Ending function.',sep=''))
      return()
    }
    K.max<-max(Data[ ,Effect],na.rm=TRUE) #max K score


    #Remove NA and negative numbers
    if (length(which(is.na(Data[ ,Effect])))>0){
      Data<-Data[-which(is.na(Data[ ,Effect])), ]
    }
    if (length(which(Data[ ,Effect]<0))>0){
      Data<-Data[-which(Data[ ,Effect]<0), ]
    }
    #Convert Factors to Numerics
    Data[ ,Effect]<-as.numeric(Data[ ,Effect])

    if (K.max==0){
      print(paste('There is no variation in ',Effect,'. Ending function.',sep=''))
      return()
    }
    #Replicates are rows, Treatment are columns  [Replicate,Treatment]
    if(!is.factor(Data[,Replicate])) Data[,Replicate] <- factor(Data[,Replicate])
    n.i.j<-xtabs( ~Data[[Replicate]]+Data[[Treatment]])
    m.i<-apply(n.i.j,2,function(Vec){
      if (length(which(Vec==0))>0){
        Vec<-Vec[-which(Vec==0)]
      }
      return(length(Vec))
    })




    x.i.j<-array(dim=c(dim(n.i.j)[1],dim(n.i.j)[2],K.max)) #Declare x.i.j , frequency array of scores k or larger
    #Each k level is on the 3rd dimension
    ## browser()
    for (K in 1:K.max){

      x.i.j[ , ,K]<-xtabs( ~Data[[Replicate]]+Data[[Treatment]],subset=Data[ ,Effect]>=K)
    }
    RSCABS.Prep.Data<-list(x.i.j=x.i.j,n.i.j=n.i.j,m.i=m.i,K.max=K.max)
    return(RSCABS.Prep.Data)
  }


#' Performs the step down aspect of RSCABS
#'
#' @param TestK The severity score being tested
#' @param x.i.j Matrix containing the number of observed "successes" for replicate i on treatment j.
#' @param n.i.j Matrix containing the number of observations for replicate i on treatment j.
#' @param m.i Matrix of number units in each treatment/replicate combination.
#' @param Effect The the end point to be tested.
#' @param test.type Indicate the type  of analysis to be performed.
#' Use "RS" to select the Rao-Scott adjustment to the Cochran-Armitage test
#' and "CA" to ignore the adjustment
#'
#' @return \item{Result.K}{An intermediary result.}
#' @export
#'
#' @author Joe Swintek
stepDownRSCABS <-
  function(TestK,x.i.j,n.i.j,m.i,Effect,test.type){
    Next<-0; #Step Down test
    #' @export
    #Replicates are rows, Treatment are columns  [Replicate,Treatment]
    Result.K<-as.data.frame({})
    x.i.j.K<-x.i.j[ , ,TestK]
    while (Next==0){
      Result<-RSCABK(x.i.j.K,n.i.j,m.i,TestK,test.type=test.type)

      #The step down
      Next<- Result['P-Value']>0.05 | dim(x.i.j.K)[2]<=2 #Stop conditions of
      ## too high of p.val or 2 treatments
      x.i.j.K<-x.i.j.K[ ,-dim(x.i.j.K)[2]]
      n.i.j<-n.i.j[ ,-dim(n.i.j)[2]]
      m.i<-m.i[-length(m.i)]
      Result.K<-rbind(Result.K, as.data.frame(Result))
    }


    ## Result.K<-rbind(Result.K, as.data.frame(Result))

    Effect<-paste(Effect,TestK,sep='')
    Result.K<-cbind(Effect,Result.K)
    return (Result.K)
  }



#' plotRSCABS
#'


#' Steps through the severity score for a given effect
#'
#' An internal function for stepping through each severity score of an endpoint.
#'
#' @param Effect Endpoint being tested.
#' @param Data.Prep Data prepared by prepDataRSCABS.
#' @param Treatment Name of the treatment variable.
#' @param Replicate Name of the replicate variable.
#' @param test.type Indicate the type of analysis to be performed.  Use "RS" to select
#' the Rao-Scott adjustment to the Cochran-Armitage test and "CA" to ignore the adjustment.
#'
#' @return Results.Effect
#' \item{Results.Effect}{An intermediary step for results.}
#' @export
#'
#' @author Joe Swintek
stepKRSCABS <-
  function(Effect,Data.Prep,Treatment,Replicate,test.type){
    #This iterates though each k-level
    #' @export
    if (is.null(Data.Prep[[Effect]])==TRUE){ #Skip effects not needed
      return()
    }

    x.i.j<-Data.Prep[[Effect]]$x.i.j
    n.i.j<-Data.Prep[[Effect]]$n.i.j
    m.i<-Data.Prep[[Effect]]$m.i
    K.max<-Data.Prep[[Effect]]$K.max

    Results.Effect<-lapply(1:K.max,stepDownRSCABS,x.i.j=x.i.j,n.i.j=n.i.j,m.i=m.i,Effect=Effect,test.type=test.type) #each k-level
    Results.Effect<-do.call("rbind", lapply(Results.Effect, data.frame, stringsAsFactors = FALSE))  #'fix' output table
    return(Results.Effect)
  }

