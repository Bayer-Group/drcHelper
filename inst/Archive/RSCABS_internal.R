


.invertOrder<-function(Data,ColNames){
  #' @export
  #For each listed vector of data, finds the an inverts the range of numbers
  #Will


  R<-Data[ ,ColNames]
  R<-cbind(R) #ensures X has at least 1 column vector

  #This inverts the rank of the number while preserving the differences between the numbers
  R1<-apply(R,2,function(X){
    X<-convert2Score(X)
    if (sum(is.na(X))==length(X)){
      return(X)
    }
    return(-X+max(X,na.rm=TRUE)+min(X,na.rm=TRUE))
  })


  Data[ ,ColNames]<-R1
  return(Data)
}



.orderOutput<-function(Results){
  #' @export
  #This function take the Results from RSCABSMain and reorders them to be easer to read


  Response<-strsplit(as.character(Results$Response),'')
  #Remove the last character
  Response<-lapply(Response,function(x){
    return(paste0(x[-length(x)],collapse=''))
  })
  Response<-unlist(Response)

  Results$Response<-Response

  #Order based on R-Score
  Results<-Results[order(Results$Response,Results[ ,'R-Score'],decreasing = TRUE), ]

  #swap R-Score and Treatment
  Results[ ,c(2,3)] <-Results[ ,c(3,2)]
  colnames(Results)[c(2,3)]<-colnames(Results)[c(3,2)]

  #Re-paste the responses together
  Results$Response<-paste0(Results$Response,Results[ ,'R-Score'])

  return(Results)
}



.tagInvert<-function(Results,Data,ColNames){
  #' @export
  #This function inverts the the R-Score listed for results


  Response<-strsplit(as.character(Results$Response),'')
  #Remove the last character
  Response<-lapply(Response,function(x){
    return(paste0(x[-length(x)],collapse=''))
  })
  Response<-unlist(Response)

  #Apply use tested endpoints
  ColNamesResponse<-ColNames[which(is.element(ColNames,Response)==TRUE)]

  #Add IsInverted tag
  Inverted<-rep('',length(Response))

  #Apply the inversion algorithm to the R-Score of every inverted Endpoint
  for (e in ColNamesResponse){
    R<-Data[ ,e]
    R<-convert2Score(R)
    iIndex<-which(Response==e)
    RScores<-Results[iIndex,'R-Score']
    Results[iIndex,'R-Score']<- {-RScores+max(R,na.rm=TRUE)+min(R,na.rm=TRUE)}
    Inverted[iIndex]<-'Yes'
  }

  #Add the last character back in
  Results$Response<-paste0(Response,Results[ ,'R-Score'])
  Results<-cbind(Results,Inverted)


  return(Results)
}




















