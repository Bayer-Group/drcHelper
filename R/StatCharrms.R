#' Williams Test from the StatCharrms Package
#'
#' @param df
#' @param resp
#' @param trt
#' @param direction
#' @param SeIn
#'
#' @return
#' @export
#'
#' @examples
williamsTest_JG<-function(df,resp,trt,direction='decreasing',SeIn=NULL){
  #' @export
  #df: The data frame
  #Each row is an observation
  #resp: The name of the response as a string
  #trt: The name of the response as a string
  #The lowest value must be the control
  #direction:  Is the direction of the test 'decreasing' or 'increasing'
  #SeIn: the standard error, default is program selected however
  #WilliamsTest can take in a different value in the case of repeated measures


  #Adjust for test direction synonyms
  direction<-toupper(direction)

  if (direction == 'DESCENDING'){
    direction<-'DECREASING'
  }

  if (direction == 'INCREASING'){
    direction<-'INCREASING'
  }

  if (is.element(direction,c('DECREASING','INCREASING'))==FALSE){
    message("Please enter either 'decreasing' or 'increasing' for the direction.\n Function ending.")
    return()
  }

  #Load look up table
  DTtable<-drcHelper::williamsTestLookUpTable
  #DTtable<-williamsTestLookUpTable #Debug code

  #Test for direction

  #get means
  df[[trt]] <-as.factor(df[[trt]])


  #remove missing data
  if(length(which(is.na(df[[trt]])==TRUE))>0){
    df<-df[-which(is.na(df[[trt]])==TRUE), ]
  }
  if(length(which(is.na(df[[resp]])==TRUE))>0){
    df<-df[-which(is.na(df[[resp]])==TRUE), ]
  }

  #
  #attain means
  Means<-by(df[[resp]],df[[trt]],mean,na.rm=TRUE)
  Means<-as.vector(Means)
  Means<-round(Means,4)
  #attain counts
  Count<-by(df[[resp]],df[[trt]],length)
  Count<-as.vector(Count)

  MeansTable<-data.frame(Means,Count)


  #get N
  N<-MeansTable$Count
  #Initial conditions
  Y.tilde<-MeansTable$Means

  N.tilde<-N
  DF=sum(N)-length(N)
  if (DF < 5){
    message('Error: Williams test is not appropriate when DF < 5. \n Function ending.')
    return()
  }

  #Loop of y.tilde
  Y0 = Y.tilde[1]
  Y.tilde = Y.tilde[-1]
  Amalg<-mat.or.vec(length(Y.tilde),1) #Keeps track of the amalgamated pieces
  End=FALSE
  while (End==FALSE){
    Y.tildeUp<-	Y.tilde[-1]
    Y.tildeDw<-	Y.tilde[-length(Y.tilde)]
    Diff=Y.tildeUp - Y.tildeDw
    if (direction=='INCREASING'){
      First=which(Diff<0) #Changes bases on
    }
    if (direction=='DECREASING'){
      First=which(Diff>0) #Changes bases on
    }
    if (length(First)==0){
      break()
      End=TRUE
    }
    First=First[1]+1
    if (Amalg[First-1]==0){ #Not amalgamated yet
      Amalg[{First-1}:First] = 1
      Combin=c({First-1}:First)

    }else{ #Amalgamates with every adjacent chain
      #Find the chains+
      Amalg[First] = 1
      Low=max(which(Amalg[1:{First-2}]== 0),0)+1
      Combin=c(Low:First)
    }
    Y.tilde[Combin] = sum(Y.tilde[Combin] * N.tilde[Combin])/sum( N.tilde[Combin])
  }
  Y.tilde= c(Y0,Y.tilde)
  #T statistic
  Vars<-by(df[[resp]],as.factor(df[[trt]]),var,na.rm=TRUE)
  #Allows user to specify diffrent se
  if (is.null(SeIn)==TRUE){
    S<-sqrt(sum(Vars*(N-1))/(sum(N)-length(N)))
    Se<-(S*sqrt(1/N+1/N[1]))
  }else{
    Se<-SeIn
  }
  T=(Y.tilde[1]-Y.tilde)/Se
  if (direction=='INCREASING'){
    T<-T*-1
  }
  #Table look up
  K=length(N)-1
  Row=which(DTtable$df==DF)
  if (length(Row)==0){  #sets the df to 10000 if the df >= 121
    Row<-119
  }
  Cols<-1+1:{2*(K-1)}
  B<-DTtable[Row,Cols[which(Cols %% 2 == 1)]]
  Q<-DTtable[Row,Cols[which(Cols %% 2 == 0)]]

  #Calculate t crit
  W=N[1]/N[-c(1,2)]
  Tcrit<-unlist(Q-10^(-2)*B*(1-1/W))
  QT1<-qt(0.95,DF)
  Tcrit<-c(QT1,Tcrit)



  #compile table
  Out<-data.frame(1:K,Y.tilde[-1],Y.tilde[1],Se[-1],DF,T[-1],Tcrit)
  #reverse the tables
  Out<-Out[rev(1:K),]
  #Lable the table with the levels of the treatment
  colnames(Out)<-c(trt,'Y.Tilde','Y0','Se Diff','DF','Will','TCrit')
  Sig<-Out$Will>Out$TCrit

  if (length(which(Sig==TRUE))>0){
    Sig[which(Sig==TRUE)]<-'*'}
  if (length(which(Sig==FALSE))>0){
    Sig[which(Sig==FALSE)]<-'.'}
  Out<-data.frame(Out,Sig)
  colnames(Out)[dim(Out)[2]]<-'Signif'

  #Clean by rounding
  Out$Y.Tilde<-signif(Out$Y.Tilde,4)
  Out$Se.Diff<-signif(Out$Se.Diff,4)
  Out$Will<-signif(Out$Will,4)
  Out$TCrit<-signif(Out$TCrit,4)
  Out[ ,1]<-rev(levels(df[[trt]])[-1]) #Exclude the control

  return(Out)
}
