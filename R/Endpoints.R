#' Summary Williams Test results.
#'
#' @param object William test result object
#' @param ... additional parameters to be passed into the function, placeholder
#'
#' @return William test results
#' @export
#'
#' @examples
summaryZG <-function (object, ...)
{
  critVal <- as.numeric(object$crit.value)
  stat <- as.numeric(object$statistic)
  dist <- object$dist
  dec <- ifelse(stat > critVal, "reject", "accept")
  critDist <- paste0(dist, "-crit")
  if (!is.matrix(object$statistic)) {
    if (grepl(pattern = "Hayter's", x = object$method)) {
      H0 <- switch(object$alternative, greater = paste("Mean(xi) - Mean(xj) <= 0"),
                   less = paste("Mean(xi) - Mean(xj) >= 0"))
    }
    else {
      H0 <- switch(object$alternative, greater = paste("Med(xi) - Med(xj) <= 0"),
                   less = paste("Med(xi) - Med(xj) >= 0"))
    }
  }
  else {
    grp1 <- as.numeric(c(col(object$statistic)))
    grp2 <- as.numeric(c(row(object$statistic)))
    cnam <- colnames(object$statistic)
    rnam <- rownames(object$statistic)
    H0 <- switch(object$alternative, greater = paste(rnam[grp2],
                                                     "-", cnam[grp1], "<=", "0"), less = paste(rnam[grp2],
                                                                                               "-", cnam[grp1], ">=", "0"))
    ok <- !is.na(stat)
    stat <- stat[ok]
    H0 <- H0[ok]
    dec <- dec[ok]
  }
  dist <- paste0(dist, "-value")
  cat("\n\t", object$method, "\n\n")
  cat("data: ", object$data.name, "\n")
  if (!is.null(object$alternative)) {
    cat("alternative hypothesis: ", object$alternative, "\n")
  }
  paramName <- names(object$parameter)
  if (length(paramName) == 2) {
    suppressWarnings(expr = xdf <- data.frame(STATISTIC = round(stat,
                                                                3), PARAM1 = object$parameter[1], PARAM2 = object$parameter[2],
                                              CRITDIST = round(critVal, 3), DECISION = dec, ALPHA = 0.05))
    names(xdf) <- c(dist, paramName[1], paramName[2], critDist,
                    "decision", "alpha")
  }
  else {
    suppressWarnings(expr = xdf <- data.frame(STATISTIC = round(stat,
                                                                3), PARAM1 = object$parameter[1], CRITDIST = round(critVal,
                                                                                                                   3), DECISION = dec, ALPHA = 0.05))
    names(xdf) <- c(dist, paramName[1], critDist, "decision",
                    "alpha")
  }
  rownames(xdf) <- H0
  return(xdf)
  invisible(object)
}




#' get from william res accept/reject
#'
#' @param william william test results
#'
#' @return
#' @export
#'
#' @examples
getwilliamRes <- function(william){
  if(class(william)=="try-error"){
    return(rep(NA,3))
  }else(return(as.character(summaryZG(william)$decision)))
}



#' Obtain Endpoint (NOEC) according to a series of p-values
#'
#' @param pvals pvals from a tests
#' @param doses corresponding doses
#' @param procedure procedure to obtain NOEC
#'
#' @return
#' @export
#'
#' @examples
getEndpoint <- function(pvals,doses=c("Control","B","C","D"),procedure="stepDown"){
  nd <-length(doses)
  if(all(is.na(pvals))) {
    pvals <- rep(0,nd-1) ## length of pvalues
  }
  np <- length(pvals)
  if(np < (nd-1)){
    pvals <- c(pvals,rep(0,nd-1-np))
  }
  sig <- pvals<0.05


  NOEC <- doses[1]
  if(procedure=="stepDown")
  {
    for(i in (nd-1):1){
      if(!sig[i]){ NOEC <- doses[i+1]

      return(NOEC)}
    }
    if(sig[1]) return(NOEC)

  }else{
    for(i in 1:(nd-1)){
      if(sig[i]){
        NOEC <- doses[i]
        return(NOEC)
      }

    }
    if(!sig[nd-1]) return(doses[nd])
  }
}




#' get Endpoint for continuous data according to results of a series of tests
#'
#' @param paov
#' @param pks
#' @param pnormal
#' @param phomogeneity
#' @param monotonicity
#' @param william
#' @param dunnett
#' @param dunn
#' @param jonckheere
#' @param procedure
#' @param doses
#'
#' @return
#' @export
#'
#' @examples
contEndpoint <- function(paov,pks,pnormal,phomogeneity,monotonicity,william,dunnett,dunn,jonckheere,procedure="stepDown",doses=c("A","B","C","D")){
  ## if monotone not rejected. linear contrast not significant quadratic sig!
  test <- "NK"
  if(is.character(monotonicity)){
    monotonicity <- as.numeric(gsub("<","",monotonicity))
  }
  if(monotonicity[1] >0.05 & monotonicity[2] < 0.05){
    if(pnormal>0.05 & phomogeneity>0.05){
      ## check if PAVA Problem!
      pvals <- dunnett$p.value
      test <- "Dunnett"
    }else{
      pvals <- dunn$p.value
      test <- "Dunn"
    }
  }else{
    if(pnormal>0.05 & phomogeneity>0.05){
      ## check if PAVA Problem!
      if(class(william)!="try-error"){
        pvals <- as.character(summaryZG(william)$decision)
        pvals <- as.numeric(plyr::mapvalues(pvals,from=c("accept","reject"),to=c(0.2,0.01)))
        test <- "Williams"
      }else{
        pvals <- jonckheere$p.value
        test <- "Jonckheere"
      }
    }else{
      pvals <- jonckheere$p.value
      test <- "Jonckheere"
    }
  }
  ############
  NOEC <- getEndpoint (pvals,doses=doses,procedure=procedure)
  attributes(NOEC) <- list(test=test)
  return(NOEC)
}


