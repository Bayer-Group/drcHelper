## PMCMRplus package helpers
## testthat/test_williamsSummary.R
## testthat/test_endpoints.R

#' Calculate Means Using the Pool Adjacent Violators Algorithm (PAVA)
#'
#' This function computes adjusted means and standard errors for groups defined by a factor variable
#' using the Pool Adjacent Violators Algorithm (PAVA).
#'
#' @param x A numeric vector of observations.
#' @param g A factor variable that defines the groups for which to calculate means.
#' @param alternative A character string specifying the alternative hypothesis.
#'                    Options are "greater" (default) or "less".
#'
#' @return A data frame containing the following columns:
#' \item{pavaMean}{The adjusted means for each group.}
#' \item{SE.diff}{The standard errors of the differences.}
#'
#' @export
#'
#' @examples
#' # Example usage:
#' x <- c(1, 2, 3, 4, 5, 6)
#' g <- factor(c("A", "A", "B", "B", "C", "C"))
#' result <- pavaMean(x, g)
#' print(result)
#' x <- c(106, 114, 116, 127, 145,
#'        110, 125, 143, 148, 151,
#'        136, 139, 149, 160, 174)
#' g <- gl(3,5)
#' levels(g) <- c("0", "I", "II")
#' pavaMean(x,g)
pavaMean <- function(x,g,alternative = "greater"){
  requireNamespace("PMCMRplus", quietly = TRUE)
  alternative <- match.arg(alternative, choices = c("greater", "less"))
  xold <- x
  if (alternative == "less") {
    x <- -x
  }
  xi <- tapply(x, g, mean, na.rm = T)
  ni <- tapply(x, g, length)
  k <- nlevels(g)
  kk <- k - 1
  if (kk > 10)
    stop("Critical t-values are only available for up to 10 dose levels.")
  N <- sum(ni)
  df <- N - k
  s2i <- tapply(x, g, var)
  s2in <- 1/df * sum(s2i * (ni - 1))
  # browser()
  # xiiso <- .Fortran("pava", y = as.double(xi), w = as.double(ni),
  #                   kt = integer(k), n = as.integer(k))$y
  xiiso <- Iso::pava(xi,ni)
  mui <- rep(NA, k)
  for (i in 1:k) {
    v <- k
    tmp <- rep(NA, length(1:i))
    for (u in 1:i) {
      j <- u
      tmp01 <- sapply(i:k, function(v) sum(ni[j:v] * xiiso[j:v])/sum(ni[j:v]))
      tmp[u] <- min(tmp01)
    }
    mui[i] <- max(tmp, na.rm = TRUE)
  }
  sei <- sapply(1:k, function(i) {
    sqrt((s2in/ni[i] + s2in/ni[1]))
  })
  res <- data.frame(`pavaMean` = mui, SE.diff = sei)
  # res$Will <- res$pavaMean-res$pavaMean[1]
  # res$Will[1] <- NA
  return(res)
}
#' Summary Williams Test results.
#'
#' @param object William test result object
#' @param verbose whether to print intermediate results.
#' @param ... additional parameters to be passed into the function, placeholder
#'
#' @return William test results
#' @export
#'
#' @examples ## Example from Sachs (1997, p. 402)
#' x <- c(106, 114, 116, 127, 145,
#'        110, 125, 143, 148, 151,
#'        136, 139, 149, 160, 174)
#' g <- gl(3,5)
#' levels(g) <- c("0", "I", "II")
#'
#' ## Williams Test
#' res <- PMCMRplus::williamsTest(x ~ g)
#' summaryZG(res) ## return a data frame instead of a list.
summaryZG <- function (object, verbose=F,...)
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
  if(verbose){
    cat("\n\t", object$method, "\n\n")
    cat("data: ", object$data.name, "\n")
    if (!is.null(object$alternative)) {
      cat("alternative hypothesis: ", object$alternative, "\n")
    }
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
  ##invisible(object)
}




#' get from william res accept/reject
#'
#' @param william william test results
#' @param n number of hypotheses to be tested. description
#'
#' @return a vector of accept and rejects.
#' @export
#'
#' @examples ## Example from Sachs (1997, p. 402)
#' x <- c(106, 114, 116, 127, 145,
#'        110, 125, 143, 148, 151,
#'        136, 139, 149, 160, 174)
#' g <- gl(3,5)
#' levels(g) <- c("0", "I", "II")
#'
#' ## Williams Test
#' res <- PMCMRplus::williamsTest(x ~ g)
#' getwilliamRes(res)
getwilliamRes <- function(william,n=NULL){
  if(inherits(william,"try-error")){
    if(is.null(n)) stop("when the test does not return a valid results, you need to specify
                        the number of hypotheses. ")
    return(rep(NA,n=n))
  }else(return(as.character(summaryZG(william)$decision)))
}



#' Obtain Endpoint (NOEC) according to a series of p-values
#'
#' @param pvals pvals from a tests
#' @param doses corresponding doses
#' @param procedure procedure to obtain NOEC
#'
#' @return NOEC
#' @export
#'
#' @details
#' If all p-values are NA, it initializes them to zeros. If there are fewer p-values than doses,
#' it pads the p-values with zeros.It checks which doses are significant by comparing the p-values
#' to a threshold (0.05).Depending on the specified procedure, it either steps down from the highest
#' dose to find the NOEC or steps up from the lowest dose..
#'
#' @examples
#' pvals <- c(0.01, 0.03, 0.07)
#' doses <- c("Control", "B", "C", "D")
#' getEndpoint(pvals, doses)
getEndpoint <- function(pvals,doses=c("Control","B","C","D"),procedure="stepDown"){
  nd <-length(doses)
  if(all(is.na(pvals))) {
    pvals <- rep(0,nd-1) ## length of pvalues
  }
  np <- length(pvals)
  if(np < (nd-1)){
    pvals <- c(pvals,rep(0,nd-1-np)) ## Assuming missing the high doses
  }
  sig <- pvals<0.05


  NOEC <- doses[1]
  if(procedure=="stepDown")
  {
    for(i in (nd-1):1){
      if(!sig[i]){
        NOEC <- doses[i+1]
        return(NOEC)
      }
    }
    if(sig[1]) return(NOEC)



  }else{
    for(i in 1:(nd-1)){
      if(sig[i]){
        NOEC <- doses[i] ## note this dose is actually the dose level where the next one is significant.
        return(NOEC)
      }

    }
    if(!sig[nd-1]) return(doses[nd])
  }
}




#' get Endpoint for continuous data according to results of a series of tests
#'
#' @param paov Anova p-value
#' @param pks Kolmogorov-Smirnov test p-value
#' @param pnormal Normal test p-value
#' @param phomogeneity Homogeneity variance test p-value
#' @param monotonicity Monotonicity test p-value
#' @param william Williams' test p-value
#' @param dunnett Dunnett's test p-value
#' @param dunn Dunn's test p-value
#' @param jonckheere Jonckheere-Tepstra test p-value
#' @param procedure step down of up
#' @param doses tested doses
#'
#' @return NOEC along with an attribute indicating which test was used.
#' @export
#' @details
#' Purpose: The contEndpoint function computes the No Observed Effect Concentration (NOEC) based on the results of various statistical tests. It evaluates these tests'
#' p-values to determine which statistical analysis is most appropriate.The function first checks the
#' significance of the monotonicity test. Depending on the results, it selects the appropriate
#' p-value from the relevant test (Dunnett, Dunn, Williams, or Jonckheere) to compute the NOEC.
#' The function returns the NOEC along with an attribute indicating which test was used. Original function
#' used directly the williams' test or dunnett's test objects as input, which can cause conflicts when
#' using different functions to get the pvals.
#'
#'
#' @examples contEndpoint(paov = 0.01, pks = 0.03, pnormal = 0.06,
#' phomogeneity = 0.07, monotonicity = c(0.1,0.2),
#' william = c(0.07, 0.06, 0.03, 0.01), dunnett = c(0.07, 0.06, 0.03, 0.01),
#' dunn = c(0.07, 0.06, 0.03, 0.01),
#' jonckheere = c(0.07, 0.06, 0.03, 0.01),
#' procedure = "stepDown",
#' doses = c("Control","A", "B", "C", "D"))
contEndpoint <- function(paov,pks,pnormal,phomogeneity,monotonicity,william,dunnett,dunn,jonckheere,procedure="stepDown",doses=c("A","B","C","D")){
  ## if monotone not rejected. linear contrast not significant and quadratic sig! Strong mono assumption!
  test <- "NK"
  if(is.character(monotonicity)){
    monotonicity <- as.numeric(gsub("<","",monotonicity))-0.001
  }
  if(monotonicity[1] >0.05 & monotonicity[2] < 0.05){ ## not monotonic
    if(pnormal>0.05 & phomogeneity>0.05){
      ## check if PAVA Problem!
      ## pvals <- dunnett$p.value
      pvals <- dunnett
      test <- "Dunnett"
    }else{
      ## pvals <- dunn$p.value
      pvals <- dunn
      test <- "Dunn"
    }
  }else{ ## monotonic
    if(pnormal>0.05 & phomogeneity>0.05){
      ## check if PAVA Problem!
      if(!inherits(william,"try-error")){
       # pvals <- as.character(summaryZG(william)$decision)
        #pvals <- as.numeric(plyr::mapvalues(pvals,from=c("accept","reject"),to=c(0.2,0.01)))
        pvals <- william
        test <- "Williams"
      }else{
        ## pvals <- jonckheere$p.value
        pvals <- jonckheere
        test <- "Jonckheere"
      }
    }else{
      ## pvals <- jonckheere$p.value
      pvals <- jonckheere
      test <- "Jonckheere"
    }
  }
  ############
  NOEC <- getEndpoint (pvals,doses=doses,procedure=procedure)
  attributes(NOEC) <- list(test=test)
  return(NOEC)
}


