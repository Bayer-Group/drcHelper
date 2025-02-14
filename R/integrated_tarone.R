## YEAR: 2021
## COPYRIGHT HOLDER: ClinStats authors
## MIT licence
#------------------------------------------------------------------------------#
## Unit Testing in # FILE: tests/testthat/test_quantal_categorical.R

#' Tarones test for Stratified OR (integrated from https://github.com/the8thday/ClinStats/blob/master/R/Tarones.R)
#'
#' \code{calcTaronesTest} is used to test whether the OR in different strata are different
#'
#' @param mylist A list of matrix Stratified by a Variable
#' @param referencerow Unexposed row
#'
#' @return string with a list of p-values
#' @export
#' @details modified the output to return p-values, need metafor package to run
#'
#' @aliases Tarones
#' @examples
#' mymatrix1 <- matrix(c(4,5,5,103),nrow=2,byrow=TRUE)
#' colnames(mymatrix1) <- c("Disease","Control")
#' rownames(mymatrix1) <- c("Exposure","Unexposed")
#' mymatrix2 <- matrix(c(10,3,5,43),nrow=2,byrow=TRUE)
#' colnames(mymatrix2) <- c("Disease","Control")
#' rownames(mymatrix2) <- c("Exposure","Unexposed")
#' mylist <- list(mymatrix1,mymatrix2)
#' calcTaronesTest(mylist)
#' \dontrun{
#' calcTaronesTest(mymatrix1)
#' }
calcTaronesTest <- function(mylist, referencerow = 2) {
  # requireNamespace("metafor")
  numstrata <- length(mylist)
  # make an array "ntrt" of the number of people in the exposed group, in each stratum
  # make an array "nctrl" of the number of people in the unexposed group, in each stratum
  # make an array "ptrt" of the number of people in the exposed group that have the disease,
  # in each stratum
  # make an array "pctrl" of the number of people in the unexposed group that have the disease,
  # in each stratum
  # make an array "htrt" of the number of people in the exposed group that don't have the
  # disease, in each stratum
  # make an array "hctrl" of the number of people in the unexposed group that don't have the
  # disease, in each stratum
  ntrt <- vector()
  nctrl <- vector()
  ptrt <- vector()
  pctrl <- vector()
  htrt <- vector()
  hctrl <- vector()
  if (referencerow == 1) {
    nonreferencerow <- 2
  } else {
    nonreferencerow <- 1
  }
  for (i in 1:numstrata)
  {
    mymatrix <- mylist[[i]]
    DiseaseUnexposed <- mymatrix[referencerow, 1]
    ControlUnexposed <- mymatrix[referencerow, 2]
    totUnexposed <- DiseaseUnexposed + ControlUnexposed
    nctrl[i] <- totUnexposed
    pctrl[i] <- DiseaseUnexposed
    hctrl[i] <- ControlUnexposed
    DiseaseExposed <- mymatrix[nonreferencerow, 1]
    ControlExposed <- mymatrix[nonreferencerow, 2]
    totExposed <- DiseaseExposed + ControlExposed
    ntrt[i] <- totExposed
    ptrt[i] <- DiseaseExposed
    htrt[i] <- ControlExposed
  }
  # calculate Tarone's test of homogeneity, using the rma.mh function from the
  # "metafor" package
  # Meta-Analysis via the Mantel-Haenszel Method
  tarone <- metafor::rma.mh(ptrt, htrt, pctrl, hctrl, ntrt, nctrl)
  pvalue <- tarone$TAp
  print(paste("Pvalue for Tarone's test =", pvalue))
  return(list(pval=pvalue,tarone=tarone))

}
