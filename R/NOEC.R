## Function to calculate NOECs using multiple methods and broomed together


#' Tarone's Z Test
#'
#' Tests the goodness of fit of the binomial distribution.
#'
#' @param M Counts
#' @param N Trials
#'
#' @return a \code{htest} object
#'
#' @author \href{https://stats.stackexchange.com/users/173082/reinstate-monica}{Ben O'Neill}
#' @references \url{https://stats.stackexchange.com/a/410376/6378} and
#' R. E. TARONE, Testing the goodness of fit of the binomial distribution, Biometrika, Volume 66, Issue 3, December 1979, Pages 585–590, \url{https://doi.org/10.1093/biomet/66.3.585}
#' @importFrom stats pnorm
#' @export
#' @examples
#'  #Generate example data
#' N <- c(30, 32, 40, 28, 29, 35, 30, 34, 31, 39)
#' M <- c( 9, 10, 22, 15,  8, 19, 16, 19, 15, 10)
#' Tarone.test(N, M)
Tarone.test <- function(N, M) {

  #Check validity of inputs
  if(!(all(N == as.integer(N)))) { stop("Error: Number of trials should be integers"); }
  if(min(N) < 1) { stop("Error: Number of trials should be positive"); }
  if(!(all(M == as.integer(M)))) { stop("Error: Count values should be integers"); }
  if(min(M) < 0) { stop("Error: Count values cannot be negative"); }
  if(any(M > N)) { stop("Error: Observed count value exceeds number of trials"); }

  #Set description of test and data
  method      <- "Tarone's Z test";
  data.name   <- paste0(deparse(substitute(M)), " successes from ",
                        deparse(substitute(N)), " trials");

  #Set null and alternative hypotheses
  null.value  <- 0;
  attr(null.value, "names") <- "dispersion parameter";
  alternative <- "greater";

  #Calculate test statistics
  estimate    <- sum(M)/sum(N);
  attr(estimate, "names") <- "proportion parameter";
  S           <- ifelse(estimate == 1, sum(N),
                        sum((M - N*estimate)^2/(estimate*(1 - estimate))));
  statistic   <- (S - sum(N))/sqrt(2*sum(N*(N-1)));
  attr(statistic, "names") <- "z";

  #Calculate p-value
  p.value     <- 2*pnorm(-abs(statistic), 0, 1);
  attr(p.value, "names") <- NULL;

  #Create htest object
  TEST        <- list(method = method, data.name = data.name,
                      null.value = null.value, alternative = alternative,
                      estimate = estimate, statistic = statistic, p.value = p.value);
  class(TEST) <- "htest";
  TEST;
}


#------------------------------------------------------------------------------#
#' C(alpha) test from the epiphy package.
#'
#' The C(alpha) test is a test of the binomial distribution against the
#' alternative of the beta-binomial distribution.
#'
#' It is based on calculation of a test statistic, z, that has an asymptotic
#' standard normal distribution under the null hypothesis. It is one-sided (in
#' the way that the alternative is aggregation, not just "non-randomness"), thus
#' with a confidence level of 95%, the null hypothesis is rejected when z >
#' 1.64. When all the sampling units contain the same total number of
#' individuals, n, the test statistic is calculated from:
#'
#' z = (n(N - 1)I - Nn)/(2Nn(n - 1))^(1/2)
#'
#' where N is the number of sampling units, and I, Fisher's index of aggregation
#' for incidence data.
#'
#' @param x The output of the \code{\link{agg_index}} function with
#'     \code{method = "fisher"} as parameter.
#' @param ... Not yet implemented.
#'
#' @returns
#' Same kind of object as the one returns by the stats
#' \code{\link[stats]{chisq.test}} function for example.
#'
#' @examples
#' # For incidence data:
#' my_incidence <- incidence(tobacco_viruses)
#' my_fisher <- agg_index(my_incidence, method = "fisher")
#' calpha.test(my_fisher)
#'
#' @seealso \code{\link{chisq.test}}, \code{\link{z.test}}
#'
#' @references
#'
#' Neyman J. 1959. Optimal asymptotic tests of composite statistical hypotheses.
#' In: Probability and Statistics, 213-234. Wiley, New York.
#'
#' Tarone RE. 1979. Testing the goodness of fit of the binomial distribution.
#' Biometrika, 66(3): 585-590.
#'
#' @export
#------------------------------------------------------------------------------#
calpha.test <- function(x, ...) UseMethod("calpha.test")

#------------------------------------------------------------------------------#
#' @rdname calpha.test
#' @method calpha.test fisher
#' @export
#------------------------------------------------------------------------------#
calpha.test.fisher <- function(x, ...) {
  call  <- match.call() # TODO: Not yet used.
  dname <- deparse(substitute(x))
  N     <- x[["N"]]
  switch (x[["flavor"]],
          "count" = {
            stop("No calpha.test for count data.",
                 call. = FALSE)
          },
          "incidence" = {
            n <- x[["n"]]
            stat <- ((n * (N - 1) * x[["index"]]) - (N * n)) /
              sqrt(2 * N * n * (n - 1)) # a one-sided test in the sens that...
          }
  )
  pval <- 2 * pnorm(abs(stat), lower.tail = FALSE)
  structure(list(
    statistic = c("z" = stat),
    #parameter =,
    p.value   = pval,
    method    = "C(alpha) test",
    data.name = dname
    #observed =,
    #expected =,
    #residuals =,
    #stdres =
  ), class = "htest")

}



#' Tarones test for Stratified OR (taken from https://github.com/the8thday/ClinStats/blob/master/R/Tarones.R)
#'
#' \code{calcTaronesTest} is ued to test whether the OR in different strata are different
#'
#' @param mylist A list of matrix Stratified by a Variable
#' @param referencerow Unexposed row
#'
#' @return string with results
#' @export
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
  tarone <- metafor::rma.mh(ptrt, htrt, pctrl, hctrl, ntrt, nctrl)
  pvalue <- tarone$TAp
  print(paste("Pvalue for Tarone's test =", pvalue))
}
