## unit testing: test_ordinal.R #######


#' Get the EC50 Estimate from a Model
#'
#' This function calculates the EC50 (the dose at which 50% of the maximum effect is observed)
#' from a fitted model. It supports generalized linear models (GLMs) and generalized linear mixed models (GLMMs).
#'
#' @param mod A fitted model object. This can be of class "glm" or "glmmPQL", or other models compatible with the `ED` function.
#'
#' @return A data frame containing the following columns:
#' \item{EC50}{The estimated EC50 value.}
#' \item{lower}{The lower bound of the 95% confidence interval for the EC50.}
#' \item{upper}{The upper bound of the 95% confidence interval for the EC50.}
#' \item{se}{The standard error of the EC50 estimate.}
#'
#' @examples
#' # Assuming `fit` is a fitted glm model
#' fit <- glm(yt ~ log(dose),family= quasibinomial(link="logit"),data=pvi_example)
#' ec50_result <- getEC50(fit)
#' print(ec50_result)
#'
#' @export
getEC50 <- function(mod,approximate = FALSE){
  if (!inherits(mod, c("glm", "glmmPQL","drc"))) {
    stop("The model must be of class 'glm' or 'glmmPQL' of 'drc'.")
  }
  if("glm" %in% class(mod)){
    ec50 <- MASS::dose.p(mod,p=0.5)
    se <- attr(ec50,"SE")
    xmin <- as.numeric(ec50 - 1.96*se)
    xmax <- as.numeric(ec50 + 1.96*se)
    SE <- backCalcSE(se = as.numeric(se), mu = as.numeric(ec50), approximate = approximate)
    res <- data.frame(EC50=exp(as.numeric(ec50)),
                      lower=exp(xmin),upper=exp(xmax),se=SE)
  }else{
    if("glmmPQL" %in% class(mod)){
      res1 <- dose.p.glmmPQL(mod,cf=1:2,p=0.5)
      se <- attr(res1,"SE")
      SE <- backCalcSE(se = as.numeric(se), mu = as.numeric(res1[1]),
                       approximate = approximate)

      res <- data.frame(EC50=exp(res1[1]),lower=exp(res1[1]-1.96*se),
                        upper=exp(res1[1]+1.96*se),se=SE)
    }else{
      res1 <- ED(mod,50,interval="delta")
      res <- data.frame(EC50=res1[1,"Estimate"],lower=res1[1,"Lower"],upper=res1[1,"Upper"],se=res1[1,"Std. Error"])

    }
  }
  return(res)
}

#' Calculate Dose Probability for GLMM using PQL
#'
#' This function calculates the dose probability for a generalized linear mixed model (GLMM)
#' using Penalized Quasi-Likelihood (PQL).
#'
#' @param obj A fitted GLMM object of class "glmmPQL".
#' @param cf A numeric vector specifying the coefficients to be used for the calculation (default is 1:2).
#' @param p A numeric value representing the probability level (default is 0.5).
#'
#' @return A structured object of class "glm.dose" containing the estimated dose probability and its standard error.
#'
#' @examples
#' # Assuming `glmm_model` is a fitted glmmPQL model
#' library(MASS)
#' glmm_model <- glmmPQL(yt ~ dose,random= ~1 | Obs,family= quasibinomial(link="logit"),data=pvi_example)
#' dose_result <- dose.p.glmmPQL(glmm_model)
#' print(dose_result)
#'
#' @export
dose.p.glmmPQL <- function(obj,cf=1:2,p=0.5){
  eta <- obj$family$linkfun(p)
  b <- obj$coefficients$fixed[cf]
  x.p <- (eta - b[1L])/b[2L]
  names(x.p) <- paste("p = ", format(p), ":", sep = "")
  pd <- -cbind(1, x.p)/b[2L]
  SE <- sqrt(((pd %*% vcov(obj)[cf, cf]) * pd) %*% c(1, 1))
  res <- structure(x.p, SE = SE, p = p)
  class(res) <- "glm.dose"
  res
}

#' Back Calculate Standard Error from Lognormal Parameters
#'
#' This function calculates the standard error (SE) of a lognormally distributed variable
#' based on the mean (mu) and the standard error of the logarithm (se).
#'
#' @param se The standard error of the logarithm.
#' @param mu The mean of the logarithm.
#' @param approximate Logical. If TRUE, uses an approximation; otherwise, calculates based on the variance of the lognormal distribution.
#'
#' @return The back-calculated standard error.
#' @export
#'
#' @examples
#' backCalcSE(se = 0.1, mu = 0)
#' backCalcSE(se = 0.1, mu = 0, approximate = TRUE)
backCalcSE <- function(se,mu,approximate = FALSE){
  if (!is.numeric(se) || !is.numeric(mu)) {
    stop("Both 'se' and 'mu' must be numeric values.")
  }
 if(approximate){
   SE <- exp(mu)*se
 }else{
   SE <- sqrt((exp(se^2)-1)*exp(2*mu+se^2)) ## Variance of the lognormal distribution
 }
  return(SE)
}
