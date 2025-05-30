% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ordinal.R
\name{backCalcSE}
\alias{backCalcSE}
\title{Back Calculate Standard Error from Lognormal Parameters}
\usage{
backCalcSE(se, mu, approximate = FALSE)
}
\arguments{
\item{se}{The standard error of the logarithm.}

\item{mu}{The mean of the logarithm.}

\item{approximate}{Logical. If TRUE, uses an approximation;
otherwise, calculates based on the variance of the lognormal distribution.}
}
\value{
The back-calculated standard error.
}
\description{
This function calculates the standard error (SE) of a lognormally distributed variable
based on the mean (mu) and the standard error of the logarithm (se).
}
\examples{
backCalcSE(se = 0.1, mu = 0)
backCalcSE(se = 0.1, mu = 0, approximate = TRUE)
}
