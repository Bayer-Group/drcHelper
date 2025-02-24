% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Endpoints.R
\name{getEndpoint}
\alias{getEndpoint}
\title{Obtain Endpoint (NOEC) according to a series of p-values}
\usage{
getEndpoint(pvals, doses = c("Control", "B", "C", "D"), procedure = "stepDown")
}
\arguments{
\item{pvals}{pvals from a tests}

\item{doses}{corresponding doses}

\item{procedure}{procedure to obtain NOEC}
}
\description{
Obtain Endpoint (NOEC) according to a series of p-values
}
\examples{
pvals <- c(0.01, 0.03, 0.07, 0.08)
doses <- c("Control", "B", "C", "D")
getEndpoint(pvals, doses)
}
