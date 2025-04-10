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
\value{
NOEC
}
\description{
Obtain Endpoint (NOEC) according to a series of p-values
}
\details{
If all p-values are NA, it initializes them to zeros. If there are fewer p-values than doses,
it pads the p-values with zeros.It checks which doses are significant by comparing the p-values
to a threshold (0.05).Depending on the specified procedure, it either steps down from the highest
dose to find the NOEC or steps up from the lowest dose..
}
\examples{
pvals <- c(0.01, 0.03, 0.07)
doses <- c("Control", "B", "C", "D")
getEndpoint(pvals, doses)
}
