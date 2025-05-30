% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dose_response_simulation.R
\name{simDRdata}
\alias{simDRdata}
\title{Wrapper around rdrm}
\usage{
simDRdata(
  nosim,
  fct,
  mpar,
  xerror,
  xpar = 1,
  yerror = "rnorm",
  ypar = c(0, 1),
  onlyY = FALSE
)
}
\arguments{
\item{nosim}{numeric. The number of simulated curves to be returned.}

\item{fct}{list. Any built-in function in the package drc or a list with similar components.}

\item{mpar}{numeric. The model parameters to be supplied to fct.}

\item{xerror}{numeric or character. The distribution for the dose values.}

\item{xpar}{numeric vector supplying the parameter values defining the distribution for the dose values. If xerror is a distribution then remember that the number of dose values also is part of this argument (the first argument).}

\item{yerror}{numeric or character. The error distribution for the response values.}

\item{ypar}{numeric vector supplying the parameter values defining the error distribution for the response values.}

\item{onlyY}{logical. If TRUE then only the response values are returned (useful in simulations). Otherwise both dose values and response values (and for binomial data also the weights) are returned.}
}
\value{
a data frame
}
\description{
Wrapper around rdrm to generate a data frame or a tibble object instead of a list of two matrices
}
\examples{
\dontrun{
dat <- rdrm(1, LL.3(), c(`b:(Intercept)` = 3,
`d:(Intercept)` = 8, `e:(Intercept)` = 3),
xerror=c(0, 0, 0, 0, 0, 0, 0.94, 0.94, 0.94, 1.88, 1.88, 1.88, 3.75,
         3.75, 3.75, 7.5, 7.5, 7.5, 15, 15, 15, 30, 30, 30),
yerror = "rnorm", ypar = c(0, 0.6))
dat <- data.frame(Dose = dat$x[1,], Response = dat$y[1,])
simDRdata(10, LL.3(), c(`b:(Intercept)` = 3,
`d:(Intercept)` = 8, `e:(Intercept)` = 3),
xerror=c(0, 0, 0, 0, 0, 0, 0.94, 0.94, 0.94, 1.88, 1.88, 1.88, 3.75,
         3.75, 3.75, 7.5, 7.5, 7.5, 15, 15, 15, 30, 30, 30),
yerror = "rnorm", ypar = c(0, 0.6))
}
}
\seealso{
\code{\link[drc:rdrm]{drc::rdrm()}}, \code{\link[MCPMod:genDFdata]{MCPMod::genDFdata()}}
}
\keyword{simulation}
