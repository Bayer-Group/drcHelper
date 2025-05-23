% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RSCABS.R
\name{prepDataRSCABS}
\alias{prepDataRSCABS}
\title{Prepares data for an RSCABS analysis}
\usage{
prepDataRSCABS(
  Effect = "",
  Data = {
 },
  Treatment = "",
  Replicate = ""
)
}
\arguments{
\item{Effect}{The endpoint being converted.}

\item{Data}{The tall formatted data set.}

\item{Treatment}{The name of the treatment variable.}

\item{Replicate}{The name of the replicate variable.}
}
\value{
Returns a list containing:
\item{x.i.j}{
matrix containing the number of observed "successes" for replicate i on treatment j.
}
\item{n.i.j}{
matrix containing the number of observations for replicate i on treatment j.
}
\item{m.i}{
matrix of number replicates in each treatment-replicate combination.
}
\item{K.max}{
The maximum severity score for the endpoint.
}
}
\description{
Prepares data for an RSCABS analysis
}
\author{
Joe Swintek
}
