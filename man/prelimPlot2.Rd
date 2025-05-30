% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prelimnary.R
\name{prelimPlot2}
\alias{prelimPlot2}
\title{Preliminary Plot 2 for Dose Response Data, with x continuous.}
\usage{
prelimPlot2(
  testdata,
  ylab = "Response",
  xlab = "Test Concentration [nominal, mg a.s./L]",
  title = "Measured Variable",
  dose_col = "Dose",
  response_col = "Response"
)
}
\arguments{
\item{testdata}{A data frame containing the dose and response data.}

\item{ylab}{A string for the y-axis label. Default is "Response".}

\item{xlab}{A string for the x-axis label. Default is \code{"Test Concentration [nominal, mg a.s./L]"}.}

\item{title}{A string for the plot title. Default is "Measured Variable".}

\item{dose_col}{name of the dose column, default being "Dose".}

\item{response_col}{name of the response column.}
}
\value{
A ggplot object.
}
\description{
This function generates a scatter plot of response against dose on a log1p scale.
}
