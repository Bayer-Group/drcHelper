% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drc_Helper.R
\name{calcNW}
\alias{calcNW}
\title{Calculate Normalized Width for Effect Concentration Estimates}
\usage{
calcNW(x, ED = "ZG")
}
\arguments{
\item{x}{A data frame containing EC estimates, typically output from ED
function. Must contain columns 'Upper', 'Lower', and 'Estimate'.}

\item{ED}{Character string indicating the source of EC estimates. Must be either
"ZG" (default) or "drc".}
}
\value{
A data frame with two columns:
\itemize{
\item NW: Normalized width calculated as (Upper - Lower) / Estimate
\item Rating: Categorical rating of the normalized width
}
Row names are preserved from input for "ZG" or modified to "EC_x" format
for "drc".
}
\description{
Calculates the normalized width of confidence intervals for effect concentration
(EC) estimates by dividing the interval width by the estimate value. Also
provides a rating for the calculated normalized width.
}
\examples{
# Example with ZG format
ec_data <- data.frame(
  Lower = c(1.2, 2.3),
  Upper = c(1.8, 3.1),
  Estimate = c(1.5, 2.7),
  row.names = c("EC10", "EC50")
)
calcNW(ec_data, ED = "ZG")

# Example with drc format
drc_data <- data.frame(
  Lower = c(1.2, 2.3),
  Upper = c(1.8, 3.1),
  Estimate = c(1.5, 2.7),
  row.names = c("Dose:1:10", "Dose:1:50")
)
calcNW(drc_data, ED = "drc")

}
\seealso{
\code{\link{ECx_rating}} for the rating system used
}
