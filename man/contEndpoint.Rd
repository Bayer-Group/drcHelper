% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Endpoints.R
\name{contEndpoint}
\alias{contEndpoint}
\title{get Endpoint for continuous data according to results of a series of tests}
\usage{
contEndpoint(
  paov,
  pks,
  pnormal,
  phomogeneity,
  monotonicity,
  william,
  dunnett,
  dunn,
  jonckheere,
  procedure = "stepDown",
  doses = c("A", "B", "C", "D")
)
}
\arguments{
\item{paov}{Anova p-value}

\item{pks}{Kolmogorov-Smirnov test p-value}

\item{pnormal}{Normal test p-value}

\item{phomogeneity}{Homogeneity variance test p-value}

\item{monotonicity}{Monotonicity test p-value}

\item{william}{Williams' test p-value}

\item{dunnett}{Dunnett's test p-value}

\item{dunn}{Dunn's test p-value}

\item{jonckheere}{Jonckheere-Tepstra test p-value}

\item{procedure}{step down of up}

\item{doses}{tested doses}
}
\value{
NOEC along with an attribute indicating which test was used.
}
\description{
get Endpoint for continuous data according to results of a series of tests
}
\details{
Purpose: The contEndpoint function computes the No Observed Effect Concentration (NOEC) based on the results of various statistical tests. It evaluates these tests'
p-values to determine which statistical analysis is most appropriate.The function first checks the
significance of the monotonicity test. Depending on the results, it selects the appropriate
p-value from the relevant test (Dunnett, Dunn, Williams, or Jonckheere) to compute the NOEC.
The function returns the NOEC along with an attribute indicating which test was used. Original function
used directly the williams' test or dunnett's test objects as input, which can cause conflicts when
using different functions to get the pvals.
}
\examples{
contEndpoint(paov = 0.01, pks = 0.03, pnormal = 0.06,
phomogeneity = 0.07, monotonicity = c(0.1,0.2),
william = c(0.07, 0.06, 0.03, 0.01), dunnett = c(0.07, 0.06, 0.03, 0.01),
dunn = c(0.07, 0.06, 0.03, 0.01),
jonckheere = c(0.07, 0.06, 0.03, 0.01),
procedure = "stepDown",
doses = c("Control","A", "B", "C", "D"))
}
