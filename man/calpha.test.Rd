% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/integrated_calpha.R
\name{calpha.test}
\alias{calpha.test}
\alias{calpha.test.fisher}
\title{C(alpha) test from the epiphy package.}
\usage{
calpha.test(x, ...)

\method{calpha.test}{fisher}(x, ...)
}
\arguments{
\item{x}{The output of the \code{\link[epiphy]{agg_index}} function with
\code{method = "fisher"} as parameter.}

\item{...}{Not yet implemented.}
}
\value{
Same kind of object as the one returns by the stats
\code{\link[stats]{chisq.test}} function for example.
}
\description{
The C(alpha) test is a test of the binomial distribution against the
alternative of the beta-binomial distribution. Note that this is not exported
to the package namespace but kept internal. The license is MIT with the
COPYRIGHT HOLDER: Christophe Gigot and YEAR: 2023 (included in source code).
The function is included for validation purpose. Please use epiphy package
for related calculation in a non-GLP environment.
}
\details{
It is based on calculation of a test statistic, z, that has an asymptotic
standard normal distribution under the null hypothesis. It is one-sided (in
the way that the alternative is aggregation, not just "non-randomness"), thus
with a confidence level of 95\%, the null hypothesis is rejected when z >
1.64. When all the sampling units contain the same total number of
individuals, n, the test statistic is calculated from:

z = (n(N - 1)I - Nn)/(2Nn(n - 1))^(1/2)

where N is the number of sampling units, and I, Fisher's index of aggregation
for incidence data.
}
\examples{
# For incidence data:
# my_incidence <- epiphy::incidence(epiphy::tobacco_viruses)
# my_fisher <- epiphy::agg_index(my_incidence, method = "fisher")
my_fisher <- structure(list(index = 3.14396182555326,
name = "Fisher's index of dispersion", flavor = "incidence", N = 75L, n = 40L),
class = c("fisher", "agg_index"))
drcHelper:::calpha.test(my_fisher)

}
\references{
Neyman J. 1959. Optimal asymptotic tests of composite statistical hypotheses.
In: Probability and Statistics, 213-234. Wiley, New York.

Tarone RE. 1979. Testing the goodness of fit of the binomial distribution.
Biometrika, 66(3): 585-590.
}
\seealso{
\code{\link{chisq.test}}, \code{\link[epiphy]{z.test}}
}
