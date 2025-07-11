% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/integrated_tarone.R
\name{calcTaronesTest}
\alias{calcTaronesTest}
\alias{Tarones}
\title{Tarones test for Stratified OR (ClinStats version)}
\usage{
calcTaronesTest(mylist, referencerow = 2)
}
\arguments{
\item{mylist}{A list of matrix Stratified by a Variable}

\item{referencerow}{Unexposed row}
}
\value{
string with a list of p-values
}
\description{
\code{calcTaronesTest} is used to test whether the OR in different strata are different.
It is integrated from
https://github.com/the8thday/ClinStats/blob/master/R/Tarones.R). The original
MIT license is included in the source code. The function
is included for validation purpose. Please use the most updated function in
ClinStats package for related calculation in a non-GLP environment.
}
\details{
modified the output to return p-values, need metafor package to run.
This function is not really used in GLP calculations as it is not designed for
ecotox studies where usually only one control is included.
However, it could be useful in refinement analyses.
}
\examples{
mymatrix1 <- matrix(c(4,5,5,103),nrow=2,byrow=TRUE)
colnames(mymatrix1) <- c("Disease","Control")
rownames(mymatrix1) <- c("Exposure","Unexposed")
mymatrix2 <- matrix(c(10,3,5,43),nrow=2,byrow=TRUE)
colnames(mymatrix2) <- c("Disease","Control")
rownames(mymatrix2) <- c("Exposure","Unexposed")
mylist <- list(mymatrix1,mymatrix2)
calcTaronesTest(mylist)
\dontrun{
calcTaronesTest(mymatrix1)
}
}
