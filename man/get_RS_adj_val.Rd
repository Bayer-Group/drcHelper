% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RSCABS_AO.R
\name{get_RS_adj_val}
\alias{get_RS_adj_val}
\title{Calculate Rao-Scott Adjusted Values for Clustered Binary Data}
\usage{
get_RS_adj_val(group, replicate, affected, total)
}
\arguments{
\item{group}{Vector of treatment group identifiers}

\item{replicate}{Vector of replicate/tank identifiers within treatment groups}

\item{affected}{Vector of counts of affected subjects (fish with injuries) in each replicate}

\item{total}{Vector of total subjects (fish) in each replicate}
}
\value{
A tibble containing the following columns:
\item{grp}{Treatment group identifier}
\item{x}{Total number of affected subjects in the treatment group}
\item{n}{Total number of subjects in the treatment group}
\item{m}{Number of replicates in the treatment group}
\item{p_hat}{Estimated proportion of affected subjects in the treatment group}
\item{b}{Binomial variance of p_hat}
\item{v}{Estimated variance accounting for clustering}
\item{D}{Design effect (ratio of cluster-adjusted variance to binomial variance)}
\item{n_tilde}{Adjusted sample size accounting for clustering}
\item{x_tilde}{Adjusted number of affected subjects accounting for clustering}
}
\description{
This function calculates the Rao-Scott adjustment for clustered binary data
to account for intra-cluster correlation when analyzing dose-response relationships.
}
\details{
The function is modified based on the function written by Allen Olmstead.
It first aggregates data by treatment group to calculate overall proportions.
It then computes the variance within each treatment group accounting for clustering,
and calculates a design effect (D) as the ratio of cluster-adjusted variance to
binomial variance. The sample size and affected counts are then adjusted by
dividing by this design effect.
}
\author{
Allen Olmstead
}
