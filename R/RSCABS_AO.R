#' Calculate Rao-Scott Adjusted Values for Clustered Binary Data
#'
#' This function calculates the Rao-Scott adjustment for clustered binary data
#' to account for intra-cluster correlation when analyzing dose-response relationships.
#'
#' @param group Vector of treatment group identifiers
#' @param replicate Vector of replicate/tank identifiers within treatment groups
#' @param affected Vector of counts of affected subjects (fish with injuries) in each replicate
#' @param total Vector of total subjects (fish) in each replicate
#'
#' @return A tibble containing the following columns:
#'   \item{grp}{Treatment group identifier}
#'   \item{x}{Total number of affected subjects in the treatment group}
#'   \item{n}{Total number of subjects in the treatment group}
#'   \item{m}{Number of replicates in the treatment group}
#'   \item{p_hat}{Estimated proportion of affected subjects in the treatment group}
#'   \item{b}{Binomial variance of p_hat}
#'   \item{v}{Estimated variance accounting for clustering}
#'   \item{D}{Design effect (ratio of cluster-adjusted variance to binomial variance)}
#'   \item{n_tilde}{Adjusted sample size accounting for clustering}
#'   \item{x_tilde}{Adjusted number of affected subjects accounting for clustering}
#'
#' @author Originally by Allen Olmstead
#' @details
#' The function first aggregates data by treatment group to calculate overall proportions.
#' It then computes the variance within each treatment group accounting for clustering,
#' and calculates a design effect (D) as the ratio of cluster-adjusted variance to
#' binomial variance. The sample size and affected counts are then adjusted by
#' dividing by this design effect.
#'
#' @examples
#' # Calculate adjusted values for the fish injury data
#' adj_vals <- get_RS_adj_val(
#'   dat_bcs1$tmt,
#'   dat_bcs1$tank,
#'   dat_bcs1$S1 + dat_bcs1$S2 + dat_bcs1$S3,
#'   dat_bcs1$total
#' )
get_RS_adj_val <- function(group, replicate, affected, total) {
  # create data frame from input vectors
  dat <- tibble(grp = group, rep = replicate, aff = affected, tot = total)

  # create aggregates by dose levels
  agg <- group_by(dat, grp) %>%
    summarize(x = sum(aff), n = sum(tot), m = n()) %>%
    mutate(p_hat = x/n,
           b = p_hat*(1 - p_hat)/n)

  # add aggregates to original data frame
  dat <- left_join(dat, agg, by = "grp") %>%
    mutate(r2 = (aff - tot*p_hat)^2) # square of residuals

  # calculate subgroup variances
  subgrp_var <- group_by(dat, grp, m, n) %>%
    summarize(sum_r2 = sum(r2), .groups = "drop") %>%
    mutate(v = m*sum_r2/n^2/(m - 1))

  agg$v <- subgrp_var$v

  # calculate adjusted n and x values
  mutate(agg, D = ifelse(v/b < 1, 1, v/b),
         n_tilde = n/D,
         x_tilde = x/D)
}




#' Calculate Cochran-Armitage Trend Test Z-Statistic
#'
#' This function calculates the Z-statistic for the Cochran-Armitage trend test
#' using adjusted counts and sample sizes.
#'
#' @param adj_x Vector of adjusted affected counts for each treatment group
#' @param adj_n Vector of adjusted sample sizes for each treatment group
#'
#' @return The Z-statistic for the Cochran-Armitage trend test
#'
#' @details
#' The Cochran-Armitage trend test examines whether there is a linear trend in
#' proportions across ordered categories (treatment groups). This implementation
#' uses adjusted values to account for clustering in the data.
#'
#' The function assigns scores (1, 2, 3, ...) to the treatment groups and
#' calculates the Z-statistic using the formula:
#' Z = [sum(adj_x*d) - N*p_bar*d_bar] / sqrt[p_bar*(1-p_bar)*(sum(adj_n*d^2) - N*d_bar^2)]
#' where:
#' - d are the scores (1, 2, 3, ...)
#' - N is the total adjusted sample size
#' - d_bar is the weighted average of scores
#' - p_bar is the overall proportion of affected subjects
#'
#' @examples
#' # Get adjusted values
#' adj_vals <- get_RS_adj_val(
#'   dat_bcs1$tmt,
#'   dat_bcs1$tank,
#'   dat_bcs1$S1 + dat_bcs1$S2 + dat_bcs1$S3,
#'   dat_bcs1$total
#' )
#' # Calculate Z-statistic
#' Z <- get_CA_Z(adj_vals$x_tilde, adj_vals$n_tilde)
get_CA_Z <- function(adj_x, adj_n) {
  d <- 1:length(adj_x)  # Assign scores 1, 2, 3, ... to treatment groups
  N <- sum(adj_n)       # Total adjusted sample size
  d_bar <- sum(d*adj_n)/N  # Weighted average of scores
  p_bar <- sum(adj_x)/N    # Overall proportion of affected subjects

  # Calculate numerator and denominator of Z-statistic
  num <- sum(adj_x*d) - N*p_bar*d_bar
  den <- p_bar*(1 - p_bar)*(sum(adj_n*(d)^2) - N*d_bar^2)

  # Return Z-statistic
  num/sqrt(den)
}


#' Run Rao-Scott Adjusted Cochran-Armitage Trend Test
#'
#' This function is a wrapper that performs the Rao-Scott adjusted Cochran-Armitage
#' trend test for clustered binary data.
#'
#' @param group Vector of treatment group identifiers
#' @param replicate Vector of replicate/tank identifiers within treatment groups
#' @param affected Vector of counts of affected subjects (fish with injuries) in each replicate
#' @param total Vector of total subjects (fish) in each replicate
#'
#' @return A list containing:
#'   \item{interm_values}{A tibble with intermediate values from the Rao-Scott adjustment}
#'   \item{Z}{The Z-statistic for the Cochran-Armitage trend test}
#'
#' @details
#' This function combines the Rao-Scott adjustment and the Cochran-Armitage trend test
#' to analyze dose-response relationships in clustered data. It first calculates
#' adjusted values accounting for clustering, then uses these values to perform
#' the trend test.
#'
#' The p-value can be calculated using: 2 * (1 - pnorm(abs(Z)))
#' @export
#' @examples
#' # Test for trend in injury rates across treatment groups
#' # Considering S1, S2, and S3 as "affected"
#' result <- run_RSCA(
#'   dat_bcs1$tmt,
#'   dat_bcs1$tank,
#'   dat_bcs1$S1 + dat_bcs1$S2 + dat_bcs1$S3,
#'   dat_bcs1$total
#' )
#'
#' # View intermediate values
#' print(result$interm_values)
#'
#' # View Z-statistic
#' print(result$Z)
#'
#' # Calculate p-value
#' p_value <- 2 * (1 - pnorm(abs(result$Z)))
#' print(paste("p-value:", p_value))
run_RSCA <- function(group, replicate, affected, total) {
  interm_values <- get_RS_adj_val(group, replicate, affected, total)
  Z <- get_CA_Z(interm_values$x_tilde, interm_values$n_tilde)
  list(interm_values = interm_values, Z = Z)
}
