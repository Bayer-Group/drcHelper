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
#' @export
#' @author Allen Olmstead
#' @details
#' The function is modified based on the function written by Allen Olmstead.
#' It first aggregates data by treatment group to calculate overall proportions.
#' It then computes the variance within each treatment group accounting for clustering,
#' and calculates a design effect (D) as the ratio of cluster-adjusted variance to
#' binomial variance. The sample size and affected counts are then adjusted by
#' dividing by this design effect.
get_RS_adj_val <- function(group, replicate, affected, total) {
  # Create data frame from input vectors
  dat <- tibble(grp = group, rep = replicate, aff = affected, tot = total)

  # Create aggregates by dose levels
  agg <- dat %>%
    group_by(.data$grp) %>%
    summarize(x = sum(.data$aff),
              n = sum(.data$tot),
              m = n(),
              .groups = "drop") %>%
    mutate(p_hat = .data$x / .data$n,
           b = .data$p_hat * (1 - .data$p_hat) / .data$n)

  # Add aggregates to original data frame
  dat <- dat %>%
    left_join(agg, by = "grp") %>%
    mutate(r2 = (.data$aff - .data$tot * .data$p_hat)^2) # square of residuals

  # Calculate subgroup variances
  subgrp_var <- dat %>%
    group_by(.data$grp, m, n) %>%
    summarize(sum_r2 = sum(.data$r2), .groups = "drop") %>%
    mutate(v = .data$m * .data$sum_r2 / .data$n^2 / (.data$m - 1))

  agg$v <- subgrp_var$v

  # Calculate adjusted n and x values
  agg <- agg %>%
    mutate(D = ifelse(.data$v / .data$b < 1, 1, .data$v / .data$b),
           n_tilde = .data$n / .data$D,
           x_tilde = .data$x / .data$D)

  return(agg)
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
#' @author Originally by Allen Olmstead
#'
#' @details
#' The Cochran-Armitage trend test examines whether there is a linear trend in
#' proportions across ordered categories (treatment groups). This implementation
#' uses adjusted values to account for clustering in the data.
#'
#' The function assigns scores (1, 2, 3, ...) to the treatment groups and
#' calculates the Z-statistic using the formula:
#' ``\deqn{Z = [sum(adj_x*d) - N*p_bar*d_bar] / sqrt[p_bar*(1-p_bar)*(sum(adj_n*d^2) - N*d_bar^2)]``
#' where:
#' - d are the scores (1, 2, 3, ...)
#' - N is the total adjusted sample size
#' - d_bar is the weighted average of scores
#' - p_bar is the overall proportion of affected subjects
#'
#' @export
#' @examples
#' # Get adjusted values
#' data(dat_bcs1)
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
#' @param correction continuity correction when there is 1, default is 0, can be changed to 0.5.
#'
#' @return A list containing:
#'   \item{interm_values}{A tibble with intermediate values from the Rao-Scott adjustment}
#'   \item{Z}{The Z-statistic for the Cochran-Armitage trend test}
#' @author Originally by Allen Olmstead
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
run_RSCA <- function(group, replicate, affected, total,correction=0) {
  affected <- affected + correction
  total <- total + correction
  interm_values <- get_RS_adj_val(group, replicate, affected, total)
  Z <- get_CA_Z(interm_values$x_tilde, interm_values$n_tilde)
  list(interm_values = interm_values, Z = Z)
}




#' Run Rao-Scott Adjusted Cochran-Armitage Trend Test for a Specific Injury Threshold
#'
#' This function performs the Rao-Scott adjusted Cochran-Armitage trend test for
#' a specific injury threshold, counting fish with injuries at or above that threshold
#' as "affected".
#'
#' @param data A data frame containing fish injury data
#' @param threshold Numeric value indicating the injury threshold (e.g., 1 for S1+)
#' @param score_cols Character vector of column names containing injury scores (default: NULL, auto-detected)
#' @param treatment_col Name of the column containing treatment groups (default: "tmt")
#' @param replicate_col Name of the column containing tank/replicate IDs (default: "tank")
#' @param total_col Name of the column containing total counts (default: "total")
#' @param direction Character string indicating threshold direction: "greater" for ≥ threshold,
#'        "less" for ≤ threshold (default: "greater")
#' @param alternative Character string specifying the alternative hypothesis:
#'        "two.sided" (default), "greater" (proportion increases with treatment level),
#'        or "less" (proportion decreases with treatment level)
#'
#' @return A list containing:
#'   \item{threshold}{The numeric threshold value}
#'   \item{threshold_label}{Character label for the threshold (e.g., "S1+")}
#'   \item{Z}{Z-statistic from the RSCA test}
#'   \item{p_value}{P-value based on the specified alternative hypothesis}
#'   \item{alternative}{The alternative hypothesis used}
#'   \item{interm_values}{Intermediate values from the Rao-Scott adjustment}
#'   \item{has_zero_counts}{Logical indicating if any treatment group had zero affected individuals}
#'   \item{affected_counts}{Data frame showing affected counts by treatment group}
#'
#' @examples
#' # Example data
#' fish_data <- data.frame(
#'   tmt = rep(c("Control", "Low", "Medium", "High"), each = 3),
#'   tank = paste0(rep(c("Control", "Low", "Medium", "High"), each = 3),
#'                 rep(1:3, times = 4)),
#'   S0 = c(8,7,9, 6,5,7, 4,5,3, 2,3,1),
#'   S1 = c(2,2,1, 3,4,2, 4,3,5, 4,3,5),
#'   S2 = c(0,1,0, 1,1,1, 2,2,1, 3,3,3),
#'   S3 = c(0,0,0, 0,0,0, 0,0,1, 1,1,1),
#'   S4 = c(0,0,0, 0,0,0, 0,0,0, 0,0,0),
#'   total = rep(10, 12)
#' )
#'
#' # Two-sided test for trend in S2+ injuries
#' result_two_sided <- run_threshold_RSCA(fish_data, threshold = 2)
#'
#' # One-sided test (proportion increases with treatment level)
#' result_greater <- run_threshold_RSCA(fish_data, threshold = 2,
#'                                     alternative = "greater")
#'
#' # One-sided test (proportion decreases with treatment level)
#' result_less <- run_threshold_RSCA(fish_data, threshold = 2,
#'                                  alternative = "less")
#'
#' @importFrom dplyr group_by summarize mutate select left_join
#' @importFrom stats pnorm
#' @export
run_threshold_RSCA <- function(data, threshold,
                               score_cols = NULL,
                               treatment_col = "tmt",
                               replicate_col = "tank",
                               total_col = "total",
                               direction = "greater",
                               alternative = "two.sided") {

  # Validate alternative parameter
  if (!alternative %in% c("two.sided", "greater", "less")) {
    stop("alternative must be one of: 'two.sided', 'greater', or 'less'")
  }

  # Auto-detect score columns if not provided
  if (is.null(score_cols)) {
    score_cols <- grep("^S[0-9]+$", names(data), value = TRUE)
    if (length(score_cols) == 0) {
      stop("No score columns found with pattern 'S[0-9]+'. Please specify score_cols manually.")
    }
  }

  # Validate inputs
  if (!treatment_col %in% names(data)) {
    stop("Treatment column '", treatment_col, "' not found in data")
  }
  if (!replicate_col %in% names(data)) {
    stop("Replicate column '", replicate_col, "' not found in data")
  }
  if (!total_col %in% names(data)) {
    stop("Total column '", total_col, "' not found in data")
  }
  if (!all(score_cols %in% names(data))) {
    missing <- setdiff(score_cols, names(data))
    stop("Missing score columns: ", paste(missing, collapse = ", "))
  }
  if (!direction %in% c("greater", "less")) {
    stop("Direction must be either 'greater' or 'less'")
  }

  # Extract numeric values from score column names
  score_values <- as.numeric(gsub("\\D", "", score_cols))

  # Determine which scores to include based on direction
  if (direction == "greater") {
    # Greater than or equal to threshold
    selected_cols <- score_cols[score_values >= threshold]
    threshold_label <- paste0("S", threshold, "+")
  } else {
    # Less than or equal to threshold
    selected_cols <- score_cols[score_values <= threshold]
    threshold_label <- paste0("S≤", threshold)
  }

  # Calculate affected counts
  affected <- rowSums(data[, selected_cols, drop = FALSE])

  # Check for zero counts in any treatment group
  has_zero_counts <- any(tapply(affected, data[[treatment_col]], function(x) all(x == 0)))

  # Calculate affected counts by treatment for reporting
  affected_counts <- aggregate(
    affected ~ data[[treatment_col]],
    FUN = sum,
    data = data.frame(affected = affected)
  )
  names(affected_counts) <- c("treatment", "affected")

  # If any treatment group has all zeros, return NA for Z and p-value
  if (has_zero_counts) {
    warning("Some treatment groups have zero affected individuals at threshold ",
            threshold_label, ". RSCA test may not be valid.")

    # Try to run the test anyway, but catch errors
    tryCatch({
      # Run the RSCA test
      result <- run_RSCA(
        group = data[[treatment_col]],
        replicate = data[[replicate_col]],
        affected = affected,
        total = data[[total_col]]
      )

      # Calculate p-value based on alternative hypothesis
      Z <- result$Z
      p_value <- switch(alternative,
                        "two.sided" = 2 * pnorm(-abs(Z)),
                        "greater" = pnorm(-Z),  # H1: proportion increases with treatment level
                        "less" = pnorm(Z)       # H1: proportion decreases with treatment level
      )

      # Return results with warning flag
      return(list(
        threshold = threshold,
        threshold_label = threshold_label,
        Z = Z,
        p_value = p_value,
        alternative = alternative,
        interm_values = result$interm_values,
        has_zero_counts = TRUE,
        affected_counts = affected_counts
      ))
    }, error = function(e) {
      # Return NA results if test fails
      return(list(
        threshold = threshold,
        threshold_label = threshold_label,
        Z = NA,
        p_value = NA,
        alternative = alternative,
        interm_values = NULL,
        has_zero_counts = TRUE,
        affected_counts = affected_counts,
        error = e$message
      ))
    })
  } else {
    # Run the RSCA test
    result <- run_RSCA(
      group = data[[treatment_col]],
      replicate = data[[replicate_col]],
      affected = affected,
      total = data[[total_col]]
    )

    # Calculate p-value based on alternative hypothesis
    Z <- result$Z
    p_value <- switch(alternative,
                      "two.sided" = 2 * pnorm(-abs(Z)),
                      "greater" = pnorm(-Z),  # H1: proportion increases with treatment level
                      "less" = pnorm(Z)       # H1: proportion decreases with treatment level
    )

    # Return results
    return(list(
      threshold = threshold,
      threshold_label = threshold_label,
      Z = Z,
      p_value = p_value,
      alternative = alternative,
      interm_values = result$interm_values,
      has_zero_counts = FALSE,
      affected_counts = affected_counts
    ))
  }
}

#' Run Rao-Scott Adjusted Cochran-Armitage Trend Tests for All Thresholds
#'
#' This function performs the Rao-Scott adjusted Cochran-Armitage trend test for
#' multiple injury thresholds, providing a comprehensive analysis of dose-response
#' relationships at different severity levels.
#'
#' @param data A data frame containing fish injury data
#' @param max_score Maximum score value to consider (default: NULL, auto-detected)
#' @param min_score Minimum score value to consider (default: 1)
#' @param score_cols Character vector of column names containing injury scores (default: NULL, auto-detected)
#' @param treatment_col Name of the column containing treatment groups (default: "tmt")
#' @param replicate_col Name of the column containing tank/replicate IDs (default: "tank")
#' @param total_col Name of the column containing total counts (default: "total")
#' @param direction Character string indicating threshold direction: "greater" for ≥ threshold,
#'        "less" for ≤ threshold (default: "greater")
#' @param alternative Character string specifying the alternative hypothesis:
#'        "two.sided" (default), "greater" (proportion increases with treatment level),
#'        or "less" (proportion decreases with treatment level)
#' @param include_fisher Logical indicating whether to use Fisher's exact test when RSCA fails (default: TRUE)
#'
#' @return A list containing:
#'   \item{results}{Data frame with test results for each threshold}
#'   \item{proportions}{Data frame with proportions of affected fish by treatment and threshold}
#'   \item{detailed_results}{List of detailed results for each threshold}
#'
#' @examples
#' # Example data
#' fish_data <- data.frame(
#'   tmt = rep(c("Control", "Low", "Medium", "High"), each = 3),
#'   tank = paste0(rep(c("Control", "Low", "Medium", "High"), each = 3),
#'                 rep(1:3, times = 4)),
#'   S0 = c(8,7,9, 6,5,7, 4,5,3, 2,3,1),
#'   S1 = c(2,2,1, 3,4,2, 4,3,5, 4,3,5),
#'   S2 = c(0,1,0, 1,1,1, 2,2,1, 3,3,3),
#'   S3 = c(0,0,0, 0,0,0, 0,0,1, 1,1,1),
#'   S4 = c(0,0,0, 0,0,0, 0,0,0, 0,0,0),
#'   total = rep(10, 12)
#' )
#'
#' # Run two-sided tests for all thresholds
#' all_results <- run_all_threshold_tests(fish_data)
#'
#' # Run one-sided tests (proportion increases with treatment level)
#' all_results_greater <- run_all_threshold_tests(
#'   fish_data,
#'   alternative = "greater"
#' )
#'
#' # View results table
#' print(all_results$results)
#' print(all_results_greater$results)
#'
#' @importFrom dplyr group_by summarize mutate select across where
#' @importFrom stats fisher.test
#' @export
run_all_threshold_tests <- function(data,
                                    max_score = NULL,
                                    min_score = 1,
                                    score_cols = NULL,
                                    treatment_col = "tmt",
                                    replicate_col = "tank",
                                    total_col = "total",
                                    direction = "greater",
                                    alternative = "two.sided",
                                    include_fisher = TRUE) {

  # Validate alternative parameter
  if (!alternative %in% c("two.sided", "greater", "less")) {
    stop("alternative must be one of: 'two.sided', 'greater', or 'less'")
  }

  # Auto-detect score columns if not provided
  if (is.null(score_cols)) {
    score_cols <- grep("^S[0-9]+$", names(data), value = TRUE)
    if (length(score_cols) == 0) {
      stop("No score columns found with pattern 'S[0-9]+'. Please specify score_cols manually.")
    }
  }

  # Auto-detect max_score if not provided
  if (is.null(max_score)) {
    score_values <- as.numeric(gsub("\\D", "", score_cols))
    max_score <- max(score_values)
  }

  # Create a list to store results
  results_list <- list()

  # Run tests for each threshold
  thresholds <- min_score:max_score
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    results_list[[i]] <- run_threshold_RSCA(
      data = data,
      threshold = threshold,
      score_cols = score_cols,
      treatment_col = treatment_col,
      replicate_col = replicate_col,
      total_col = total_col,
      direction = direction,
      alternative = alternative
    )
  }

  # Compile results into a data frame
  results_df <- data.frame(
    Threshold = sapply(results_list, function(x) x$threshold_label),
    Z_statistic = sapply(results_list, function(x) x$Z),
    P_value = sapply(results_list, function(x) x$p_value),
    Has_zero_counts = sapply(results_list, function(x) x$has_zero_counts),
    Alternative = sapply(results_list, function(x) x$alternative),
    stringsAsFactors = FALSE
  )

  # Add Fisher's exact test for thresholds with zero counts if requested
  if (include_fisher) {
    for (i in seq_along(thresholds)) {
      if (results_list[[i]]$has_zero_counts && is.na(results_list[[i]]$p_value)) {
        threshold <- thresholds[i]

        # Determine which scores to include based on direction
        score_values <- as.numeric(gsub("\\D", "", score_cols))
        if (direction == "greater") {
          selected_cols <- score_cols[score_values >= threshold]
        } else {
          selected_cols <- score_cols[score_values <= threshold]
        }

        # Calculate affected counts
        affected <- rowSums(data[, selected_cols, drop = FALSE])
        unaffected <- data[[total_col]] - affected

        # Aggregate by treatment
        agg_data <- aggregate(
          cbind(affected, unaffected) ~ data[[treatment_col]],
          FUN = sum
        )
        names(agg_data)[1] <- "treatment"

        # Run Fisher's exact test
        tryCatch({
          cont_matrix <- as.matrix(agg_data[, c("affected", "unaffected")])
          rownames(cont_matrix) <- agg_data$treatment

          # Adjust Fisher's test based on alternative hypothesis
          fisher_result <- switch(alternative,
                                  "two.sided" = fisher.test(cont_matrix, alternative = "two.sided"),
                                  "greater" = fisher.test(cont_matrix, alternative = "greater"),
                                  "less" = fisher.test(cont_matrix, alternative = "less")
          )

          # Update results
          results_df$P_value[i] <- fisher_result$p.value
          results_df$Method[i] <- paste0("Fisher's Exact Test (", alternative, ")")
        }, error = function(e) {
          results_df$Method[i] <- "Not calculable"
        })
      } else {
        results_df$Method[i] <- paste0("RSCA (", alternative, ")")
      }
    }
  }

  # Calculate proportions for each threshold by treatment
  prop_by_treatment <- data.frame(Treatment = unique(data[[treatment_col]]))

  for (threshold in thresholds) {
    # Determine which scores to include based on direction
    score_values <- as.numeric(gsub("\\D", "", score_cols))
    if (direction == "greater") {
      selected_cols <- score_cols[score_values >= threshold]
      threshold_label <- paste0("S", threshold, "+")
    } else {
      selected_cols <- score_cols[score_values <= threshold]
      threshold_label <- paste0("S≤", threshold)
    }

    # Calculate proportions by treatment
    props <- aggregate(
      cbind(affected = rowSums(data[, selected_cols, drop = FALSE]),
            total = data[[total_col]]) ~ data[[treatment_col]],
      FUN = sum
    )
    names(props)[1] <- "Treatment"
    props$proportion <- props$affected / props$total

    # Add to the data frame
    prop_by_treatment[[threshold_label]] <-
      props$proportion[match(prop_by_treatment$Treatment, props$Treatment)]
  }

  # Return both results and proportions
  return(structure(list(
    results = results_df,
    proportions = prop_by_treatment,
    detailed_results = results_list,
    alternative = alternative
  ),class="RSCABS"))
}


#' Printing method for run_all_threshold_tests results
#'
#' @param x an object from run_all_threshold_tests with class RSCABS
#'
#' @return printed results from run_all_threshold_tests
#' @export
#'
print.RSCABS <- function(x){
  cat("\nresult table: \n\n")
  print(x$results)
  cat("\n Summarized Proportions: \n")
  print(x$proportions)
  cat("\n Alternative Hypothesis: \n")
  print(x$alternative)
  invisible(x$detailed_results)
}





#' Perform Step-Down Rao-Scott Adjusted Cochran-Armitage Trend Test Procedure
#'
#' This function performs a step-down procedure using Rao-Scott adjusted Cochran-Armitage
#' trend tests (RSCABS). The procedure systematically excludes the highest treatment groups
#' to identify at which treatment level the dose-response relationship becomes significant.
#'
#' @param data A data frame containing fish injury data
#' @param treatment_col Name of the column containing treatment groups (default: "tmt")
#' @param treatment_order Optional vector specifying the order of treatment groups from
#'        lowest to highest dose. If NULL, alphabetical order is used (default: NULL)
#' @param max_score Maximum score value to consider (default: NULL, auto-detected)
#' @param min_score Minimum score value to consider (default: 1)
#' @param score_cols Character vector of column names containing injury scores (default: NULL, auto-detected)
#' @param replicate_col Name of the column containing tank/replicate IDs (default: "tank")
#' @param total_col Name of the column containing total counts (default: "total")
#' @param direction Character string indicating threshold direction: "greater" for ≥ threshold,
#'        "less" for ≤ threshold (default: "greater")
#' @param alternative Character string specifying the alternative hypothesis:
#'        "two.sided", "greater" (proportion increases with treatment level),
#'        or "less" (proportion decreases with treatment level) (default: "greater")
#' @param include_fisher Logical indicating whether to use Fisher's exact test when RSCA fails (default: TRUE)
#'
#' @return A list of class "StepDownRSCABS" containing:
#'   \item{combined_results}{Data frame with test results for all steps and thresholds}
#'   \item{step_results}{List of RSCABS objects for each step}
#'   \item{summary}{Summary of significant findings by step}
#'   \item{lowest_significant}{Information about the lowest significant treatment level}
#'   \item{parameters}{List of parameters used for the analysis}
#'
#' @examples
#' # Example data
#' fish_data <- data.frame(
#'   tmt = c(rep("Control", 8), rep("Low", 4), rep("Medium", 4), rep("High", 4)),
#'   tank = c(paste0("C", 1:8), paste0("L", 1:4), paste0("M", 1:4), paste0("H", 1:4)),
#'   S0 = c(3, 2, 3, 2, 2, 1, 2, 3, 3, 3, 4, 2, 2, 3, 2, 2, 2, 3, 1, 2),
#'   S1 = c(1, 2, 0, 1, 2, 2, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 2, 0),
#'   S2 = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1),
#'   S3 = c(0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1),
#'   total = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
#' )
#'
#' # Run step-down procedure with default parameters
#' result <- step_down_RSCABS(fish_data, treatment_col = "tmt",
#'                           treatment_order = c("Control", "Low", "Medium", "High"))
#'
#' # Print results
#' print(result)
#'
#' # Plot results
#' plot(result)
#'
#' @importFrom dplyr filter mutate select bind_rows arrange
#' @importFrom stats aggregate
#' @export
step_down_RSCABS <- function(data,
                             treatment_col = "tmt",
                             treatment_order = NULL,
                             max_score = NULL,
                             min_score = 1,
                             score_cols = NULL,
                             replicate_col = "tank",
                             total_col = "total",
                             direction = "greater",
                             alternative = "greater",
                             include_fisher = TRUE) {

  # Input validation
  if (!treatment_col %in% names(data)) {
    stop("Treatment column '", treatment_col, "' not found in data")
  }

  # Get treatment levels
  if (is.null(treatment_order)) {
    treatments <- sort(unique(data[[treatment_col]]))
  } else {
    # Validate treatment_order
    if (!all(unique(data[[treatment_col]]) %in% treatment_order)) {
      missing <- setdiff(unique(data[[treatment_col]]), treatment_order)
      stop("Some treatments in data are not in treatment_order: ",
           paste(missing, collapse = ", "))
    }
    if (!all(treatment_order %in% unique(data[[treatment_col]]))) {
      extra <- setdiff(treatment_order, unique(data[[treatment_col]]))
      stop("Some treatments in treatment_order are not in data: ",
           paste(extra, collapse = ", "))
    }
    treatments <- treatment_order
  }

  # Ensure treatment is a factor with the correct order
  data[[treatment_col]] <- factor(data[[treatment_col]], levels = treatments)

  # Number of treatment levels
  n_treatments <- length(treatments)

  # Check if there are at least 2 treatment levels
  if (n_treatments < 2) {
    stop("At least 2 treatment levels are required for step-down procedure")
  }

  # Initialize list to store results
  step_results <- list()
  combined_results <- data.frame()

  # Perform tests for each step
  for (i in 1:(n_treatments-1)) {
    # Select treatments for this step
    included_treatments <- treatments[1:(n_treatments-i+1)]
    ## need to droplevels so that the step step does not calculate for non-existing treatment levels.
    step_data <- droplevels(data[data[[treatment_col]] %in% included_treatments, ])

    # Run tests
    results <- run_all_threshold_tests(
      data = step_data,
      max_score = max_score,
      min_score = min_score,
      score_cols = score_cols,
      treatment_col = treatment_col,
      replicate_col = replicate_col,
      total_col = total_col,
      direction = direction,
      alternative = alternative,
      include_fisher = include_fisher
    )

    # Add step information to results dataframe
    results$results$Step <- i
    results$results$Included_Treatments <- paste(included_treatments, collapse = ", ")
    results$results$Highest_Treatment <- included_treatments[length(included_treatments)]

    # Store results
    step_results[[i]] <- results
    combined_results <- bind_rows(combined_results, results$results)
  }

  # Create summary of significant findings by step
  summary <- list()
  lowest_significant <- list(
    step = 0,
    treatment = NA,
    threshold = NA,
    p_value = NA
  )
  ##browser()
  for (i in 1:(n_treatments-1)) {
    step_data <- combined_results[combined_results$Step == i, ]
    sig_thresholds <- step_data[step_data$P_value < 0.05, "Threshold"]

    summary[[i]] <- list(
      step = i,
      included_treatments = unique(step_data$Included_Treatments),
      highest_treatment = unique(step_data$Highest_Treatment),
      significant_thresholds = sig_thresholds,
      has_significant_findings = length(sig_thresholds) > 0
    )

    # Update lowest significant treatment if this step has significant findings
    if (length(sig_thresholds) > 0 && i > lowest_significant$step) {
      lowest_sig_row <- step_data[step_data$P_value < 0.05, ][
        which.min(step_data[step_data$P_value < 0.05, "P_value"]), ]

      lowest_significant$step <- i
      lowest_significant$treatment <- unique(step_data$Highest_Treatment)
      lowest_significant$threshold <- lowest_sig_row$Threshold
      lowest_significant$p_value <- lowest_sig_row$P_value
    }
  }

  # If no significant findings were found, update lowest_significant
  if (lowest_significant$step==0) {
    lowest_significant$step <- NA
  }

  # Return results
  result <- list(
    combined_results = combined_results,
    step_results = step_results,
    summary = summary,
    lowest_significant = lowest_significant,
    parameters = list(
      treatment_col = treatment_col,
      treatment_order = treatments,
      max_score = max_score,
      min_score = min_score,
      score_cols = score_cols,
      replicate_col = replicate_col,
      total_col = total_col,
      direction = direction,
      alternative = alternative,
      include_fisher = include_fisher
    )
  )

  class(result) <- "StepDownRSCABS"
  return(result)
}

#' Print method for StepDownRSCABS objects
#'
#' @param x A StepDownRSCABS object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the object
#' @export
print.StepDownRSCABS <- function(x, printLowest=FALSE,...) {
  cat("Step-Down RSCABS Analysis\n")
  cat("========================\n\n")

  cat("Parameters:\n")
  cat("  Direction:", x$parameters$direction, "\n")
  cat("  Alternative hypothesis:", x$parameters$alternative, "\n")
  cat("  Treatment levels:", paste(x$parameters$treatment_order, collapse = ", "), "\n\n")

  cat("Summary of findings:\n")
  for (i in seq_along(x$summary)) {
    cat("Step", i, ": ")
    cat("Included treatments:", x$summary[[i]]$included_treatments, "\n")

    if (x$summary[[i]]$has_significant_findings) {
      cat("  Significant thresholds:",
          paste(x$summary[[i]]$significant_thresholds, collapse = ", "), "\n")
    } else {
      cat("  No significant findings\n")
    }
  }
  if(printLowest){
    cat("\nLowest treatment level with significant findings:\n")
    if (!is.na(x$lowest_significant$step)) {
      cat("  Treatment:", x$lowest_significant$treatment, "\n")
      cat("  Threshold:", x$lowest_significant$threshold, "\n")
      cat("  P-value:", round(x$lowest_significant$p_value, 4), "\n")
    } else {
      cat("  No significant findings at any treatment level\n")
    }

  }

  invisible(x)
}

#' Plot method for StepDownRSCABS objects
#'
#' @param x A StepDownRSCABS object
#' @param ... Additional arguments (not used)
#'
#' @return A ggplot object
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient scale_y_reverse labs theme_minimal theme element_text element_blank
#' @export
plot.StepDownRSCABS <- function(x, ...) {
  # Prepare data for plotting
  plot_data <- x$combined_results

  # Create step labels
  step_labels <- sapply(seq_along(x$summary), function(i) {
    paste("Step", i, ":", x$summary[[i]]$highest_treatment)
  })

  # Create the plot
  p <- ggplot(plot_data, aes(x = Threshold, y = Step, fill = -log10(P_value))) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.4f", round(P_value, 4))), color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "steelblue",
                        name = "-log10(p-value)") +
    scale_y_reverse(breaks = seq_along(step_labels),
                    labels = step_labels) +
    labs(
      title = "Step-Down RSCABS Results",
      subtitle = "P-values for each threshold at each step",
      x = "Injury Threshold",
      y = "Step"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )

  return(p)
}

#' Summary method for StepDownRSCABS objects
#'
#' @param object A StepDownRSCABS object
#' @param ... Additional arguments (not used)
#'
#' @return A list with summary information
#' @export
summary.StepDownRSCABS <- function(object, ...) {
  # Create a summary data frame of results
  summary_df <- data.frame(
    Step = integer(),
    Included_Treatments = character(),
    Highest_Treatment = character(),
    Significant_Thresholds = character(),
    Min_P_Value = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(object$summary)) {
    step_summary <- object$summary[[i]]

    if (step_summary$has_significant_findings) {
      sig_thresholds <- paste(step_summary$significant_thresholds, collapse = ", ")
      min_p <- min(object$combined_results$P_value[
        object$combined_results$Step == i &
          object$combined_results$Threshold %in% step_summary$significant_thresholds
      ])
    } else {
      sig_thresholds <- "None"
      min_p <- NA
    }

    summary_df <- rbind(summary_df, data.frame(
      Step = i,
      Included_Treatments = step_summary$included_treatments,
      Highest_Treatment = step_summary$highest_treatment,
      Significant_Thresholds = sig_thresholds,
      Min_P_Value = min_p,
      stringsAsFactors = FALSE
    ))
  }

  # Return summary information
  list(
    summary_table = summary_df,
    lowest_significant = object$lowest_significant,
    parameters = object$parameters
  )
}
