#' Create Contingency Table from Count Data
#'
#' This function creates a contingency table by aggregating counts based on a grouping variable.
#'
#' @param data A data frame containing the data
#' @param group_col String name of the column to group by (e.g., "dose")
#' @param success_col String name of the column containing success counts (e.g., "alive")
#' @param failure_col String name of the column containing failure counts (e.g., "dead")
#' @param prefix String prefix to add to row names (default: the group column name followed by "_")
#' @param col_names Character vector of length 2 for column names (default: c("success", "failure"))
#'
#' @return A matrix where rows represent groups and columns represent success/failure counts
#' @importFrom dplyr group_by summarize ungroup
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#' data <- data.frame(
#'   dose = rep(c(0, 1, 10), each = 3),
#'   alive = c(9, 8, 10, 7, 6, 5, 3, 4, 2),
#'   dead = c(1, 2, 0, 3, 4, 5, 7, 6, 8)
#' )
#'
#' # Create contingency table
#' create_contingency_table(data, "dose", "alive", "dead")
#'
create_contingency_table <- function(data, group_col, success_col, failure_col,
                                     prefix = NULL, col_names = NULL) {

  # Validate inputs
  if (!all(c(group_col, success_col, failure_col) %in% colnames(data))) {
    stop("One or more specified columns not found in the data")
  }

  # Set default prefix if not specified
  if (is.null(prefix)) {
    prefix <- paste0(group_col, "_")
  }

  # Set default column names if not specified
  if (is.null(col_names)) {
    col_names <- c("success", "failure")
  } else if (length(col_names) != 2) {
    stop("col_names must be a character vector of length 2")
  }

  # Convert column names to symbols for proper use in dplyr functions
  group_sym <- sym(group_col)
  success_sym <- sym(success_col)
  failure_sym <- sym(failure_col)

  # Aggregate counts by group using correct syntax for dynamic column names
  summary_data <- data %>%
    group_by(!!group_sym) %>%
    summarize(
      success_count = sum(!!success_sym),
      failure_count = sum(!!failure_sym),
      .groups = "drop"
    )

  # Create matrix
  result_matrix <- as.matrix(summary_data[, c("success_count", "failure_count")])

  # Set row and column names
  rownames(result_matrix) <- paste0(prefix, summary_data[[group_col]])
  colnames(result_matrix) <- col_names

  return(result_matrix)
}



#' Many-to-One Pairwise Fisher's Exact Test
#'
#' Performs Fisher's exact test comparing each group to a reference group, modified based on rstatix pairwise implementation
#'
#' @param xtab A contingency table as a matrix or data frame
#' @param ref.group Character string specifying the reference group (must match one of the row names in xtab)
#' @param p.adjust.method Method for adjusting p-values for multiple comparisons
#' @param detailed Logical indicating whether to return detailed results
#' @param ... Additional arguments passed to fisher.test()
#'
#' @return A data frame containing the results of the Fisher's exact tests
#' @importFrom dplyr %>% mutate select add_row
#' @importFrom purrr map
#' @importFrom rstatix adjust_pvalue add_significance fisher_test
#' @export
#'
#' @examples \dontrun{
#' # Create contingency table
#' ctable <- create_contingency_table(data)
#' # Run many-to-one Fisher's test with dose_0 as reference
#' many_to_one_fisher_test(ctable, ref.group = "dose_0")
#' }
many_to_one_fisher_test <- function(xtab, ref.group = NULL, p.adjust.method = "holm",
                                    detailed = FALSE, ...) {
  ## requireNamespace("rstatix",quietly = FALSE)
  if (is.data.frame(xtab))
    xtab <- as.matrix(xtab)

  if (ncol(xtab) > 2 & nrow(xtab) == 2)
    xtab <- t(xtab)

  if (is.null(colnames(xtab)) | any(0 %in% nchar(colnames(xtab)))) {
    colnames(xtab) <- paste0("col", 1:ncol(xtab))
  }

  if (is.null(rownames(xtab)) | any(0 %in% nchar(rownames(xtab)))) {
    rownames(xtab) <- paste0("row", 1:nrow(xtab))
  }

  if (ncol(xtab) > 2) {
    stop("A two-dimensional contingency table required.")
  }

  # Check if reference group exists in the data
  if (is.null(ref.group)) {
    stop("Reference group must be specified")
  }

  if (!ref.group %in% rownames(xtab)) {
    stop(paste("Reference group", ref.group, "not found in the data"))
  }

  # Function to compare a group with the reference group
  compare_to_ref <- function(group, ref, xtab, ...) {
    rows <- c(ref, group)
    fisher_test(xtab[rows, ], detailed = detailed, ...) %>%
      rstatix:::add_columns(group1 = ref, group2 = group, .before = 1) %>%
      rstatix:::keep_only_tbl_df_classes()
  }

  # Get all groups except reference
  other_groups <- setdiff(rownames(xtab), ref.group)

  # Perform comparisons
  results <- lapply(other_groups, compare_to_ref, ref = ref.group, xtab = xtab, ...) %>%
    bind_rows() %>%
    adjust_pvalue("p", method = p.adjust.method) %>%
    add_significance("p.adj") %>%
    mutate(p.adj = signif(.data$p.adj, digits = 3)) %>%
    dplyr::select(-.data$p.signif)

  # Add class and attributes
  args <- c(as.list(environment()), list(...)) %>%
    rstatix:::add_item(method = "fisher_test", ref.group = ref.group)

  results %>%
    rstatix:::set_attrs(args = args) %>%
    rstatix:::add_class(c("rstatix_test", "fisher_test", "many_to_one_test"))
}


#' Perform Fisher's Exact Test Comparing Each Level to Control
#'
#' This function performs Fisher's exact test comparing each level of a factor
#' to a control level, using counts of success and failure.
#'
#' @param data A data frame containing the data
#' @param factor_col String name of the column containing the factor (e.g., "dose")
#' @param success_col String name of the column containing success counts (e.g., "alive")
#' @param failure_col String name of the column containing failure counts (e.g., "dead")
#' @param control_level The level to use as control (default: first level in the data)
#' @param alternative Direction of the alternative hypothesis ("two.sided", "less", or "greater")
#' @param conf.level Confidence level for the returned confidence interval
#'
#' @return A data frame with factor levels and corresponding p-values
#' @export
#' @importFrom stats p.adjust
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#' doses <- c(0, 0.1, 1, 10, 50, 100)
#' reps <- 4
#' data <- data.frame(
#'   dose = rep(doses, each = reps),
#'   replicate = rep(1:reps, times = length(doses))
#' )
#' data$alive <- sapply(data$dose, function(x) rbinom(1, 10, prob = exp(-0.02 * x)))
#' data$dead <- 10 - data$alive
#'
#' # Run Fisher's exact test
#' compare_to_control_fisher(data, "dose", "alive", "dead", control_level = 0)
#'
compare_to_control_fisher <- function(data, factor_col, success_col, failure_col,
                                      control_level = NULL,
                                      alternative = "two.sided",
                                      conf.level = 0.95) {

  # Validate inputs
  if (!all(c(factor_col, success_col, failure_col) %in% colnames(data))) {
    stop("One or more specified columns not found in the data")
  }

  # Extract the column data using the provided column names
  factor_data <- data[[factor_col]]
  success_data <- data[[success_col]]
  failure_data <- data[[failure_col]]

  # Set default control level if not specified
  if (is.null(control_level)) {
    control_level <- sort(unique(factor_data))[1]
  }

  if (!control_level %in% factor_data) {
    stop(paste("Control level", control_level, "not found in the data"))
  }

  # Get unique levels excluding control
  test_levels <- unique(factor_data)
  test_levels <- test_levels[test_levels != control_level]

  # Initialize results data frame
  fisher_results <- data.frame(
    level = test_levels,
    p_value = NA_real_,
    odds_ratio = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_
  )

  # Calculate control group totals
  control_success <- sum(success_data[factor_data == control_level])
  control_failure <- sum(failure_data[factor_data == control_level])

  # Perform Fisher's exact test for each level
  for (i in seq_along(test_levels)) {
    level <- test_levels[i]

    # Calculate test group totals
    test_success <- sum(success_data[factor_data == level])
    test_failure <- sum(failure_data[factor_data == level])

    # Create 2x2 contingency table
    cont_table <- matrix(
      c(control_success, control_failure, test_success, test_failure),
      nrow = 2,
      byrow = TRUE
    )

    # Perform Fisher's exact test
    test_result <- fisher.test(
      cont_table,
      alternative = alternative,
      conf.level = conf.level
    )

    # Store results
    fisher_results$p_value[i] <- test_result$p.value
    fisher_results$odds_ratio[i] <- test_result$estimate
    fisher_results$ci_lower[i] <- test_result$conf.int[1]
    fisher_results$ci_upper[i] <- test_result$conf.int[2]
  }

  # Add adjusted p-values
  fisher_results$p_adjusted <- p.adjust(fisher_results$p_value, method = "holm")

  # Rename the level column to match the input factor column name
  names(fisher_results)[names(fisher_results) == "level"] <- factor_col

  # Round numeric columns for readability
  fisher_results <- fisher_results %>%
    mutate(across(where(is.numeric), ~round(., 4)))

  return(fisher_results)
}


#' Tarone's Z Test
#'
#' Tests the goodness of fit of the binomial distribution.
#'
#' @param M Counts
#' @param N Trials
#'
#' @return a \code{htest} object
#'
#' @author \href{https://stats.stackexchange.com/users/173082/reinstate-monica}{Ben O'Neill}
#' @references \url{https://stats.stackexchange.com/a/410376/6378} and
#' R. E. TARONE, Testing the goodness of fit of the binomial distribution, Biometrika, Volume 66, Issue 3, December 1979, Pages 585–590, \url{https://doi.org/10.1093/biomet/66.3.585}
#' @importFrom stats pnorm
#' @export
#' @examples
#'  #Generate example data
#' N <- c(30, 32, 40, 28, 29, 35, 30, 34, 31, 39)
#' M <- c( 9, 10, 22, 15,  8, 19, 16, 19, 15, 10)
#' Tarone.test(N, M)
Tarone.test <- function(N, M) {

  #Check validity of inputs
  if(!(all(N == as.integer(N)))) { stop("Error: Number of trials should be integers"); }
  if(min(N) < 1) { stop("Error: Number of trials should be positive"); }
  if(!(all(M == as.integer(M)))) { stop("Error: Count values should be integers"); }
  if(min(M) < 0) { stop("Error: Count values cannot be negative"); }
  if(any(M > N)) { stop("Error: Observed count value exceeds number of trials"); }

  #Set description of test and data
  method      <- "Tarone's Z test";
  data.name   <- paste0(deparse(substitute(M)), " successes from ",
                        deparse(substitute(N)), " trials");

  #Set null and alternative hypotheses
  null.value  <- 0;
  attr(null.value, "names") <- "dispersion parameter";
  alternative <- "greater";

  #Calculate test statistics
  estimate    <- sum(M)/sum(N);
  attr(estimate, "names") <- "proportion parameter";
  S           <- ifelse(estimate == 1, sum(N),
                        sum((M - N*estimate)^2/(estimate*(1 - estimate))));
  statistic   <- (S - sum(N))/sqrt(2*sum(N*(N-1)));
  attr(statistic, "names") <- "z";

  #Calculate p-value
  p.value     <- 2*pnorm(-abs(statistic), 0, 1);
  attr(p.value, "names") <- NULL;

  #Create htest object
  TEST        <- list(method = method, data.name = data.name,
                      null.value = null.value, alternative = alternative,
                      estimate = estimate, statistic = statistic, p.value = p.value);
  class(TEST) <- "htest";
  TEST;
}






