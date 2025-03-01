## unit testing: test_ordinal.R #######


#' Get the EC50 Estimate from a Model
#'
#' This function calculates the EC50 (the dose at which 50% of the maximum effect is observed)
#' from a fitted model. It supports generalized linear models (GLMs) and generalized linear mixed models (GLMMs).
#'
#' @param mod A fitted model object. This can be of class "glm" or "glmmPQL", or other models compatible with the `ED` function.
#'
#' @return A data frame containing the following columns:
#' \item{EC50}{The estimated EC50 value.}
#' \item{lower}{The lower bound of the 95% confidence interval for the EC50.}
#' \item{upper}{The upper bound of the 95% confidence interval for the EC50.}
#' \item{se}{The standard error of the EC50 estimate.}
#'
#' @examples
#' # Assuming `fit` is a fitted glm model
#' fit <- glm(yt ~ log(dose),family= quasibinomial(link="logit"),data=pvi_example)
#' ec50_result <- getEC50(fit)
#' print(ec50_result)
#'
#' @export
getEC50 <- function(mod,approximate = FALSE){
  if (!inherits(mod, c("glm", "glmmPQL","drc"))) {
    stop("The model must be of class 'glm' or 'glmmPQL' of 'drc'.")
  }
  if("glm" %in% class(mod)){
    ec50 <- MASS::dose.p(mod,p=0.5)
    se <- attr(ec50,"SE")
    xmin <- as.numeric(ec50 - 1.96*se)
    xmax <- as.numeric(ec50 + 1.96*se)
    SE <- backCalcSE(se = as.numeric(se), mu = as.numeric(ec50), approximate = approximate)
    res <- data.frame(EC50=exp(as.numeric(ec50)),
                      lower=exp(xmin),upper=exp(xmax),se=SE)
  }else{
    if("glmmPQL" %in% class(mod)){
      res1 <- dose.p.glmmPQL(mod,cf=1:2,p=0.5)
      se <- attr(res1,"SE")
      SE <- backCalcSE(se = as.numeric(se), mu = as.numeric(res1[1]),
                       approximate = approximate)

      res <- data.frame(EC50=exp(res1[1]),lower=exp(res1[1]-1.96*se),
                        upper=exp(res1[1]+1.96*se),se=SE)
    }else{
      res1 <- ED(mod,50,interval="delta")
      res <- data.frame(EC50=res1[1,"Estimate"],lower=res1[1,"Lower"],upper=res1[1,"Upper"],se=res1[1,"Std. Error"])

    }
  }
  return(res)
}

#' Calculate Dose Probability for GLMM using PQL
#'
#' This function calculates the dose probability for a generalized linear mixed model (GLMM)
#' using Penalized Quasi-Likelihood (PQL).
#'
#' @param obj A fitted GLMM object of class "glmmPQL".
#' @param cf A numeric vector specifying the coefficients to be used for the calculation (default is 1:2).
#' @param p A numeric value representing the probability level (default is 0.5).
#'
#' @return A structured object of class "glm.dose" containing the estimated dose probability and its standard error.
#'
#' @examples
#' # Assuming `glmm_model` is a fitted glmmPQL model
#' library(MASS)
#' glmm_model <- glmmPQL(yt ~ dose,random= ~1 | Obs,family= quasibinomial(link="logit"),data=pvi_example)
#' dose_result <- dose.p.glmmPQL(glmm_model)
#' print(dose_result)
#'
#' @export
dose.p.glmmPQL <- function(obj,cf=1:2,p=0.5){
  eta <- obj$family$linkfun(p)
  b <- obj$coefficients$fixed[cf]
  x.p <- (eta - b[1L])/b[2L]
  names(x.p) <- paste("p = ", format(p), ":", sep = "")
  pd <- -cbind(1, x.p)/b[2L]
  SE <- sqrt(((pd %*% vcov(obj)[cf, cf]) * pd) %*% c(1, 1))
  res <- structure(x.p, SE = SE, p = p)
  class(res) <- "glm.dose"
  res
}

#' Back Calculate Standard Error from Lognormal Parameters
#'
#' This function calculates the standard error (SE) of a lognormally distributed variable
#' based on the mean (mu) and the standard error of the logarithm (se).
#'
#' @param se The standard error of the logarithm.
#' @param mu The mean of the logarithm.
#' @param approximate Logical. If TRUE, uses an approximation; otherwise, calculates based on the variance of the lognormal distribution.
#'
#' @return The back-calculated standard error.
#' @export
#'
#' @examples
#' backCalcSE(se = 0.1, mu = 0)
#' backCalcSE(se = 0.1, mu = 0, approximate = TRUE)
backCalcSE <- function(se,mu,approximate = FALSE){
  if (!is.numeric(se) || !is.numeric(mu)) {
    stop("Both 'se' and 'mu' must be numeric values.")
  }
 if(approximate){
   SE <- exp(mu)*se
 }else{
   SE <- sqrt((exp(se^2)-1)*exp(2*mu+se^2)) ## Variance of the lognormal distribution
 }
  return(SE)
}






#' Expand Aggregated Fish Data to Individual Records (tidyverse version)
#'
#' @param data A data frame containing aggregated fish data
#' @param treatment_col Name of the column containing treatment groups (default: "tmt")
#' @param replicate_col Name of the column containing tank/replicate IDs (default: "tank")
#' @param score_prefix Prefix used for score columns (default: "S")
#' @param total_col Name of the column containing total counts (default: "total")
#'
#' @return A data frame with individual fish records
#' @export
expand_to_individual_tidy <- function(data, treatment_col = "tmt", replicate_col = "tank",
                                      score_prefix = "S", total_col = "total") {
  # Input validation
  if (!treatment_col %in% names(data)) {
    stop("Treatment column '", treatment_col, "' not found in data")
  }
  if (!replicate_col %in% names(data)) {
    stop("Replicate column '", replicate_col, "' not found in data")
  }
  if (!total_col %in% names(data)) {
    stop("Total column '", total_col, "' not found in data")
  }

  # Find score columns
  score_cols <- grep(paste0("^", score_prefix, "[0-9]+$"), names(data), value = TRUE)
  if (length(score_cols) == 0) {
    stop("No score columns found with prefix '", score_prefix, "' followed by numbers")
  }

  # Check if totals match sum of scores
  data$check_sum <- rowSums(data[, score_cols, drop = FALSE])
  if (any(data$check_sum != data[[total_col]])) {
    warning("Some rows have score counts that don't sum to the total")
  }

  # Convert to long format and expand
  result <- data %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(score_cols),
      names_to = "score",
      values_to = "count_weight"  # Use a specific name for the count column
    ) %>%
    dplyr::filter(count_weight > 0) %>%
    dplyr::select(tidyselect::all_of(c(treatment_col, replicate_col, "score", "count_weight"))) %>%
    tidyr::uncount(weights = count_weight) %>%  # Use the specific column name
    dplyr::arrange(.data[[treatment_col]], .data[[replicate_col]])

  return(result)
}

#' Aggregate Individual Fish Records to Summarized Format (tidyverse version)
#'
#' @param data A data frame containing individual fish records
#' @param treatment_col Name of the column containing treatment groups (default: "tmt")
#' @param replicate_col Name of the column containing tank/replicate IDs (default: "tank")
#' @param score_col Name of the column containing scores (default: "score")
#' @param total_col Name of the column to be created for total counts (default: "total")
#'
#' @return A data frame with aggregated counts per replicate
#' @export
aggregate_from_individual_tidy <- function(data, treatment_col = "tmt", replicate_col = "tank",
                                           score_col = "score", total_col = "total") {
  # Input validation
  required_cols <- c(treatment_col, replicate_col, score_col)
  if (!all(required_cols %in% names(data))) {
    missing <- setdiff(required_cols, names(data))
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Aggregate the data
  result <- data %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(treatment_col, replicate_col, score_col)))) %>%
    dplyr::summarize(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = tidyselect::all_of(score_col),
      values_from = n,
      values_fill = 0
    ) %>%
    dplyr::mutate(!!total_col := rowSums(dplyr::across(where(is.numeric)))) %>%
    dplyr::arrange(.data[[treatment_col]], .data[[replicate_col]])

  return(result)
}


#' Aggregate Individual Fish Records to Summarized Format (simple without tidyverse version)
#'
#' @param data A data frame containing individual fish records
#' @param treatment_col Name of the column containing treatment groups (default: "tmt")
#' @param replicate_col Name of the column containing tank/replicate IDs (default: "tank")
#' @param score_col Name of the column containing scores (default: "score")
#' @param total_col Name of the column to be created for total counts (default: "total")
#' @param all_scores Optional vector of all score categories to include in the result,
#'                  even if they have zero occurrences (default: NULL, which uses unique values in data)
#'
#' @return A data frame with aggregated counts per replicate
#' @export
aggregate_from_individual_simple <- function(data, treatment_col = "tmt", replicate_col = "tank",
                                             score_col = "score", total_col = "total",
                                             all_scores = NULL) {
  # Input validation
  required_cols <- c(treatment_col, replicate_col, score_col)
  if (!all(required_cols %in% names(data))) {
    missing <- setdiff(required_cols, names(data))
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Get unique combinations of treatment and replicate
  unique_groups <- unique(data[, c(treatment_col, replicate_col)])

  # Get unique score values
  if (is.null(all_scores)) {
    unique_scores <- sort(unique(data[[score_col]]))
  } else {
    unique_scores <- sort(all_scores)
    # Check if data contains scores not in all_scores
    data_scores <- unique(data[[score_col]])
    missing_scores <- setdiff(data_scores, all_scores)
    if (length(missing_scores) > 0) {
      warning("Data contains scores not in all_scores: ",
              paste(missing_scores, collapse = ", "))
    }
  }

  # Initialize result data frame
  result <- unique_groups

  # Add columns for each score
  for (score in unique_scores) {
    result[[score]] <- 0
  }

  # Add total column
  result[[total_col]] <- 0

  # Fill in counts
  for (i in 1:nrow(result)) {
    treatment_val <- result[[treatment_col]][i]
    replicate_val <- result[[replicate_col]][i]

    # Filter data for this treatment and replicate
    subset_data <- data[data[[treatment_col]] == treatment_val &
                          data[[replicate_col]] == replicate_val, ]

    # Count occurrences of each score
    for (score in unique_scores) {
      count <- sum(subset_data[[score_col]] == score)
      result[i, score] <- count
    }

    # Calculate total
    result[[total_col]][i] <- nrow(subset_data)
  }

  # Sort the result
  result <- result[order(result[[treatment_col]], result[[replicate_col]]), ]
  rownames(result) <- NULL

  return(result)
}

#' Expand Aggregated Fish Data to Individual Records (simple without tidyverse version)
#'
#' @param data A data frame containing aggregated fish data
#' @param treatment_col Name of the column containing treatment groups (default: "tmt")
#' @param replicate_col Name of the column containing tank/replicate IDs (default: "tank")
#' @param score_prefix Prefix used for score columns (default: "S")
#' @param total_col Name of the column containing total counts (default: "total")
#'
#' @return A list containing:
#'   - data: A data frame with individual fish records
#'   - score_columns: Vector of all score column names from the original data
#'
#' @export
expand_to_individual_simple <- function(data, treatment_col = "tmt", replicate_col = "tank",
                                        score_prefix = "S", total_col = "total") {
  # Input validation
  if (!treatment_col %in% names(data)) {
    stop("Treatment column '", treatment_col, "' not found in data")
  }
  if (!replicate_col %in% names(data)) {
    stop("Replicate column '", replicate_col, "' not found in data")
  }
  if (!total_col %in% names(data)) {
    stop("Total column '", total_col, "' not found in data")
  }

  # Find score columns
  score_cols <- grep(paste0("^", score_prefix, "[0-9]+$"), names(data), value = TRUE)
  if (length(score_cols) == 0) {
    stop("No score columns found with prefix '", score_prefix, "' followed by numbers")
  }

  # Check if totals match sum of scores
  data$check_sum <- rowSums(data[, score_cols, drop = FALSE])
  if (any(data$check_sum != data[[total_col]])) {
    warning("Some rows have score counts that don't sum to the total")
  }

  # Initialize empty data frame for individual records
  result <- data.frame()

  # Process each row of the input data
  for (i in 1:nrow(data)) {
    row_data <- data[i, ]

    for (score in score_cols) {
      count <- row_data[[score]]
      if (count > 0) {
        # Create a data frame for this score and count
        new_rows <- data.frame(
          row_data[rep(1, count), c(treatment_col, replicate_col)],
          score = rep(score, count),
          stringsAsFactors = FALSE
        )
        result <- rbind(result, new_rows)
      }
    }
  }

  # Sort the result
  result <- result[order(result[[treatment_col]], result[[replicate_col]]), ]
  rownames(result) <- NULL

  # Return both the individual data and the original score columns
  return(list(
    data = result,
    score_columns = score_cols
  ))
}

#' Convert Between Aggregated and Individual Fish Data
#'
#' This function provides a complete workflow for converting between aggregated
#' fish data and individual fish records, ensuring all score categories are preserved.
#'
#' @param data A data frame containing either aggregated or individual fish data
#' @param direction Either "to_individual" or "to_aggregated"
#' @param treatment_col Name of the column containing treatment groups (default: "tmt")
#' @param replicate_col Name of the column containing tank/replicate IDs (default: "tank")
#' @param score_prefix Prefix used for score columns when direction is "to_individual" (default: "S")
#' @param score_col Name of the column containing scores when direction is "to_aggregated" (default: "score")
#' @param total_col Name of the column containing/for total counts (default: "total")
#' @param all_scores Optional vector of all score categories to include when direction is "to_aggregated"
#'
#' @return A data frame with the converted data
#'
#' @export
convert_fish_data <- function(data, direction,
                              treatment_col = "tmt", replicate_col = "tank",
                              score_prefix = "S", score_col = "score",
                              total_col = "total", all_scores = NULL) {

  if (direction == "to_individual") {
    # Convert from aggregated to individual
    result <- expand_to_individual_simple(
      data, treatment_col, replicate_col, score_prefix, total_col
    )

    # Store score columns as an attribute for future reference
    attr(result$data, "score_columns") <- result$score_columns
    return(result$data)

  } else if (direction == "to_aggregated") {
    # Check for stored score columns
    if (is.null(all_scores) && !is.null(attr(data, "score_columns"))) {
      all_scores <- attr(data, "score_columns")
    }

    # Convert from individual to aggregated
    return(aggregate_from_individual_simple(
      data, treatment_col, replicate_col, score_col, total_col, all_scores
    ))

  } else {
    stop("Invalid direction. Use 'to_individual' or 'to_aggregated'.")
  }
}


