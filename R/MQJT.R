#' Multiquantal Jonckheere-Terpstra (MQJT) Test
#'
#' @description
#' Performs the Multiquantal Jonckheere-Terpstra (MQJT) test for ordinal data,
#' such as developmental stage in Amphibian Metamorphosis Assay (AMA) studies.
#'
#' The MQJT approach dichotomises the ordinal response at multiple cutpoints
#' (quantiles), computes replicate-level proportions exceeding each cutpoint,
#' and applies a step-down Jonckheere-Terpstra trend test at each slice.
#' The overall NOEC is determined as the most conservative (lowest) NOEC
#' across all slices.
#'
#' @references
#' Green JW, Springer TA, Holbech H (2018). Statistical Analysis of
#' Ecotoxicity Studies. Wiley. Chapter 9, Sections 9.3.1-9.3.4.
#'
#' @param data A data frame with individual-level observations.
#' @param stage_col Character. Column name for the ordinal stage variable
#'   (numeric or integer). For AMA studies, this is typically the NF
#'   developmental stage.
#' @param dose_col Character. Column name for dose/concentration group.
#'   Will be coerced to a factor if not already one. The first level is
#'   treated as the control.
#' @param replicate_col Character. Column name for replicate/tank identifier.
#' @param cutpoints Numeric vector of cutpoints at which to dichotomise.
#'   Each slice creates a binary indicator \eqn{I(\text{stage} \geq c)}.
#'   If \code{NULL} (default), cutpoints are derived from the unique stage
#'   values observed in the control group (excluding the minimum, since
#'   all animals would exceed it).
#' @param alternative Character. Direction of the alternative hypothesis
#'   for the JT test. One of \code{"greater"} (increasing trend, default)
#'   or \code{"less"} (decreasing trend). For AMA studies where treatment
#'   is expected to retard development, use \code{"less"}: the proportion
#'   of animals reaching higher stages decreases with dose.
#' @param alpha Numeric. Significance level (default 0.05).
#' @param use_replicate_means Logical. If \code{TRUE} (default), JT is
#'   run on replicate-level proportions (the standard MQJT approach).
#'   If \code{FALSE}, runs on individual binary indicators.
#'
#' @return An object of class \code{"mqjtTest"}, which is a list with:
#'   \item{slice_results}{Data frame with step-down results for each cutpoint.}
#'   \item{overall_NOEC}{The overall NOEC (most conservative across slices).}
#'   \item{overall_LOEC}{The corresponding LOEC.}
#'   \item{noec_per_slice}{Data frame of NOEC/LOEC per cutpoint.}
#'   \item{cutpoints}{The cutpoints used.}
#'   \item{detail}{List of step-down results per cutpoint.}
#'   \item{alternative}{The alternative hypothesis used.}
#'   \item{alpha}{The significance level used.}
#'
#' @examples
#' # Create example AMA-like data
#' set.seed(42)
#' ama_data <- data.frame(
#'   stage = c(
#'     sample(54:62, 20, replace = TRUE, prob = c(1,1,2,3,4,5,5,4,3)),
#'     sample(54:62, 20, replace = TRUE, prob = c(2,2,3,4,4,4,3,2,1)),
#'     sample(54:62, 20, replace = TRUE, prob = c(3,3,4,4,3,3,2,1,1))
#'   ),
#'   dose = rep(c("0", "0.1", "1.0"), each = 20),
#'   tank = rep(rep(c("A", "B"), each = 10), 3)
#' )
#' result <- mqjt_test(ama_data, "stage", "dose", "tank", alternative = "less")
#' result
#'
#' @export
mqjt_test <- function(
  data,
  stage_col,
  dose_col,
  replicate_col,
  cutpoints = NULL,
  alternative = "greater",
  alpha = 0.05,
  use_replicate_means = TRUE
) {
  # --- Input validation ---
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  required_cols <- c(stage_col, dose_col, replicate_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  alternative <- match.arg(alternative, c("greater", "less"))

  # Ensure stage is numeric
  stage <- data[[stage_col]]
  if (is.factor(stage)) {
    stage <- as.numeric(levels(stage))[stage]
  } else if (is.character(stage)) {
    stage <- as.numeric(stage)
  }
  if (any(is.na(stage) & !is.na(data[[stage_col]]))) {
    warning(
      "Some stage values could not be converted to numeric and became NA."
    )
  }
  data[[stage_col]] <- stage

  # Ensure dose is a properly ordered factor
  if (!is.factor(data[[dose_col]])) {
    # Try to sort numerically if possible, otherwise alphabetically
    unique_doses <- unique(data[[dose_col]])
    numeric_doses <- suppressWarnings(as.numeric(as.character(unique_doses)))
    if (!any(is.na(numeric_doses))) {
      data[[dose_col]] <- factor(
        data[[dose_col]],
        levels = unique_doses[order(numeric_doses)]
      )
    } else {
      data[[dose_col]] <- factor(data[[dose_col]])
    }
  }
  dose_levels <- levels(data[[dose_col]])

  if (length(dose_levels) < 2) {
    stop("Need at least 2 dose levels to run the MQJT test.")
  }

  # --- Determine cutpoints ---
  if (is.null(cutpoints)) {
    ctrl_stages <- data[[stage_col]][data[[dose_col]] == dose_levels[1]]
    ctrl_stages <- ctrl_stages[!is.na(ctrl_stages)]
    if (length(ctrl_stages) == 0) {
      stop("No valid stage values in the control group (first factor level).")
    }
    # Use unique observed stages in control, excluding the minimum
    cutpoints <- sort(unique(ctrl_stages))
    cutpoints <- cutpoints[cutpoints > min(ctrl_stages)]

    if (length(cutpoints) == 0) {
      warning(
        "All control animals have the same stage. Using that stage as the sole cutpoint."
      )
      cutpoints <- unique(ctrl_stages)
    }
  }

  cutpoints <- sort(unique(cutpoints))

  # --- Run step-down JT at each cutpoint ---
  slice_results_list <- list()
  detail_list <- list()

  for (cp in cutpoints) {
    # Dichotomise: I(stage >= cutpoint)
    dat_work <- data[!is.na(data[[stage_col]]), , drop = FALSE]
    dat_work$.binary <- as.numeric(dat_work[[stage_col]] >= cp)

    if (use_replicate_means) {
      # Aggregate to replicate-level proportions
      agg <- stats::aggregate(
        dat_work$.binary,
        by = list(
          dose = dat_work[[dose_col]],
          replicate = dat_work[[replicate_col]]
        ),
        FUN = mean
      )
      names(agg) <- c("dose", "replicate", "prop")

      # Check replicate counts
      rep_counts <- table(agg$dose)
      if (any(rep_counts < 2)) {
        warning(sprintf(
          "Cutpoint %s: fewer than 2 replicates in some dose groups. Skipping.",
          cp
        ))
        next
      }

      # Ensure factor ordering is preserved
      agg$dose <- factor(agg$dose, levels = dose_levels)

      sd_result <- step_down_jt(
        response = agg$prop,
        group = agg$dose,
        alternative = alternative,
        alpha = alpha
      )
    } else {
      dat_work[[dose_col]] <- factor(dat_work[[dose_col]], levels = dose_levels)
      sd_result <- step_down_jt(
        response = dat_work$.binary,
        group = dat_work[[dose_col]],
        alternative = alternative,
        alpha = alpha
      )
    }

    sd_result$cutpoint <- cp
    slice_results_list[[as.character(cp)]] <- sd_result$summary
    detail_list[[as.character(cp)]] <- sd_result
  }

  # --- Combine across slices ---
  if (length(slice_results_list) == 0) {
    warning("No valid slices could be tested.")
    output <- list(
      slice_results = data.frame(),
      overall_NOEC = NA_character_,
      overall_LOEC = NA_character_,
      noec_per_slice = data.frame(
        cutpoint = numeric(0),
        NOEC = character(0),
        LOEC = character(0),
        stringsAsFactors = FALSE
      ),
      cutpoints = cutpoints,
      detail = list(),
      alternative = alternative,
      alpha = alpha
    )
    class(output) <- "mqjtTest"
    return(output)
  }

  # Add cutpoint column to each slice summary
  for (nm in names(slice_results_list)) {
    slice_results_list[[nm]]$cutpoint <- as.numeric(nm)
  }
  slice_summary <- do.call(rbind, slice_results_list)
  rownames(slice_summary) <- NULL

  # NOEC per slice
  noec_per_slice <- vapply(detail_list, function(x) x$NOEC, character(1))
  loec_per_slice <- vapply(detail_list, function(x) x$LOEC, character(1))

  noec_df <- data.frame(
    cutpoint = as.numeric(names(noec_per_slice)),
    NOEC = unname(noec_per_slice),
    LOEC = unname(loec_per_slice),
    stringsAsFactors = FALSE
  )

  # Overall NOEC: most conservative = lowest NOEC across slices
  # Map NOECs to dose_levels positions; handle "< lowest_dose" as position 0
  noec_positions <- match(noec_per_slice, dose_levels)
  # If a NOEC starts with "<", it means below all tested doses -> position 0

  noec_positions[is.na(noec_positions)] <- 0L
  min_pos <- which.min(noec_positions)
  overall_NOEC <- unname(noec_per_slice[min_pos])
  overall_LOEC <- unname(loec_per_slice[min_pos])

  output <- list(
    slice_results = slice_summary,
    overall_NOEC = overall_NOEC,
    overall_LOEC = overall_LOEC,
    noec_per_slice = noec_df,
    cutpoints = cutpoints,
    detail = detail_list,
    alternative = alternative,
    alpha = alpha
  )
  class(output) <- "mqjtTest"
  output
}


#' Step-Down Jonckheere-Terpstra Trend Test
#'
#' Performs a step-down JT test starting from all dose groups and sequentially
#' removing the highest dose group until the trend is no longer significant.
#' This follows the standard ecotoxicology step-down paradigm for NOEC
#' determination.
#'
#' @param response Numeric vector of responses (replicate-level proportions
#'   or individual binary values).
#' @param group Factor vector of dose groups (ordered from control to highest).
#' @param alternative Character. \code{"greater"} or \code{"less"}.
#' @param alpha Numeric. Significance level (default 0.05).
#'
#' @return A list with:
#'   \item{summary}{Data frame of step-down results with columns:
#'     \code{groups_included}, \code{highest_dose}, \code{JT_statistic},
#'     \code{Z_statistic}, \code{p_value}, \code{significant}.}
#'   \item{NOEC}{Character. The determined NOEC dose level.}
#'   \item{LOEC}{Character. The determined LOEC dose level.}
#'
#' @details
#' The procedure starts by testing for a trend across all dose groups
#' (including control). If significant at level \code{alpha}, the highest
#' dose is removed and the test is repeated. This continues until either
#' the test is no longer significant (the highest remaining dose is the
#' NOEC) or only two groups remain.
#'
#' Uses \code{PMCMRplus::jonckheereTest()} as the underlying test engine.
#'
#' @keywords internal
#' @export
step_down_jt <- function(
  response,
  group,
  alternative = "greater",
  alpha = 0.05
) {
  group <- factor(group, levels = levels(group))
  dose_levels <- levels(group)
  k <- length(dose_levels)

  if (k < 2) {
    stop("Need at least 2 groups for the step-down JT test.")
  }

  results <- vector("list", k - 1)

  for (i in k:2) {
    keep <- group %in% dose_levels[1:i]
    sub_response <- response[keep]
    sub_group <- factor(group[keep], levels = dose_levels[1:i])

    # Need at least 2 observations per group
    tab <- table(sub_group)
    if (any(tab < 1)) {
      warning(sprintf(
        "Empty group(s) when testing through %s. Skipping.",
        dose_levels[i]
      ))
      next
    }

    jt <- suppressWarnings(PMCMRplus::jonckheereTest(
      sub_response ~ sub_group,
      alternative = alternative
    ))

    results[[k - i + 1]] <- data.frame(
      groups_included = paste(dose_levels[1:i], collapse = " | "),
      highest_dose = dose_levels[i],
      JT_statistic = as.numeric(jt$estimate),
      Z_statistic = as.numeric(jt$statistic),
      p_value = jt$p.value,
      significant = jt$p.value < alpha,
      stringsAsFactors = FALSE
    )
  }

  summary_df <- do.call(rbind, Filter(Negate(is.null), results))
  rownames(summary_df) <- NULL

  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(list(
      summary = data.frame(
        groups_included = character(0),
        highest_dose = character(0),
        JT_statistic = numeric(0),
        Z_statistic = numeric(0),
        p_value = numeric(0),
        significant = logical(0),
        stringsAsFactors = FALSE
      ),
      NOEC = dose_levels[length(dose_levels)],
      LOEC = NA_character_
    ))
  }

  # Determine NOEC via step-down logic
  # summary_df is ordered from full model (all doses) to smallest (ctrl + lowest dose)
  sig_vec <- summary_df$significant

  if (all(sig_vec)) {
    # All steps significant -> NOEC is below the lowest tested dose
    NOEC <- paste0("< ", dose_levels[2])
    LOEC <- dose_levels[2]
  } else if (!sig_vec[1]) {
    # Full model not significant -> no trend detected
    NOEC <- dose_levels[length(dose_levels)]
    LOEC <- NA_character_
  } else {
    # Find first non-significant step (stepping down from highest dose)
    first_ns <- which(!sig_vec)[1]
    NOEC <- summary_df$highest_dose[first_ns]
    if (first_ns > 1) {
      LOEC <- summary_df$highest_dose[first_ns - 1]
    } else {
      LOEC <- NA_character_
    }
  }

  list(
    summary = summary_df,
    NOEC = NOEC,
    LOEC = LOEC
  )
}


#' @export
#' @method print mqjtTest
print.mqjtTest <- function(x, ...) {
  cat("Multiquantal Jonckheere-Terpstra (MQJT) Test\n")
  cat("=============================================\n\n")
  cat("Alternative hypothesis:", x$alternative, "\n")
  cat("Significance level:", x$alpha, "\n")
  cat("Cutpoints:", paste(x$cutpoints, collapse = ", "), "\n\n")

  cat("NOEC per slice:\n")
  print(x$noec_per_slice, row.names = FALSE)

  cat("\n")
  cat("Overall NOEC:", x$overall_NOEC, "\n")
  cat("Overall LOEC:", x$overall_LOEC, "\n")
  invisible(x)
}


#' Summarise MQJT Results as a Data Frame
#'
#' @param x An object of class \code{"mqjtTest"}.
#' @param ... Additional arguments (ignored).
#'
#' @return A data frame with one row per cutpoint showing the NOEC and LOEC.
#' @export
#' @method summary mqjtTest
summary.mqjtTest <- function(object, ...) {
  cat("Multiquantal Jonckheere-Terpstra (MQJT) Test Summary\n")
  cat("====================================================\n\n")
  cat("Alternative:", object$alternative, "| Alpha:", object$alpha, "\n\n")

  cat("Step-down results per cutpoint:\n\n")
  for (cp in names(object$detail)) {
    cat(sprintf("--- Cutpoint: %s (stage >= %s) ---\n", cp, cp))
    print(object$detail[[cp]]$summary, row.names = FALSE)
    cat(sprintf(
      "  NOEC: %s | LOEC: %s\n\n",
      object$detail[[cp]]$NOEC,
      object$detail[[cp]]$LOEC
    ))
  }

  cat("NOEC per slice:\n")
  print(object$noec_per_slice, row.names = FALSE)
  cat(sprintf("\nOverall NOEC: %s\n", object$overall_NOEC))
  cat(sprintf("Overall LOEC: %s\n", object$overall_LOEC))
  invisible(object$noec_per_slice)
}
