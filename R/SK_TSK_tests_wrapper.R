## README
## Functions included in this R file
##* *SpearmanKarber_modified*
##* *Modular `extract_*` Functions:
##*


## A cleaned-up and fixed version of Harold's SpearmanKarber_modified function.
## It corrects indexing, handles zero-dose safely, avoids hard-coded sizes,
## simplifies pruning/pooling when control mortality > 0, and computes CIs in a stable way.
## I kept your two-branch logic (p0 ≈ 0 vs p0 > 0) and Fieller CI for the latter.

#' Spearman-Karber Estimation with Modified Handling for Control Mortality
#'
#' Estimates the LC50 (median lethal concentration) or ED50 (median effective dose)
#' and its confidence interval using the Spearman-Karber method. This version includes
#' robust handling for zero-dose (control) mortality and a Fieller-like approach for
#' confidence interval estimation when control mortality is non-zero.
#'
#' @details
#' The function operates in two branches:
#'   - If control mortality (p0) is near zero, the standard Spearman-Karber method is used on the log-transformed doses, assuming the first observed response is 0%.
#'   -  If p0 is non-zero, it estimates the background rate (c) by cumulative pooling, Abbott-corrects the proportions, prunes/pools low-dose groups, and uses a modified Spearman-Karber method on the scaled proportions. Confidence intervals are calculated using an approach inspired by Fieller's theorem to account for the background correction uncertainty.
#'
#' All concentrations must be non-negative and in increasing order.
#'
#' @param conc A numeric vector of doses or concentrations (must be >= 0 and in increasing order).
#' @param dead A numeric vector of the number of organisms that died or responded at each \code{conc} level.
#' @param total A numeric vector of the total number of organisms (population size) at each \code{conc} level. Can be a single value if all are the same.
#' @param conf.level A single numeric value specifying the confidence level for the interval estimation (e.g., 0.95 for a 95% CI). Must be in (0, 1).
#' @param retData A logical value. If \code{TRUE}, the results are returned in a list. If \code{FALSE}, \code{invisible(NULL)} is returned.
#' @param showOutput A logical value. If \code{TRUE}, the input data table and estimation results (including log-scale values and CIs) are printed to the console.
#' @param showPlot A logical value. If \code{TRUE}, a plot is generated showing the observed and Abbott-corrected mortalities/responses, the estimated LC50, and its confidence interval.
#'
#' @return A list containing the estimation results if \code{retData} is \code{TRUE}, otherwise \code{invisible(NULL)}. The list includes:
#'   \item{\code{log10LC50}}{Estimated log10 of the LC50.}
#'   \item{\code{varianceOfLog10LC50}}{Estimated variance of log10LC50.}
#'   \item{\code{StandardDeviationOfm}}{Estimated standard deviation of log10LC50.}
#'   \item{\code{confidenceIntervalLog10}}{CI for log10LC50 (vector of two values).}
#'   \item{\code{LC50}}{Estimated LC50 (10^log10LC50).}
#'   \item{\code{confidenceIntervalLC50}}{CI for LC50 (vector of two values).}
#'   \item{\code{conf.level}}{The confidence level used for the CIs.}
#'
#' @export
#'
#' @references
#' ART CARTER, WYETH-AYERST RESEARCH, CHAZY, NY (1994): Using the Spearman-Karber Method to Estimate the ED50.
#' Proceedings of the Nineteenth Annual SAS Users Group International Conference, Dallas, Texas, April, pp. 10-13.
#' \url{http://www.sascommunity.org/sugi/SUGI94/Sugi-94-195%20Carter.pdf}.
#'
#' @seealso \code{\link[drcHelper]{tsk_auto}}
#'
#' @author Sarah Baumert, Harald Schulz, Zhenglei Gao
#'
#' @examples
#' # Example 1: Zero control mortality (standard Spearman-Karber)
#' x1 <- c(0, 0.2, 0.3, 0.375, 0.625, 2)
#' n1 <- c(30, 30, 30, 30, 30, 30)
#' r1 <- c(0, 1, 3, 16, 24, 30)
#' SpearmanKarber_modified(x1, r1, n1, showOutput = TRUE)
#'
#' # Example 2: Non-zero control mortality (modified method)
#' x2 <- c(0, 15.54, 20.47, 27.92, 35.98, 55.52)
#' n2 <- c(40, 40, 40, 40, 40, 40)
#' r2 <- c(3, 2, 6, 11, 18, 33)
#' results <- SpearmanKarber_modified(x2, r2, n2, retData = TRUE,
#'                                    showOutput = TRUE, showPlot = TRUE)
#' print(results$LC50)
SpearmanKarber_modified <- function(conc, dead, total,
                                    conf.level = 0.95,
                                    retData = TRUE, showOutput = FALSE, showPlot = FALSE) {
  # Basic checks
  if (any(is.na(conc))) stop("conc must not contain NA")
  if (any(is.na(dead))) stop("dead must not contain NA")
  if (any(is.na(total))) stop("total must not contain NA")
  if (!is.numeric(conc)) stop("conc must be numeric")
  if (!is.numeric(dead)) stop("dead must be numeric")
  if (!is.numeric(total)) stop("total must be numeric")

  N <- length(conc)
  if (N != length(dead)) stop("Different numbers of concentrations and responses were given")
  if (length(total) == 1) total <- rep(total, N)
  if (N != length(total)) stop("Different numbers of concentrations and subject groups were given")
  if (any(conc < 0)) stop("All concentrations must be >= 0. Provide concentrations, not log10(concentrations).")
  if (!all(conc == conc[order(conc)])) stop("Data must be in order of increasing concentration.")
  if (any(dead < 0)) stop("Dead (responses) must be >= 0")
  if (any(total <= 0)) stop("Population sizes must be > 0")
  if (any(dead > total)) stop("dead cannot exceed total")
  if (conf.level <= 0 || conf.level >= 1) stop("conf.level must be in (0,1)")

  # Observed proportions
  p <- dead / total
  p0 <- p[1]
  eps <- .Machine$double.eps^0.5
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  # Abbott-corrected proportions for reporting/plotting (clamped to [0,1])
  if (p0 >= 1 - 1e-12) {
    stop("Control mortality is 100% (or extremely close). Spearman-Karber not applicable.")
  }
  pAbbott <- (p - p0) / (1 - p0)
  pAbbott <- pmin(pmax(pAbbott, 0), 1)
  adjustCounts <- pAbbott * total

  # Containers for outputs
  mu <- NA_real_
  LC50 <- NA_real_
  varianceOfMu <- NA_real_
  ciMu <- c(NA_real_, NA_real_)
  ciLC50 <- c(NA_real_, NA_real_)
  ciHalfWidth <- NA_real_  # for reporting convenience

  # Helper for variance term with interior i
  # xlog is log10(x) for positive x; t is totals for same indices; pin is proportions (not Abbott) for same indices
  variance_SK <- function(xlog, pin, t) {
    M <- length(xlog)
    if (M < 3) return(NA_real_)
    v <- 0
    for (i in 2:(M - 1)) {
      v <- v + 0.25 * (xlog[i - 1] - xlog[i + 1])^2 * (pin[i] * (1 - pin[i])) / t[i]
    }
    v
  }

  # Case A: control mortality effectively zero
  if (p0 <= eps) {
    # Use only positive concentrations for log10
    pos_idx <- which(conc > 0)
    if (length(pos_idx) < 2L) {
      stop("Need at least two positive concentrations when control mortality is zero.")
    }
    x <- conc[pos_idx]
    p_use <- p[pos_idx]         # equals pAbbott[pos_idx] since p0 ~ 0
    t_use <- total[pos_idx]

    # Build extended log-dose with ghost endpoints
    xlog_ext <- numeric(length(x) + 2L)
    xlog_ext[2:(length(x) + 1L)] <- log10(x)
    # Low ghost (needs at least two positive x)
    xlog_ext[1] <- 2 * xlog_ext[2] - xlog_ext[3]
    # High ghost
    xlog_ext[length(x) + 2L] <- 2 * xlog_ext[length(x) + 1L] - xlog_ext[length(x)]

    # Build extended proportions: 0 at low, observed in middle, 1 at high
    p_ext <- numeric(length(p_use) + 2L)
    p_ext[1] <- 0
    p_ext[2:(length(p_use) + 1L)] <- p_use
    p_ext[length(p_use) + 2L] <- 1

    # Spearman-Karber trapezoid
    mids <- 0.5 * (xlog_ext[-length(xlog_ext)] + xlog_ext[-1])
    dp <- diff(p_ext)
    mu <- sum(dp * mids)
    LC50 <- 10^mu

    # Variance on observed internal points (interior of observed grid, not ghosts)
    v <- variance_SK(xlog = xlog_ext[2:(length(xlog_ext) - 1L)],
                     pin = p_use,
                     t = t_use)
    varianceOfMu <- if (is.na(v)) NA_real_ else max(v, 0)
    sdMu <- if (is.na(varianceOfMu)) NA_real_ else sqrt(varianceOfMu)
    if (!is.na(sdMu)) {
      ciMu <- c(mu - z * sdMu, mu + z * sdMu)
      ciLC50 <- 10^ciMu
      ciHalfWidth <- z * sdMu
    }

  } else {
    # Case B: nonzero control mortality, estimate background c by cumulative pooling
    cum_dead <- cumsum(dead)
    cum_total <- cumsum(total)
    cum_ratio <- cum_dead / cum_total
    k_star <- which.min(cum_ratio)[1]
    c_bg <- cum_ratio[k_star]
    n_c <- cum_total[k_star]

    # Prune: keep all observations with p >= c_bg - small tolerance; pool others into the first kept
    keep_mask <- p >= (c_bg - 1e-12)
    if (!any(keep_mask)) stop("After background-rate screening, no observations remain.")
    keep_idx <- which(keep_mask)
    # Ensure order
    x_keep <- conc[keep_idx]
    d_keep <- dead[keep_idx]
    n_keep <- total[keep_idx]

    # Pool the excluded into the first kept entry
    if (any(!keep_mask)) {
      d_pool <- sum(dead[!keep_mask])
      n_pool <- sum(total[!keep_mask])
      d_keep[1] <- d_keep[1] + d_pool
      n_keep[1] <- n_keep[1] + n_pool
    }

    # Ensure positive concentrations for integration; pool any nonpositive into first positive
    pos_keep <- which(x_keep > 0)
    if (length(pos_keep) < 2L) {
      stop("Need at least two positive concentrations after background-rate adjustment.")
    }
    if (any(x_keep <= 0)) {
      first_pos <- pos_keep[1]
      d_keep[first_pos] <- d_keep[first_pos] + sum(d_keep[x_keep <= 0])
      n_keep[first_pos] <- n_keep[first_pos] + sum(n_keep[x_keep <= 0])
      keep_positive <- x_keep > 0
      x_keep <- x_keep[keep_positive]
      d_keep <- d_keep[keep_positive]
      n_keep <- n_keep[keep_positive]
    }

    p_keep <- d_keep / n_keep
    xlog <- log10(x_keep)
    M <- length(xlog)

    # Rescale by background-rate c to [0,1]; force the first to be exactly 0
    p_scaled <- (p_keep - c_bg) / (1 - c_bg)
    p_scaled <- pmin(pmax(p_scaled, 0), 1)
    p_scaled[1] <- 0

    # Append a high ghost endpoint at p=1 and log-dose extrapolated
    xlog_ext <- c(xlog, 2 * xlog[M] - xlog[M - 1])
    p_scaled_ext <- c(p_scaled, 1)

    # Raw SK on scaled proportions
    mids <- 0.5 * (xlog_ext[-length(xlog_ext)] + xlog_ext[-1])
    dp <- diff(p_scaled_ext)
    mu_raw <- sum(dp * mids)

    # Variance on original (unscaled) proportions, interior points only
    v_raw <- variance_SK(xlog = xlog, pin = p_keep, t = n_keep)
    varianceOfMu <- if (is.na(v_raw)) NA_real_ else max(v_raw, 0)

    # Fieller-based CI on log-scale (following your structure)
    a <- 0.5 * (xlog[1] + xlog[2])  # first-segment midpoint
    V12 <- (a^2) * c_bg * (1 - c_bg) / n_c
    V22 <- c_bg * (1 - c_bg) / n_c
    V11 <- varianceOfMu + V12
    B <- 1 - c_bg
    g <- (z^2) * V22 / (B^2)
    # Guard against edge cases
    discr <- V11 - 2 * mu_raw * V12 + (mu_raw^2) * V22 - g * (V11 - (V12^2) / V22)
    discr <- max(discr, 0)
    denom <- (1 - g)
    if (abs(denom) < 1e-12) {
      sigma <- NA_real_
    } else {
      sigma <- (z / B) * sqrt(discr) / denom
    }
    # Adjust mu for background c
    mu <- (mu_raw - c_bg * a) / (1 - c_bg)
    LC50 <- 10^mu

    if (is.finite(sigma)) {
      ciMu <- sort(c(mu - sigma, mu + sigma))
      ciLC50 <- 10^ciMu
      # Back out a variance proxy from sigma (approximate)
      varianceOfMu <- max((sigma / z)^2, varianceOfMu)
      ciHalfWidth <- sigma
    } else {
      ciMu <- c(NA_real_, NA_real_)
      ciLC50 <- c(NA_real_, NA_real_)
      ciHalfWidth <- NA_real_
    }
  }

  # Plot if requested
  if (isTRUE(showPlot)) {
    xlimMax <- max(conc, na.rm = TRUE)
    plot(conc, p * 100, type = "p", pch = 4, xlim = c(0, xlimMax),
         ylim = c(0, 100), ylab = "Response, %", xlab = "Concentration", cex = 0.7)
    lines(conc, pAbbott * 100, lty = 2)
    legend("topleft",
           c("Observed Mortality %", "Abbott-corrected %"),
           lty = c(0, 2), pch = c(4, NA), cex = 0.8, bty = "n")
    points(LC50, 50, pch = 16, col = "blue")
    # horizontal error bar for LC50 CI (if available)
    if (all(is.finite(ciLC50))) {
      segments(x0 = ciLC50[1], y0 = 50, x1 = ciLC50[2], y1 = 50, col = "blue")
      segments(x0 = ciLC50[1], y0 = 50 - 2, x1 = ciLC50[1], y1 = 50 + 2, col = "blue")
      segments(x0 = ciLC50[2], y0 = 50 - 2, x1 = ciLC50[2], y1 = 50 + 2, col = "blue")
    }
  }

  # Console output if requested
  if (isTRUE(showOutput)) {
    out <- data.frame(
      idx = seq_len(N),
      concentration = conc,
      total = total,
      dead = dead,
      obs_prop = round(p, 6),
      abbott_prop = round(pAbbott, 6),
      abbott_count = round(adjustCounts, 3)
    )
    print(out, row.names = FALSE)
    cat("log10(LC50) =", mu, "\n")
    cat("estimated variance of log10(LC50) =", varianceOfMu, "\n")
    cat("approx. half-width on log10-scale =", ciHalfWidth, "\n")
    cat(paste0(round(100 * conf.level, 1), "% CI for log10(LC50) = [",
               paste(ciMu, collapse = ", "), "]\n"))
    cat("estimated LC50 =", LC50, "\n")
    cat(paste0("estimated ", round(100 * conf.level, 1), "% CI for LC50 = [",
               paste(ciLC50, collapse = ", "), "]\n"))
  }

  if (isTRUE(retData)) {
    return(list(
      log10LC50 = mu,
      varianceOfLog10LC50 = varianceOfMu,
      StandardDeviationOfm = if (is.na(varianceOfMu)) NA_real_ else sqrt(varianceOfMu),
      confidenceIntervalLog10 = ciMu,
      LC50 = LC50,
      confidenceIntervalLC50 = ciLC50,
      conf.level = conf.level
    ))
  } else {
    invisible(NULL)
  }
}





##*   **Modular `extract_*` Functions:**
##*   I created a set of individual `extract_*` functions, one for each SK method.
##*   Each function runs its respective model, safely captures key results (LC50, CI, etc.),
##*   and returns them in a standardized, single-row data frame.
##*   This allows you to easily combine their outputs into a final comparison table.

# REVISED: Helper to create a standardized output row with clearer SD columns
standard_row <- function(method, scale, trim_or_A = NA, est = NA, lcl = NA, ucl = NA,
                         log10est = NA, sd_lc50 = NA, sd_log10 = NA, gsd = NA,
                         notes = "") {
  data.frame(
    method = as.character(method),
    scale = as.character(scale),
    trim_or_A = as.numeric(trim_or_A),
    LC50 = as.numeric(est),
    LCL = as.numeric(lcl),
    UCL = as.numeric(ucl),
    log10LC50 = as.numeric(log10est),
    SD_LC50 = as.numeric(sd_lc50),      # SD on the same scale as LC50
    SD_log10 = as.numeric(sd_log10),    # SD on the log10 scale
    GSD = as.numeric(gsd),
    notes = as.character(notes),
    stringsAsFactors = FALSE
  )
}


# CORRECTED: For drcHelper::tsk_auto
extract_tsk_auto <- function(x, n, r, use.log.doses = TRUE, conf.level = 0.95) {
  method_name <- "tsk_auto"
  scale_name <- if (use.log.doses) "log-dose" else "linear-dose"

  obj <- try(
    drcHelper::tsk_auto(data.frame(x = x, n = n, r = r), use.log.doses = use.log.doses, conf.level = conf.level),
    silent = TRUE
  )

  if (inherits(obj, "try-error")) {
    return(standard_row(method_name, scale_name, notes = as.character(obj)))
  }

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # *** THE FIX IS HERE: Added obj$mu to the list of possible estimate names ***
  est <- obj$LD50 %||% obj$ED50 %||% obj$mu %||% NA

  ci <- obj$conf.int %||% obj$ci %||% c(NA, NA)
  sd_val <- obj$sd %||% obj$SD %||% NA
  gsd_val <- obj$GSD %||% obj$gsd %||% NA
  trim_val <- obj$trim %||% attr(obj, "trim") %||% NA

  standard_row(
    method = method_name,
    scale = scale_name,
    trim_or_A = trim_val,
    est = est,
    lcl = ci[1],
    ucl = ci[2],
    log10est = if (is.finite(est) && est > 0) log10(est) else NA,
    sd_lc50 = if (!use.log.doses) sd_val else NA, # SD(LC50) is returned for linear scale
    sd_log10 = if (use.log.doses && is.finite(gsd_val)) log(gsd_val) else NA, # SD(log10) is ~log(GSD) for log scale
    gsd = gsd_val
  )
}


# UPDATED: For your custom function to match new standard_row
extract_SpearmanKarber_modified <- function(x, n, r, conf.level = 0.95) {
  skm <- try(
    SpearmanKarber_modified(x, r, n, conf.level = conf.level, retData = TRUE),
    silent = TRUE
  )

  if (inherits(skm, "try-error")) {
    return(standard_row("SpearmanKarber_modified", "log-dose", notes = as.character(skm)))
  }

  standard_row(
    method = "SpearmanKarber_modified",
    scale = "log-dose",
    est = skm$LC50,
    lcl = skm$confidenceIntervalLC50[1],
    ucl = skm$confidenceIntervalLC50[2],
    log10est = skm$log10LC50,
    sd_log10 = if (!is.null(skm$StandardDeviationOfm)) skm$StandardDeviationOfm else NA
  )
}


# UPDATED: For ecotoxicology::SpearmanKarber to match new standard_row
extract_ecotox_SpearmanKarber <- function(x, n, r) {
  if (length(unique(n)) != 1) {
    return(standard_row("ecotox::SpearmanKarber", "log-dose", notes = "Requires constant N"))
  }

  use_idx <- which(x > 0)
  if (length(use_idx) < 2) {
    return(standard_row("ecotox::SpearmanKarber", "log-dose", notes = "Requires at least 2 positive doses"))
  }

  toxData <- cbind(x[use_idx], r[use_idx], r[use_idx] / n[use_idx])

  obj <- try(
    ecotoxicology::SpearmanKarber(toxData, N = unique(n)[1], retData = TRUE, showOutput = FALSE, showPlot = FALSE),
    silent = TRUE
  )

  if (inherits(obj, "try-error")) {
    return(standard_row("ecotox::SpearmanKarber", "log-dose", notes = as.character(obj)))
  }

  standard_row(
    method = "ecotox::SpearmanKarber",
    scale = "log-dose",
    est = obj$LC50,
    lcl = obj$confidenceInterval95LC50[1],
    ucl = obj$confidenceInterval95LC50[2],
    log10est = obj$log10LC50,
    sd_log10 = if (!is.null(obj$varianceOfm)) sqrt(obj$varianceOfm) else NA
  )
}



#' Unified Spearman-Karber Analysis Function
#'
#' This function serves as a wrapper to run various Spearman-Karber (SK) and
#' Trimmed Spearman-Karber (TSK) analyses using a single interface.
#' It returns a standardized one-row data frame for easy comparison.
#'
#' @param x Numeric vector of concentrations.
#' @param n Numeric vector of total subjects per concentration.
#' @param r Numeric vector of responses (e.g., dead) per concentration.
#' @param method The analysis method to use. Must be one of:
#'   - "SK_modified" (your custom SpearmanKarber_modified function)
#'   - "tsk_auto_log" (drcHelper::tsk_auto with log-doses)
#'   - "tsk_auto_linear" (drcHelper::tsk_auto with linear doses)
#'   - "ecotox_SK" (ecotoxicology::SpearmanKarber)
#' @param conf.level The confidence level for intervals (default 0.95).
#' @param ... Additional arguments passed to the underlying functions (e.g., max.trim for tsk_auto).
#'
#' @return A single-row data frame containing the standardized analysis results.
#' @export
#'
#' @examples
#' x <- c(0, 0.2, 0.3, 0.375, 0.625, 2)
#' n <- c(30, 30, 30, 30, 30, 30)
#' r <- c(0, 1, 3, 16, 24, 30)
#'
#' # Run a single analysis
#' analyze_SK(x, n, r, method = "SK_modified")
#'
#' # Run multiple analyses and combine into a single table
#' methods_to_run <- c("SK_modified", "tsk_auto_log", "tsk_auto_linear", "ecotox_SK")
#' all_results <- lapply(methods_to_run, function(m) {
#'   analyze_SK(x, n, r, method = m)
#' })
#' comparison_table <- do.call(rbind, all_results)
#' print(comparison_table)
analyze_SK <- function(x, n, r, method, conf.level = 0.95, ...) {
  # --- Input Validation ---
  available_methods <- c("SK_modified", "tsk_auto_log", "tsk_auto_linear", "ecotox_SK")
  if (!method %in% available_methods) {
    stop(paste("Unknown method specified. Please choose from:", paste(available_methods, collapse = ", ")))
  }

  dots <- list(...)

  # --- Method Dispatch using switch() ---
  switch(
    method,

    "SK_modified" = {
      obj <- try(SpearmanKarber_modified(x, r, n, conf.level = conf.level, retData = TRUE), silent = TRUE)
      if (inherits(obj, "try-error")) {
        return(standard_row(method, "log-dose", notes = as.character(obj)))
      }
      standard_row(method, "log-dose",
                   est = obj$LC50, lcl = obj$confidenceIntervalLC50[1], ucl = obj$confidenceIntervalLC50[2],
                   log10est = obj$log10LC50, sd_log10 = obj$StandardDeviationOfm)
    },

    "tsk_auto_log" = {
      call_args <- c(list(data.frame(x = x, n = n, r = r), use.log.doses = TRUE, conf.level = conf.level), dots)
      obj <- try(do.call(drcHelper::tsk_auto, call_args), silent = TRUE)
      if (inherits(obj, "try-error")) {
        return(standard_row(method, "log-dose", notes = as.character(obj)))
      }
      est <- obj$LD50 %||% obj$ED50 %||% obj$mu %||% NA
      ci <- obj$conf.int %||% obj$ci %||% c(NA, NA)
      gsd <- obj$GSD %||% obj$gsd %||% NA
      standard_row(method, "log-dose", trim_or_A = obj$trim %||% attr(obj, "trim"),
                   est = est, lcl = ci[1], ucl = ci[2],
                   log10est = if (is.finite(est) && est > 0) log10(est) else NA,
                   sd_log10 = if (is.finite(gsd)) log(gsd) else NA, gsd = gsd)
    },

    "tsk_auto_linear" = {
      call_args <- c(list(data.frame(x = x, n = n, r = r), use.log.doses = FALSE, conf.level = conf.level), dots)
      obj <- try(do.call(drcHelper::tsk_auto, call_args), silent = TRUE)
      if (inherits(obj, "try-error")) {
        return(standard_row(method, "linear-dose", notes = as.character(obj)))
      }
      est <- obj$LD50 %||% obj$ED50 %||% obj$mu %||% NA
      ci <- obj$conf.int %||% obj$ci %||% c(NA, NA)
      sd_val <- obj$sd %||% obj$SD %||% NA
      standard_row(method, "linear-dose", trim_or_A = obj$trim %||% attr(obj, "trim"),
                   est = est, lcl = ci[1], ucl = ci[2],
                   log10est = if (is.finite(est) && est > 0) log10(est) else NA,
                   sd_lc50 = sd_val)
    },

    "ecotox_SK" = {
      if (length(unique(n)) != 1) {
        return(standard_row(method, "log-dose", notes = "Requires constant N"))
      }
      use_idx <- which(x > 0)
      if (length(use_idx) < 2) {
        return(standard_row(method, "log-dose", notes = "Requires at least 2 positive doses"))
      }
      toxData <- cbind(x[use_idx], r[use_idx], r[use_idx] / n[use_idx])
      obj <- try(ecotoxicology::SpearmanKarber(toxData, N = unique(n)[1], retData = TRUE, showOutput = FALSE, showPlot = FALSE), silent = TRUE)
      if (inherits(obj, "try-error")) {
        return(standard_row(method, "log-dose", notes = as.character(obj)))
      }
      standard_row(method, "log-dose",
                   est = obj$LC50, lcl = obj$confidenceInterval95LC50[1], ucl = obj$confidenceInterval95LC50[2],
                   log10est = obj$log10LC50, sd_log10 = if (!is.null(obj$varianceOfm)) sqrt(obj$varianceOfm) else NA)
    }
  )
}
