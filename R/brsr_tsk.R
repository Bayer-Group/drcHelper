## Integrated from tsk single function package with licence GPL3 (GNU General Public License v3.0)


#' This is hamilton data
#'
#' Example dose-response data given in Hamilton (1977).
#' Note that, as per Hamilton (1978), the confidence intervals
#' given in Hamilton (1977) for these data sets are incorrect.
#' @author B R S Recht
#' @format A list containing ten data frames: dr1a, dr1b, dr1c,
#' dr1d, dr1e, dr4a, dr4b, dr4c, dr4d, dr4e,
#' @docType data
#' @source Hamilton, 1977.
#' @references \url{https://github.com/brsr/tsk}
#' @keywords data
"hamilton"


#' Trimmed Spearman-Karber Method by brsr
#'
#' The function is adapted from the single function package tsk developed with
#' GPL3 license. It is included for validation purpose.
#'
#'
#' @param ... inputs
#' @return tsk estimations
#' @export
#' @references \url{https://github.com/brsr/tsk}
#'
tsk <- function(...) UseMethod("tsk")

## wrapper for extracting output. not used anymore, see @extract_tsk_auto
extract_tsk_like <- function(obj) {
  # Try common fields; fall back to NA
  est <- obj$LD50 %||% obj$ED50 %||% obj$estimate %||% NA_real_
  lcl <- (obj$conf.int %||% obj$ci %||% c(NA_real_, NA_real_))[1]
  ucl <- (obj$conf.int %||% obj$ci %||% c(NA_real_, NA_real_))[2]
  sd <- obj$sd %||% obj$SE %||% NA_real_
  gsd <- obj$GSD %||% obj$gsd %||% NA_real_
  list(est = as.numeric(est), lcl = as.numeric(lcl), ucl = as.numeric(ucl),
       sd = as.numeric(sd), gsd = as.numeric(gsd))
}


#' Auto-trimmed TSK Analysis
#'
#' This function automatically determines the appropriate trim level for TSK analysis
#' and applies it. It first tries with no trimming, and if that fails due to responses
#' not spanning the required range, it automatically calculates and applies the minimum
#' required trim level based on the data characteristics.
#'
#' The automatic trimming is triggered when the response proportions don't increase
#' from the trim level to 1-trim level, which typically occurs when responses are
#' too close to 0% or 100% at the extreme doses.
#'
#' @param x A numeric vector of doses (for numeric method) or a data frame
#'   containing columns 'x', 'n', and 'r' (for data.frame method).
#' @param n A numeric vector of total counts (for numeric method only).
#' @param r A numeric vector of response counts (for numeric method only).
#' @param control A numeric value indicating the control dose (default is 0).
#' @param conf.level A numeric value indicating the confidence level (default is 0.95).
#' @param use.log.doses A logical value indicating whether to use log-transformed
#'   doses (default is TRUE).
#' @param max.trim A numeric value indicating the maximum allowed trim level
#'   (default is 0.45, must be < 0.5).
#' @param ... Additional arguments passed to the tsk function.
#' @return The result of the TSK analysis with automatic trimming applied.
#' @export
#' @examples
#' \dontrun{
#' # With numeric vectors - data that needs trimming
#' doses <- c(0, 1, 2, 3, 4, 5)
#' total <- rep(20, 6)
#' responses <- c(0, 2, 8, 14, 18, 20)  # Goes from 0 to 100%
#' result <- tsk_auto(doses, total, responses)
#'
#' # With data frame - moderate responses that may not need trimming
#' data <- data.frame(
#'   x = c(0.1, 0.5, 1, 2, 4, 8),
#'   n = rep(20, 6),
#'   r = c(2, 5, 8, 12, 15, 17)
#' )
#' result <- tsk_auto(data)
#'
#' # Using hamilton dataset (if available)
#' if (exists("hamilton")) {
#'   # Try with one of the hamilton datasets
#'   result <- tsk_auto(hamilton$dr1a)
#' }
#' }
tsk_auto <- function(x, ...) {
  UseMethod("tsk_auto")
}

#' @rdname tsk_auto
#' @method tsk_auto numeric
#' @export
tsk_auto.numeric <- function(x, n, r, control = 0, conf.level = 0.95,
                             use.log.doses = TRUE, max.trim = 0.45, ...) {
  input <- data.frame(x = x, n = n, r = r)
  tsk_auto.data.frame(input, control = control, conf.level = conf.level,
                      use.log.doses = use.log.doses, max.trim = max.trim, ...)
}

#' @rdname tsk_auto
#' @method tsk_auto data.frame
#' @export
tsk_auto.data.frame <- function(x, control = 0, conf.level = 0.95,
                                use.log.doses = TRUE, max.trim = 0.45, ...) {
  input <- x
  # Optional: strip 'trim' from ... to avoid duplicate matching if user passes it to tsk_auto
  dots <- list(...)
  if (!is.null(dots$trim)) {
    warning("tsk_auto ignores 'trim'. Use drcHelper::tsk(...) for explicit trims. Removing 'trim' from ...")
    dots$trim <- NULL
  }

  # Validate max.trim
  if (max.trim <= 0 || max.trim >= 0.5) {
    stop("max.trim must be between 0 and 0.5 (exclusive).")
  }
  # ADD THIS BLOCK: preemptively trim if log-doses requested and zero (or negative) doses exist
  if (isTRUE(use.log.doses) && any(input$x <= 0, na.rm = TRUE)) {
    # Minimal safe trim: roughly 1/min(total per dose), capped by max.trim
    # Falls back to 0.001 if totals are missing
    base_trim <- tryCatch(1 / max(input$n, na.rm = TRUE), error = function(e) NA_real_)
    if (!is.finite(base_trim) || base_trim <= 0) base_trim <- 0.001
    auto_trim <- min(base_trim, max.trim)

    message(sprintf("tsk_auto: zero dose with log transform detected; applying auto-trim = %.4f", auto_trim))
    # Try trimmed call; if it fails with a “suggested trim” message, upgrade trim slightly (capped)
    return(tryCatch(
      do.call(tsk, c(list(input, control = control, trim = auto_trim, conf.level = conf.level,
                          use.log.doses = use.log.doses), dots)),
      error = function(e) {
        if (grepl("consider using this trim:", e$message)) {
          suggested_trim_match <- regmatches(e$message, regexpr("consider using this trim: [0-9.]+", e$message))
          if (length(suggested_trim_match) > 0) {
            suggested_trim <- as.numeric(sub("consider using this trim: ", "", suggested_trim_match))
            auto_trim2 <- min(suggested_trim + 0.001, max.trim)
            message(sprintf("tsk_auto: increasing auto-trim to %.4f due to suggestion", auto_trim2))
            return(do.call(tsk, c(list(input, control = control, trim = auto_trim2, conf.level = conf.level,
                                       use.log.doses = use.log.doses), dots)))
          }
        }
        stop(e)
      }
    ))
  }
  # First try with no trimming
  result <- tryCatch({
    tsk(input, control = control, trim = 0, conf.level = conf.level,
        use.log.doses = use.log.doses, ...)
  }, error = function(e) {
    # Only apply auto-trimming for specific trim-related errors
    if (grepl("responses do not increase from trim to 1-trim", e$message)) {
      # Extract suggested trim from error message
      suggested_trim_match <- regmatches(e$message,
                                        regexpr("consider using this trim: [0-9.]+", e$message))

      if (length(suggested_trim_match) > 0) {
        suggested_trim <- as.numeric(sub("consider using this trim: ", "", suggested_trim_match))

        # Apply a small buffer to ensure success, but cap at max.trim
        auto_trim <- min(suggested_trim + 0.001, max.trim)

        message(paste("Auto-trimming applied: trim =", round(auto_trim, 4)))
        message(paste("Reason: Responses don't span the full range from 0 to 1"))

        # Try again with calculated trim
        tsk(input, control = control, trim = auto_trim, conf.level = conf.level,
            use.log.doses = use.log.doses, ...)
      } else {
        # Re-throw if we can't parse the suggested trim
        stop(e)
      }
    } else {
      # Re-throw other errors
      stop(e)
    }
  })

  return(result)
}

#' TSK Analysis for Numeric Input
#'
#' This function performs TSK analysis for numeric input.
#'
#' @param x A numeric vector of doses.
#' @param n A numeric vector of total counts.
#' @param r A numeric vector of response counts.
#' @param control A numeric value indicating the control dose (default is 0).
#' @param trim A numeric value indicating the trim level (default is 0).
#' @param conf.level A numeric value indicating the confidence level
#'  (default is 0.95).
#' @param use.log.doses A logical value indicating whether to use
#' log-transformed doses (default is TRUE).
#' @param ... Additional arguments passed to the function.
#' @return The result of the TSK analysis.
#' @method tsk numeric
#' @export
tsk.numeric <-
  function(x, n, r, control = 0, trim = 0, conf.level = 0.95,
           use.log.doses = TRUE, ...) {
    input <- data.frame(x = x, n = n, r = r)
    tsk(input, control, trim, conf.level, use.log.doses, ...)
  }

#' TSK Analysis for Data Frame Input
#'
#' This function performs TSK analysis for data frame input.
#'
#' @param input A data frame containing the columns `x` (doses),
#'  `n` (total counts), and `r` (response counts).
#' @param control A numeric value indicating the control dose (default is 0).
#' @param trim A numeric value indicating the trim level (default is 0).
#' @param conf.level A numeric value indicating the confidence level
#' (default is 0.95).
#' @param use.log.doses A logical value indicating whether to use
#' log-transformed doses (default is TRUE).
#' @param ... Additional arguments passed to the function.
#' @return The result of the TSK analysis.
#' @export
#' @method tsk data.frame
#' @importFrom isotone gpava
tsk.data.frame <-
    function(input, control = 0, trim = 0, conf.level = 0.95,
             use.log.doses = TRUE, ...) {
        ### ---CHECK THAT THE INPUT IS VALID------------------------------
        if (use.log.doses && any(input$x < 0)) {
            warning(
                "Negative doses exist in the data. ",
                "Forcing use.log.doses=FALSE."
            )
            use.log.doses <- FALSE
        }
        if (any(input$r < 0)) {
            stop("Responses must be nonnegative.")
        }
        if (any(input$n <= 0)) {
            stop("Population sizes should be positive.")
        }
        if (trim < 0 || trim >= 0.5) {
            stop("The trim must be between 0 and 0.5.")
        }
        if (conf.level <= 0.5 || conf.level >= 1) {
            stop("The confidence must be between 0.5 and 1.")
        }
        if (anyDuplicated(input$x)) {
            stop("Duplicate doses exist in the data.")
        }
        if (is.unsorted(input$x)) {
            input <- input[order(input$x), ]
        }
        N <- length(input$x)
        ### ---TRANSFORM THE DATA-------------------------------------------
        if (use.log.doses) {
            input$x <- log10(input$x)
        }
        data.smoothed <- data.frame(input, p = input$r / input$n)
        ### Fix nondecreasingness, as per the first step in
        ### Hamilton. What Hamilton describes is equivalent to the
        ### pool-adjacent-violators algorithm, so gpava is used here.
        ### PAVA was published 11 years after Hamilton's paper, so I
        ### can't blame them for not knowing about it. See:
        ### http://stackoverflow.com/questions/11423846/
        ### smoothing-a-sequence-without-using-a-loop-in-r
        needs.smooth <- is.unsorted(data.smoothed$p)
        if (needs.smooth) {
            data.smoothed$p <- isotone::gpava(
                z = data.smoothed$x,
                y = data.smoothed$p,
                weights = data.smoothed$n
            )$x
            data.smoothed$r <- data.smoothed$p * data.smoothed$n
        }

        ### Correct for the control dose.
        data.smoothed$p <- (data.smoothed$p - control) / (1 - control)

        ### ---SCALE AND TRIM---------------------------------------------
        ### As per steps 2 and 3 in Hamilton
        data.scaled <- data.smoothed
        data.scaled$p <- (data.smoothed$p - trim) / (1 - 2 * trim)
        if (data.scaled$p[1L] > 0 || data.scaled$p[length(data.scaled$p)] < 1) {
            trim.maybe <- max(
                data.smoothed$p[1L],
                1 - data.smoothed$p[length(data.scaled$p)]
            )

            stop("After smoothing, the responses do not increase from trim to 1-trim.
		If the data contains a decreasing set of responses, use the opposite
		response: i.e. if the data has death counts, use counts of subjects
		left alive instead.
		If that is not the issue, consider using this trim: ", trim.maybe)
        }
        ### Linearly interpolate points where the lines p=0 and p=1 meet
        ### the trim.
        Larray <- which(data.scaled$p <= 0)
        ln <- Larray[length(Larray)]
        Uarray <- which(data.scaled$p >= 1)
        un <- Uarray[1L]
        keepers <- data.scaled[(data.scaled$p >= 0) & (data.scaled$p <= 1), ]
        trimx <- keepers$x
        trimp <- keepers$p
        ### There's some silliness with the approx interpolation function
        ### here... it doesn't work right if there are mulitple doses
        ### with the same p-values, and ordered, min, max, etc. don't do
        ### what I want. Subscripting seemed more efficient than building
        ### a function to pass to approx to make it work right.
        if (data.scaled$p[ln] != 0) {
            interx <- approx(
                data.scaled$p[ln:(ln + 1)],
                data.scaled$x[ln:(ln + 1)], 0
            )$y
            trimx <- c(interx, trimx)
            trimp <- c(0, trimp)
        }
        if (data.scaled$p[un] != 1) {
            interx <- approx(
                data.scaled$p[(un - 1):un],
                data.scaled$x[(un - 1):un], 1
            )$y
            trimx <- c(trimx, interx)
            trimp <- c(trimp, 1)
        }
        data.trimmed <- data.frame(x = trimx, p = trimp)
        ### ---FIND THE MEAN----------------------------------------------
        ### Step 4 in Hamilton
        trimN <- length(data.trimmed$x)
        midpoints <- (data.trimmed$x[1:(trimN - 1)] + data.trimmed$x[2:trimN]) / 2
        delp <- data.trimmed$p[2:trimN] - data.trimmed$p[1:(trimN - 1)]
        mu <- sum(midpoints * delp)
        LD50 <- 10^mu
        ### ---FIND THE VARIANCE-&-CONF-INTERVAL--------------------------
        ### Appendix in Hamilton
        V1 <-
            function(data, trim, ln, un) {
                return(((data$x[ln + 1] - data$x[ln]) * (data$p[ln + 1] - trim)^2 /
                    (data$p[ln + 1] - data$p[ln])^2)^2 *
                    data$p[ln] * (1 - data$p[ln]) / data$n[ln])
            }
        V2 <-
            function(data, trim, ln, un) {
                return(((data$x[ln] - data$x[ln + 2]) +
                    (data$x[ln + 1] - data$x[ln]) *
                        (trim - data$p[ln])^2 / (data$p[ln + 1] - data$p[ln])^2)^2 *
                    data$p[ln + 1] * (1 - data$p[ln + 1]) / data$n[ln + 1])
            }
        V3 <-
            function(data, trim, ln, un) {
                v3 <- ((data$x[(ln + 1):(un - 3)] - data$x[(ln + 3):(un - 1)])^2 *
                    data$p[(ln + 2):(un - 2)] * (1 - data$p[(ln + 2):(un - 2)]) /
                    data$n[(ln + 2):(un - 2)])
                return(sum(v3))
            }
        V4 <-
            function(data, trim, ln, un) {
                return(((data$x[un - 2] - data$x[un]) +
                    (data$x[un] - data$x[un - 1]) *
                        (data$p[un] - 1 + trim)^2 /
                        (data$p[un] - data$p[un - 1])^2)^2 * data$p[un - 1] *
                    (1 - data$p[un - 1]) / data$n[un - 1])
            }
        V5 <-
            function(data, trim, ln, un) {
                return(((data$x[un] - data$x[un - 1]) *
                    (1 - trim - data$p[un - 1])^2 /
                    (data$p[un] - data$p[un - 1])^2)^2 * data$p[un] *
                    (1 - data$p[un]) / data$n[un])
            }
        V6 <-
            function(data, trim, ln, un) {
                return(((data$x[un] - data$x[ln + 1]) *
                    (1 - trim - data$p[un])^2 /
                    (data$p[un] - data$p[ln + 1])^2 -
                    (data$x[ln + 1] - data$x[ln]) *
                        (trim - data$p[ln])^2 / (data$p[ln + 1] - data$p[ln])^2 +
                    (data$x[ln] - data$x[un]))^2 * data$p[ln + 1] *
                    (1 - data$p[ln + 1]) / data$n[ln + 1])
            }

        s <- un - ln
        if (s <= 0L) {
            Var <- (NaN)
        } else if (s == 1L) {
            Var <- (data.smoothed$x[un] - data.smoothed$x[ln])^2 *
                ((0.5 - data.smoothed$p[un])^2 /
                    (data.smoothed$p[un] - data.smoothed$p[ln])^4 *
                    data.smoothed$p[ln] *
                    (1 - data.smoothed$p[ln]) / data.smoothed$n[ln] +
                    (0.5 - data.smoothed$p[ln])^2 /
                        (data.smoothed$p[un] - data.smoothed$p[ln])^4 *
                        data.smoothed$p[un] *
                        (1 - data.smoothed$p[un]) / data.smoothed$n[un])
        } else if (s == 2L) {
            Var <- (V1(data.smoothed, trim, ln, un) +
                V5(data.smoothed, trim, ln, un) +
                V6(data.smoothed, trim, ln, un)) / (2 - 4 * trim)^2
        } else if (s == 3L) {
            Var <- (V1(data.smoothed, trim, ln, un) +
                V2(data.smoothed, trim, ln, un) +
                V4(data.smoothed, trim, ln, un) +
                V5(data.smoothed, trim, ln, un)) / (2 - 4 * trim)^2
        } else {
            Var <- (V1(data.smoothed, trim, ln, un) +
                V2(data.smoothed, trim, ln, un) +
                V3(data.smoothed, trim, ln, un) +
                V4(data.smoothed, trim, ln, un) +
                V5(data.smoothed, trim, ln, un)) / (2 - 4 * trim)^2
        }

        sd <- sqrt(Var)
        gsd <- 10^(sd)
        v <- -qnorm((1 - conf.level) / 2)
        if (use.log.doses) {
            cint <- LD50 * c(1 / gsd, gsd)^v
        } else {
            cint <- mu + c(-sd, sd) * v
        }
        names(use.log.doses) <- "calculations done using the logs of the doses?"
        attr(cint, "conf.level") <- conf.level
        ### ---OUTPUT-----------------------------------------------------
        if (use.log.doses) {
            names(gsd) <- "geometric standard deviation of LD50 estimate"
            rval <- list(
                use.log.doses = use.log.doses,
                trim = trim,
                was.smoothed = needs.smooth,
                LD50 = LD50,
                gsd = gsd,
                conf.int = cint
            )
        } else {
            names(sd) <- "standard deviation of LD50 estimate"
            rval <- list(
                use.log.doses = use.log.doses,
                trim = trim,
                was.smoothed = needs.smooth,
                mu = mu,
                sd = sd,
                conf.int = cint
            )
        }
        class(rval) <- "tskresult"
        return(rval)
    }
#' @method print tskresult
#' @export
print.tskresult <- function(x, ...) {
    cat("\n")
    trimpercent <- x$trim * 100
    cat(
        "Trimmed Spearman-Karber method using",
        trimpercent, "percent trim\n\n"
    )
    if (x$was.smoothed) {
        cat("Data was smoothed")
    } else {
        cat("Data was not smoothed")
    }
    cat("\n")
    if (x$use.log.doses) {
        cat("Calculation done using the logs of the doses")
    } else {
        cat("Calculation done using the raw doses (no log transform) ")
    }
    cat("\n")
    cat("Estimated LD50: ")
    if (x$use.log.doses) {
        cat(x$LD50)
        cat("\tGSD of estimate:", x$gsd)
    } else {
        cat(x$mu)
        cat("\tSD of estimate:", x$sd)
    }
    cat("\n")
    cat(
        format(100 * attr(x$conf.int, "conf.level")),
        "percent confidence interval on LD50:\n",
        format(c(x$conf.int[1L], x$conf.int[2L])), "\n"
    )
    invisible(x)
}
