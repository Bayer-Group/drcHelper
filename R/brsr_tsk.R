#' This is hamilton data
#' 
#' Example dose-response data given in Hamilton (1977).
#' Note that, as per Hamilton (1978), the confidence intervals
#' given in Hamilton (1977) for these data sets are incorrect.
#' @format A list containing ten data frames: dr1a, dr1b, dr1c,
#' dr1d, dr1e, dr4a, dr4b, dr4c, dr4d, dr4e,
#' @docType data
#' @source Hamilton, 1977.
#' @references \url{data_blah.com}
#' @keywords data
"hamilton"


#' Trimmed Spearman-Karber Method by brsr
#'
#' @param ... inputs
#' @return tsk estimations
#' @export
#' @references \url{https://github.com/brsr/tsk}
#'
tsk <- function(...) UseMethod("tsk")

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
#' @S3method
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

#' @S3method
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
#' @S3method
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
