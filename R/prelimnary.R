## Functions for preliminary assessment of dose response data

#' Preliminary Plot 1 for Dose Response Data
#'
#' This function generates a scatter plot of response against nominal dose.
#'
#' @param testdata A data frame containing the dose and response data.
#' @param ylab A string for the y-axis label. Default is "Response".
#' @param xlab A string for the x-axis label. Default is `"Test Concentration [nominal, mg a.s./L]"`.
#' @param title A string for the plot title. Default is "Measured Variable".
#' @param dose_col name of the dose column, default being "Dose".
#' @param response_col name of the response column.
#' @return A ggplot object.
#' @import ggplot2
#' @export
prelimPlot1 <- function(testdata, dose_col = "Dose", response_col = "Response",
                        ylab = "Response", xlab = "Test Concentration [nominal, mg a.s./L]",
                        title = "Measured Variable") {
  # Convert the dose column to a factor if it's numeric
  if (is.numeric(testdata[[dose_col]])) {
    testdata[[dose_col]] <- factor(testdata[[dose_col]],
                                   levels = sort(unique(testdata[[dose_col]])))
  }

  # Create the plot using ggplot2 with .data pronoun
  p <- ggplot(testdata, aes(x = .data[[dose_col]], y = .data[[response_col]])) +
    geom_point() +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title)

  return(p)
}

## Another version of prelimPlot1.0, keeping it here for references of not using strings.
# prelimPlot1.0 <- function(testdata, dose_col = Dose, response_col = Response,
#                         ylab = "Response", xlab = "Test Concentration [nominal, mg a.s./L]",
#                         title = "Measured Variable") {
#   # Convert the dose column to a factor if it's numeric
#   if (is.numeric(testdata[[deparse(substitute(dose_col))]])) {
#     testdata[[deparse(substitute(dose_col))]] <- factor(testdata[[deparse(substitute(dose_col))]],
#                                                         levels = sort(unique(testdata[[deparse(substitute(dose_col))]])))
#   }
#
#   # Create the plot using ggplot2
#   p <- ggplot(testdata, aes(x = {{ dose_col }}, y = {{ response_col }})) +
#     geom_point() +
#     ylab(ylab) +
#     xlab(xlab) +
#     ggtitle(title)
#
#   return(p)
# }

#' Preliminary Plot 2 for Dose Response Data, with x continuous.
#'
#' This function generates a scatter plot of response against dose on a log1p scale.
#'
#' @param testdata A data frame containing the dose and response data.
#' @param ylab A string for the y-axis label. Default is "Response".
#' @param xlab A string for the x-axis label. Default is `"Test Concentration [nominal, mg a.s./L]"`.
#' @param title A string for the plot title. Default is "Measured Variable".
#' @param dose_col name of the dose column, default being "Dose".
#' @param response_col name of the response column.
#' @return A ggplot object.
#' @import ggplot2
#' @import scales
#' @export
prelimPlot2 <- function(testdata, ylab = "Response", xlab = "Test Concentration [nominal, mg a.s./L]",
                        title = "Measured Variable", dose_col = "Dose",response_col="Response") {

  ilog1p <- function(x) {
    exp(x) - 1
  }
  testdata[[dose_col]] <- as.numeric(as.character(testdata[[dose_col]]))
  # Get unique doses and sort them
  doses <- sort(unique(testdata[[dose_col]]))

  # Create the plot using ggplot2 with .data pronoun
  p <- ggplot(testdata, aes(x = .data[[dose_col]], y = .data[[response_col]])) +
    geom_point() +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_x_continuous(breaks = doses, trans = scales::trans_new("log1p", log1p, ilog1p)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  return(p)
}


#' Preliminary Plot 3 for Dose Response Data
#'
#' This function generates a scatter plot of response against nominal dose.
#'
#' @param testdata A data frame containing the dose and response data.
#' @param ylab A string for the y-axis label. Default is "Response".
#' @param xlab A string for the x-axis label. Default is `"Test Concentration [nominal, mg a.s./L]"`.
#' @param title A string for the plot title. Default is "Measured Variable".
#' @param a the quantile for corresponding CI for mean. default is qnorm(0.975).
#' @param dose_col name of the dose column, default being "Dose".
#' @param response_col name of the response column.
#' @return A ggplot object.
#' @import ggplot2
#' @import dplyr
#' @keywords PrelimnaryAssessments
#' @export
prelimPlot3 <- function(testdata, ylab = "Response", xlab = "Test Concentration [nominal, mg a.s./L]",
                        title = "Measured Variable", a = 1.96, dose_col = "Dose", response_col = "Response") {

  # Convert the dose column to a factor if it's numeric
  if (is.numeric(testdata[[dose_col]])) {
    testdata[[dose_col]] <- factor(testdata[[dose_col]],
                                   levels = sort(unique(testdata[[dose_col]])))
  }

  # Create the initial plot
  p <- prelimPlot1(testdata = testdata, dose_col = dose_col, response_col=response_col,
                   ylab = ylab, xlab = xlab, title = title)

  # Summarize the data
  datsum <- testdata %>%
    group_by(.data[[dose_col]]) %>%
    summarise(mean = mean(.data[[response_col]]),
              SE = sd(.data[[response_col]]) / sqrt(length(.data[[response_col]]))) %>%
    mutate(Lower = .data$mean - a * .data$SE, Upper = .data$mean + a * .data$SE)

  # Add points and error bars to the plot
  p <- p +
    geom_point(data = datsum,
               aes(x = as.numeric(as.factor(.data[[dose_col]])) + 0.15,
                   y = mean),
               position = position_dodge(width = 0.9),
               col = "red", size = 3, shape = 24, fill = "pink") +
    geom_errorbar(data = datsum,
                  aes(x = as.numeric(as.factor(.data[[dose_col]])) + 0.15,
                      y = mean, ymin = .data$Lower, ymax = .data$Upper),
                  col = "red", position = position_dodge(width = 0.9), width = .1)

  return(p)
}


#' Preliminary Summary of Dose Response Data
#'
#' This function calculates the mean response, standard deviation, percent inhibition, and coefficient of variation for each dose level.
#'
#' @param testdata A data frame containing the dose and response data.
#' @param dose_col name of the dose column, default being "Dose".
#' @param response_col name of the response column.
#' @return A data frame summarizing the mean, standard deviation, percent inhibition, and coefficient of variation for each dose.
#' @import dplyr
#' @keywords PrelimnaryAssessments
#' @export
#' @examples
#' # Sample data frame
#' testdata <- data.frame(Dose = c(0, 1, 2, 3, 0, 1, 2, 3),
#'                       Response = c(10, 20, 30, 40, 12, 22, 32, 42))
#'
#' # Create the summary
#' summary_result <- prelimSummary(testdata, dose_col = "Dose", response_col = "Response")
#' print(summary_result)
prelimSummary <- function(testdata, dose_col = "Dose", response_col = "Response") {
  # Filter control group (Dose == 0)
  ctr <- testdata %>% filter(.data[[dose_col]] == 0)
  ctr0 <- mean(ctr[[response_col]], na.rm = TRUE)  # Calculate mean response for control group

  # Summarize the data
  sres <- testdata %>%
    group_by(.data[[dose_col]]) %>%
    summarise(Mean = mean(.data[[response_col]], na.rm = TRUE),
              SD = sd(.data[[response_col]], na.rm = TRUE)) %>%
    mutate(`% Inhibition` = -((.data$Mean - ctr0) / ctr0) * 100,
           CV = .data$SD / .data$Mean * 100)

  return(sres)
}
