## # FILE: tests/testthat/test_data_Helper.R


#' Resolve the excel datasheet number issues
#'
#' The `simplifyTreatment` function is designed to handle and simplify
#' a treatment vector, which can be either a factor or a character vector.
#' The primary goal of this function is to resolve issues with numeric
#'  precision that often arise in Excel datasheets, such as numbers
#' being represented with excessive decimal places, like 12.300000000000001
#' or 8.3579999999999.
#'
#' @param trt a treatment vector, either factor or character
#'
#' @return a simplified vector, either a factor or a character vector,
#' depending on x
#' @export
#'
#' @examples
#' x <- structure(c(
#'   1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L,
#'   3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 1L, 1L, 1L,
#'   1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L,
#'   5L, 5L, 5L, 6L, 6L, 6L, 6L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L,
#'   2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 6L,
#'   6L
#' ), levels = c(
#'   "Control", "1", "3.51", "12.300000000000001",
#'   "42.9", "150"
#' ), class = "factor")
#' simplifyTreatment(x)
simplifyTreatment <- function(trt) {
  if (is.factor(trt)) {
    leltrt <- levels(trt)
  } else {
    if (is.character(trt)) leltrt <- unique(trt)
  }
  leltrtn <- suppressWarnings(as.numeric(leltrt))
  leltrtc <- ifelse(is.na(leltrtn), leltrt, leltrtn)
  trt <- plyr::mapvalues(trt, from = leltrt, to = leltrtc)
  return(trt)
}


#' Change from an expected wide format to long format
#'
#' @param widedat data in wide format
#' @param cnames logical,1 or 0, whether to use the 1st row as the column names,
#' 1 means 1st row need to be converted to headers
#' @param repnames whether the 1st column is the Replicates column
#'
#' @return a dataframe in long format
#' @keywords internal
#' @examples
#' # Create a sample wide dataset
#' widedat <- data.frame(
#'   Replicates = c("Rep1", "Rep2", "Rep3"),
#'   Treatment1 = c(1.1, 2.2, 3.3),
#'   Treatment2 = c(4.4, 5.5, 6.6)
#' )
#' drcHelper:::wide2long(widedat, cnames = FALSE)
wide2long <- function(widedat, cnames = 1, repnames = 1) {
  if (cnames == 1) {
    widedat1 <- widedat[-1, ]
    colnames(widedat1) <- widedat[1, ]
  } else {
    widedat1 <- widedat
  }
  if (repnames == 1) {
    longdat <- widedat1 |>
      tidyr::pivot_longer(cols = seq_len(ncol(widedat))[-1])
    names(longdat) <- c("Replicates", "Treatment", "Response")
  } else {
    longdat <- widedat1 |> tidyr::pivot_longer(cols = seq_len(ncol(widedat)))
    names(longdat) <- c("Treatment", "Response")
  }

  longdat$Response <- as.numeric(longdat$Response)
  longdat <- longdat |> dplyr::filter(!is.na(.data$Response))
  return(longdat)
}
