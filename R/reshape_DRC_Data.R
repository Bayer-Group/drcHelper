## unit testing in test_helper_functions.R ###



#' Change Treatment groups to numerical dose
#'
#' @param x treatment groups with numbers and "Control"
#'
#' @return numeric vector of dose
#' @export
#'
#' @examples treatment2dose(c("Control","0.1","1","10"))
treatment2dose <- function(x){
  dose <- suppressWarnings(as.numeric(as.character(x)))
  dose[is.na(dose)]<-0
  dose
}


#' Reshape the wide data to long data
#'
#' @param dat data table with Replicate columns and dose groups
#' @param replicate_col name of the replicate column or columns need to be retained as identifier columns.
#'
#' @return long format of the dat
#' @export
#'
#' @examples  reshape_drcData(collembola_juveniles) ## note collembola_juveniles is fake data.
reshape_drcData <- function(dat, replicate_col = "Replicates") {
  dat <- dat %>%
    tidyr::pivot_longer(cols = -all_of(replicate_col),
                 names_to = "Treatment",
                 values_to = "Response") %>%
    mutate(Dose = treatment2dose(.data$Treatment)) %>%
    filter(!is.na(.data$Response))

  return(dat)
}
