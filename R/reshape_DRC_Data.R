#' Change Treatment groups to numerical dose
#'
#' @param x treatment groups with numbers and "Control"
#'
#' @return numeric vector of dose
#' @export
#'
#' @examples
treatment2dose <- function(x){
  dose <- as.numeric(as.character(x))
  dose[is.na(dose)]<-0
  dose
}

## Example Data set: collembola_juveniles <- collembola_run1%>% filter(Endpoint=="Juveniles")%>% dplyr::select(-Endpoint)

#' Reshape the wide data to long data
#'
#' @param dat data table with Replicate columns and dose groups
#'
#' @return long format of the dat
#' @export
#'
#' @examples  reshape_drcData(collembola_juveniles)
reshape_drcData <- function(dat){
  dat <- dat%>%pivot_longer(-Replicates,names_to = "Treatment",values_to = "Response")%>%
    mutate(Dose=treatment2dose(Treatment))%>%filter(!is.na(Response))
  return(dat)
}
