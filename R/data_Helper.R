#' Resolve the excel datasheet number issue like 12.300000000000001 or 8.3579999999999
#'
#' @param trt a treatment vector, either factor or character
#'
#' @return a simplified vector
#' @export
#'
#' @examples
#' x <- structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L,
#' 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 1L, 1L, 1L,
#' 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L,
#' 5L, 5L, 5L, 6L, 6L, 6L, 6L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L,
#' 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 6L,
#' 6L), levels = c("Control", "1", "3.51", "12.300000000000001",
#'              "42.9", "150"), class = "factor")
#' simplifyTreatment(x)

simplifyTreatment <- function(trt){
  if(is.factor(trt)) leltrt <- levels(trt) else{
    if(is.character(trt)) leltrt <- unique(trt)
  }
  leltrtn <- suppressWarnings(as.numeric(leltrt))
  leltrtc <- ifelse(is.na(leltrtn),leltrt,leltrtn)
  trt <- plyr::mapvalues(trt,from=leltrt,to=leltrtc)
  return(trt)
}


#' change from wide format to long format
#'
#' @param widedat
#' @param cnames
#'
#' @return
#'
#' @examples
wide2long <- function(widedat,cnames=1,repnames=1){
  if(cnames==1) {
    widedat1 <- widedat[-1,]
    colnames(widedat1) <- widedat[1,]
  }else{
    widedat1 <- widedat
  }
  if(repnames==1) {
    longdat <- widedat1 |> pivot_longer(cols=2:ncol(widedat))
    names(longdat) <- c("Replicates","Treatment","Response")
  }else{
    longdat <- widedat1 |> pivot_longer(cols=1:ncol(widedat))
    names(longdat) <- c("Treatment","Response")
  }

  longdat$Response <- as.numeric(longdat$Response)
  longdat <- longdat |> dplyr::filter(!is.na(Response))
  return(longdat)
}
