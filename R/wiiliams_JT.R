## Internal helper functions
#'@export
getComparison <- function(object,test=c("trend","many-to-one")){
  cnam <- colnames(object$p.value)
  rnam <- rownames(object$p.value)
  grp2 <- as.numeric(c(row(object$p.value)))
  test <- match.arg(test)
  if(test=="trend"){
    if (object$alternative == "less") {
      H0 <- sapply(grp2, function(i) {
        if (i == 1) {
          paste0(rnam[i], " >= ", cnam[1])
        }
        else if (i == 2) {
          paste0(rnam[i], " >= ", rnam[i - 1], " >= ",
                 cnam[1])
        }
        else {
          paste0(rnam[i], " >= ", rnam[i - 1], " >= ... >= ",
                 cnam[1])
        }
      })
    }
    else if (object$alternative == "greater") {
      H0 <- sapply(grp2, function(i) {
        if (i == 1) {
          paste0(rnam[i], " <= ", cnam[1])
        }
        else if (i == 2) {
          paste0(rnam[i], " <= ", rnam[i - 1], " <= ",
                 cnam[1])
        }
        else {
          paste0(rnam[i], " <= ", rnam[i - 1], " <= ... <= ",
                 cnam[1])
        }
      })
    }
    else {
      H0 <- sapply(grp2, function(i) {
        if (i == 1) {
          paste0(rnam[i], " == ", cnam[1])
        }
        else if (i == 2) {
          paste0(rnam[i], " == ", rnam[i - 1], " == ",
                 cnam[1])
        }
        else {
          paste0(rnam[i], " == ", rnam[i - 1], " == ... == ",
                 cnam[1])
        }
      })
    }
  }else{
    grp1 <- as.numeric(c(col(object$p.value)))
    if (!is.null(object$alternative)) {
      if (object$alternative == "less") {
        H0 <- paste(rnam[grp2], "-", cnam[grp1], ">=", "0")
        ## PVAL <- paste("Pr(<", STAT, ")", sep = "")
      }
      else if (object$alternative == "greater") {
        H0 <- paste(rnam[grp2], "-", cnam[grp1], "<=", "0")
        ## PVAL <- paste("Pr(>", STAT, ")", sep = "")
      }
      else {
        H0 <- paste(rnam[grp2], "-", cnam[grp1], "==", "0")
        ## PVAL <- paste("Pr(>|", STAT, "|)", sep = "")
      }
    }
    else {
      H0 <- paste(rnam[grp2], "-", cnam[grp1], "==", "0")
      ## PVAL <- paste("Pr(>|", STAT, "|)", sep = "")
    }
  }

  return(H0)
}
