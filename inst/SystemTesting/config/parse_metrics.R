


parse_expected_metrics <- function(test_type, exp_tbl) {
  # Dose as numeric: from column or from description
  Dose_from_label <- ifelse(is.na(convert_dose(exp_tbl$Dose)),
                            dose_from_description(exp_tbl$`Brief description`),
                            convert_dose(exp_tbl$Dose))
  exp_tbl <- dplyr::mutate(exp_tbl,
                           Dose = Dose_from_label,
                           Expected_Value = convert_numeric(`expected result value`))

  pick <- function(pattern) exp_tbl[grepl(pattern, exp_tbl$`Brief description`, ignore.case = TRUE), c("Dose", "Expected_Value")]

  out <- list()
  if (test_type %in% c("dunnett", "williams", "dunn", "wilcoxon", "welch", "student_t")) {
    out$mean <- pick("\\bMean\\b"); names(out$mean)[2] <- "Expected_Mean"
  }
  if (test_type %in% c("dunnett", "welch", "student_t")) {
    out$t <- pick("T-value(?!\\s*\\(adjusted\\))|t-value(?!\\s*\\(adjusted\\))"); names(out$t)[2] <- "Expected_T"
    out$p <- pick("p-value"); names(out$p)[2] <- "Expected_P"
  }
  if (test_type == "williams") {
    out$mean   <- pick("\\bMean\\b");                             names(out$mean)[2] <- "Expected_Mean"
    out$pinhib <- pick("%Inhibition");                            names(out$pinhib)[2] <- "Expected_PercentInhibition"
    out$tadj   <- pick("T-value\\s*\\(adjusted\\)");              names(out$tadj)[2] <- "Expected_Tadj"
    out$tcrit  <- pick("\\bTcrit\\b");                            names(out$tcrit)[2] <- "Expected_Tcrit"
    out$signif <- pick("\\bsignificance\\b|\\bpsignificance\\b"); names(out$signif)[2] <- "Expected_Significance"
    out$df     <- pick("\\bdf\\b");                               names(out$df)[2] <- "Expected_df"
    out$mdd    <- pick("\\bMDD%\\b");                             names(out$mdd)[2] <- "Expected_MDDpct"
  }
  if (test_type == "dunn") {
    out$z <- pick("\\bz-value\\b"); names(out$z)[2] <- "Expected_z"
    out$p <- pick("p-value"); names(out$p)[2] <- "Expected_P"
    out$pinhib <- pick("%Inhibition"); names(out$pinhib)[2] <- "Expected_PercentInhibition"
  }
  if (test_type == "wilcoxon") {
    out$w <- pick("\\bW-Value\\b|\\bW-value\\b"); names(out$w)[2] <- "Expected_W"
    out$p <- pick("p-value"); names(out$p)[2] <- "Expected_P"
    out$pinhib <- pick("%Inhibition"); names(out$pinhib)[2] <- "Expected_PercentInhibition"
  }
  if (test_type == "fisher") {
    out$uncorr <- pick("\\bUncorrected\\b"); names(out$uncorr)[2] <- "Expected_Uncorrected"
    out$corr <- pick("\\bCorrected\\b"); names(out$corr)[2] <- "Expected_Corrected"
    out$p <- pick("p-value"); names(out$p)[2] <- "Expected_P"
    out$signif <- pick("\\bsignificance\\b|\\bpsignificance\\b"); names(out$signif)[2] <- "Expected_Significance"
  }
  out
}
