test_registry <- list()

test_registry$williams <- list(
  keyword = "Williams",
  discover_fgs = function(res, data) build_generic_fgs(res, data, pattern = "Williams"),
  run_actual = function(endpoint_data, alternative) {
    ed <- endpoint_data
    ed$Dose_numeric <- convert_dose(ed$Dose)
    ed <- ed[!is.na(ed$Dose_numeric), ]
    # Control first (lowest dose)
    dose_levels <- sort(unique(ed$Dose_numeric))
    ed$Dose_factor <- factor(ed$Dose_numeric, levels = dose_levels)

    direction <- if (tolower(alternative) %in% c("smaller", "less")) "decreasing" else "increasing"

    # Primary: PMCMRplus
    bw <- try(drcHelper::broom_williams(Response ~ Dose_factor, data = ed,
                                        method = "Williams_PMCMRplus",
                                        direction = direction), silent = TRUE)

    used_method <- "PMCMRplus"
    if (inherits(bw, "try-error") || nrow(bw) == 0) {
      # Fallback: JG
      bw <- drcHelper::broom_williams(Response ~ Dose_factor, data = ed,
                                      method = "Williams_JG",
                                      direction = direction)
      used_method <- "JG"
    }

    actual_df <- as.data.frame(bw)

    if (used_method == "PMCMRplus") {
      # comparison like "0.0448 - 0  <= 0" or "... >= 0"
      comp_clean <- gsub("\\s*(<=|>=)\\s*0\\s*$", "", as.character(actual_df$comparison))
      actual_df$Dose <- dose_from_comparison(comp_clean)
    } else {
      # JG comparisons can miss doses; assign from treatment doses excluding control
      trt_doses <- dose_levels[-1]
      if (length(trt_doses) == nrow(actual_df)) {
        # PMCMRplus tends to list in ascending dose; JG may differ. Use ascending as default.
        actual_df$Dose <- trt_doses
      } else {
        actual_df$Dose <- NA_real_
        warning("Williams_JG: could not safely map Dose to rows; leaving Dose as NA.")
      }
    }

    # Normalize column names
    cn <- names(actual_df)
    names(actual_df)[cn == "`t'-stat"] <- "Actual_Tadj"
    names(actual_df)[cn == "`t'-crit"] <- "Actual_Tcrit"
    names(actual_df)[cn == "estimate"] <- "Actual_Diff"
    names(actual_df)[cn == "decision"] <- "Actual_Significance"  # "accept"/"reject"
    rownames(actual_df) <- NULL

    # Group means and %Inhibition
    group_means <- ed %>%
      dplyr::group_by(Dose = Dose_numeric) %>%
      dplyr::summarise(Actual_Mean = mean(Response, na.rm = TRUE), .groups = "drop")

    ctrl_mean <- group_means$Actual_Mean[group_means$Dose == min(group_means$Dose)]
    group_means$Actual_PercentInhibition <- if (!is.na(ctrl_mean) && ctrl_mean != 0) {
      100 * (1 - group_means$Actual_Mean / ctrl_mean)  # #$#%Inhibition = 100 * (1 - mean_treatment / mean_control)#$#
    } else NA_real_

    list(actual_df = actual_df, group_means = group_means)
  },
  metrics = c("Mean", "%Inhibition", "T-value (adjusted)", "Tcrit", "significance", "df", "MDD%"),
  comparator = "trend"
)
