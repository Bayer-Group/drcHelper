#' Calculate MDD% for a Williams Test Result
#'
#' @param williams_obj The tibble result from broom_williams.
#' @param data The original dataframe used for the test.
#' @param formula The formula used for the test, e.g., Response ~ Dose.
#' @return A tibble with Dose, MDD, and MDD_pct.
#' @export
compute_mdd_williams <- function(williams_obj, data, formula) {

  if (!inherits(williams_obj, "tbl_df") || !all(c("comparison", "t'-crit") %in% names(williams_obj))) {
    stop("williams_obj must be the result from a broom_williams call.")
  }
  if (nrow(williams_obj) == 0) return(tibble::tibble(Dose=numeric(), MDD_pct=numeric()))

  # 1. Extract variable names
  resp_name <- all.vars(formula)[1]
  dose_name <- all.vars(formula)[2] # Correctly gets "Dose"

  # --- START OF FIX ---
  # Create a local copy of the data to avoid modifying the original
  local_data <- data

  # Ensure the dose column is a factor with control (0) as the first level.
  # This makes the function robust and independent of the input data's column type.
  dose_values <- local_data[[dose_name]]
  factor_levels <- sort(unique(dose_values))
  local_data$dose_factor_col <- factor(dose_values, levels = factor_levels)

  # Define the new, reliable factor column name and update formula for aov()
  factor_col_name <- "dose_factor_col"
  aov_formula <- as.formula(paste(resp_name, "~", factor_col_name))
  # --- END OF FIX ---

  # 2. Extract info from the Williams test object
  Tcrit <- williams_obj[["t'-crit"]]
  doses <- as.numeric(gsub(" - 0.*", "", williams_obj$comparison))

  # 3. Get ANOVA stats (MSE) using the corrected formula and local data
  aov_fit <- stats::aov(aov_formula, data = local_data)
  mse <- summary(aov_fit)[[1]]["Residuals", "Mean Sq"]

  # 4. Get control group stats using the reliable factor column
  control_level <- levels(local_data[[factor_col_name]])[1]
  ctrl_data <- local_data[local_data[[factor_col_name]] == control_level, ]
  mu_c <- mean(ctrl_data[[resp_name]], na.rm = TRUE)
  n_c <- nrow(ctrl_data)

  # 5. Get treatment sample sizes
  n_t <- sapply(doses, function(d) sum(local_data[[dose_name]] == d))

  # 6. Calculate MDD and MDD%
  SE_diff <- sqrt(mse * (1 / n_c + 1 / n_t))
  MDD <- Tcrit * SE_diff
  MDD_pct <- 100 * MDD / abs(mu_c)

  tibble::tibble(Dose = doses, MDD_pct = MDD_pct)
}
