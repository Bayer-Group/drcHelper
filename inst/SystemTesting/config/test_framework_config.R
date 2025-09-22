# Master Statistical Test Validation Framework Configuration
# =====================================================

# Define available statistical tests and their function groups
STATISTICAL_TESTS <- list(
  
  # Parametric tests
  "dunnett" = list(
    name = "Dunnett's Multiple Comparison Test",
    function_groups = c("FG00220", "FG00221", "FG00222", "FG00225"),
    test_function = "dunnett_test",
    alternatives = c("less", "greater", "two.sided"),
    key_metrics = c("T-value", "p-value", "Mean", "df"),
    implemented = TRUE
  ),
  
  "dunn" = list(
    name = "Dunn's Multiple Comparison Test", 
    function_groups = c("FG00250", "FG00251", "FG00252", "FG00255"),
    test_function = "dunn_test",  # To be implemented
    alternatives = c("less", "greater", "two.sided"),
    key_metrics = c("z-value", "p-value", "Mean", "df", "H-statistic"),
    implemented = FALSE
  ),
  
  "williams" = list(
    name = "Williams' Trend Test",
    function_groups = c("FG00210", "FG00215"),
    test_function = "williams_test",  # To be implemented
    alternatives = c("less", "greater"),
    key_metrics = c("T-value", "Tcrit", "Mean", "df", "%Inhibition"),
    implemented = FALSE
  ),
  
  "student_t" = list(
    name = "Student's t-Test",
    function_groups = c("FG00230", "FG00235"),
    test_function = "t_test",
    alternatives = c("less", "greater", "two.sided"),
    key_metrics = c("T-value", "p-value", "Mean", "df"),
    implemented = FALSE
  ),
  
  "welch" = list(
    name = "Welch's t-Test",
    function_groups = c("FG00240", "FG00241", "FG00242", "FG00245"),
    test_function = "welch_test",
    alternatives = c("less", "greater", "two.sided"),
    key_metrics = c("T-value", "p-value", "Mean", "df"),
    implemented = FALSE
  ),
  
  # Non-parametric tests
  "wilcoxon" = list(
    name = "Wilcoxon Rank Sum Test",
    function_groups = c("FG00260", "FG00261", "FG00262", "FG00265"),
    test_function = "wilcoxon_test",
    alternatives = c("less", "greater", "two.sided"),
    key_metrics = c("W-Value", "p-value", "Mean", "df"),
    implemented = FALSE
  ),
  
  "signed_rank" = list(
    name = "Wilcoxon Signed Rank Test",
    function_groups = c("FG00270", "FG00271", "FG00272", "FG00275"),
    test_function = "signed_rank_test",
    alternatives = c("two.sided"),
    key_metrics = c("t-Value", "Mean", "%Inhibition"),
    implemented = FALSE
  ),
  
  # Dose-response models
  "spearman_karber" = list(
    name = "Spearman-Karber Test",
    function_groups = c("FG00410"),
    test_function = "spearman_karber_test",
    alternatives = NULL,
    key_metrics = c("Log10 (LR50)", "SE Log10 (LR50)", "LR50"),
    implemented = FALSE  # Already have tsk function, but need validation wrapper
  ),
  
  "trimmed_spearman_karber" = list(
    name = "Trimmed Spearman-Karber Test", 
    function_groups = c("FG00420"),
    test_function = "tsk_test",
    alternatives = NULL,
    key_metrics = c("%Trim", "Log10 (LR50)", "SE Log10 (LR50)", "LR50"),
    implemented = TRUE  # Using existing tsk function
  ),
  
  # Fisher's exact test
  "fisher" = list(
    name = "Fisher's Exact Test",
    function_groups = c("FG00280"),
    test_function = "fisher_test",
    alternatives = c("less", "greater", "two.sided"),
    key_metrics = c("p-value", "Uncorrected", "Corrected"),
    implemented = FALSE
  ),
  
  # Model fitting tests
  "probit" = list(
    name = "Probit Analysis",
    function_groups = c("FG00430", "FG00435"),
    test_function = "probit_test",
    alternatives = NULL,
    key_metrics = c("Log10 (rate)", "Uncorrected", "Corrected", "Intercept", "Slope"),
    implemented = FALSE
  ),
  
  "logistic" = list(
    name = "Logistic Regression (LN2)",
    function_groups = c("FG00450", "FG00455"),
    test_function = "logistic_test",
    alternatives = NULL,
    key_metrics = c("Log10 (rate)", "Uncorrected", "Corrected", "Intercept", "Slope"),
    implemented = FALSE
  )
)

# Tolerance settings for different metric types
TOLERANCE_SETTINGS <- list(
  "T-value" = 1e-06,
  "t-value" = 1e-06,
  "z-value" = 1e-06, 
  "W-Value" = 1e-06,
  "H-statistic" = 1e-06,
  "F-value" = 1e-06,
  "p-value" = 1e-04,  # More lenient for p-values
  "Mean" = 1e-06,
  "df" = 1e-06,
  "Estimation" = 1e-06,
  "Standard deviation" = 1e-06,
  "Log10 (LR50)" = 1e-06,
  "LR50" = 1e-06,
  "%Inhibition" = 1e-04,
  "%Reduction" = 1e-04,
  "Uncorrected" = 1e-06,
  "Corrected" = 1e-06,
  "default" = 1e-06
)

# Alternative hypothesis mapping
ALT_MAPPING <- list(
  "smaller" = "less",
  "greater" = "greater", 
  "two-sided" = "two.sided"
)

# Common utility functions
convert_dose <- function(dose_str) {
  if(is.na(dose_str) || dose_str == "") return(NA)
  # Convert European decimal notation and handle various formats
  dose_clean <- gsub(",", ".", dose_str)
  dose_clean <- gsub("[^0-9.]", "", dose_clean)
  as.numeric(dose_clean)
}

get_tolerance <- function(metric_name) {
  for(pattern in names(TOLERANCE_SETTINGS)) {
    if(grepl(pattern, metric_name, ignore.case = TRUE)) {
      return(TOLERANCE_SETTINGS[[pattern]])
    }
  }
  return(TOLERANCE_SETTINGS[["default"]])
}

convert_alternative <- function(alt_description) {
  for(key in names(ALT_MAPPING)) {
    if(grepl(key, alt_description, ignore.case = TRUE)) {
      return(ALT_MAPPING[[key]])
    }
  }
  return("two.sided")  # Default
}

# Function to get available tests
get_available_tests <- function(implemented_only = FALSE) {
  if(implemented_only) {
    return(STATISTICAL_TESTS[sapply(STATISTICAL_TESTS, function(x) x$implemented)])
  }
  return(STATISTICAL_TESTS)
}

# Function to get function groups for a specific test
get_function_groups <- function(test_name) {
  if(test_name %in% names(STATISTICAL_TESTS)) {
    return(STATISTICAL_TESTS[[test_name]]$function_groups)
  }
  return(NULL)
}

# Function to identify test type from function group
identify_test_from_fg <- function(function_group) {
  for(test_name in names(STATISTICAL_TESTS)) {
    if(function_group %in% STATISTICAL_TESTS[[test_name]]$function_groups) {
      return(test_name)
    }
  }
  return(NULL)
}