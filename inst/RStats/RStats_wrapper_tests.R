#' Wrapper for LMM with Dunnett Test (Homoscedastic)
#'
#' @param data Data frame with columns: Response, Dose, Tank
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @return Standardized test results
lmm_dunnett_homo <- function(data, alpha = 0.05, alternative = "less") {
  # Ensure Dose is a factor
  data$Dose <- as.factor(data$Dose)

  # Run test
  test_result <- drcHelper::dunnett_test(
    data = data,
    response_var = "Response",
    dose_var = "Dose",
    block_var = "Tank",
    include_random_effect = TRUE,
    variance_structure = "homoscedastic",
    alpha = alpha,
    return_model = TRUE,
    alternative = alternative
  )

  # Extract results
  result <- list(
    method = "LMM Dunnett (Homoscedastic)",
    p_values = test_result$results_table$p.value,
    estimates = test_result$results_table$estimate,
    significant = test_result$results_table$significant,
    test_statistic = test_result$results_table$statistic,
    comparisons = test_result$results_table$comparison,
    model = test_result$model,
    additional_info = list(
      noec = test_result$noec,
      model_type = test_result$model_type
    )
  )

  return(result)
}

#' Wrapper for LMM with Dunnett Test (Heteroscedastic)
#'
#' @param data Data frame with columns: Response, Dose, Tank
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @return Standardized test results
lmm_dunnett_hetero <- function(data, alpha = 0.05, alternative = "less") {
  # Ensure Dose is a factor
  data$Dose <- as.factor(data$Dose)

  # Run test
  test_result <- drcHelper::dunnett_test(
    data = data,
    response_var = "Response",
    dose_var = "Dose",
    block_var = "Tank",
    include_random_effect = TRUE,
    variance_structure = "heteroscedastic",
    alpha = alpha,
    return_model = TRUE,
    alternative = alternative
  )

  # Extract results
  result <- list(
    method = "LMM Dunnett (Heteroscedastic)",
    p_values = test_result$results_table$p.value,
    estimates = test_result$results_table$estimate,
    significant = test_result$results_table$significant,
    test_statistic = test_result$results_table$statistic,
    comparisons = test_result$results_table$comparison,
    model = test_result$model,
    additional_info = list(
      noec = test_result$noec,
      model_type = test_result$model_type
    )
  )

  return(result)
}

#' Wrapper for LM with Dunnett Test on aggregated data
#'
#' @param data Data frame with columns: Response, Dose, Tank
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @param notAgged Whether the input data is already aggregated to tank level data
#' @return Standardized test results
lm_dunnett_agg <- function(data, alpha = 0.05, alternative = "less", notAgged = TRUE) {
  # Aggregate data by tank
  if(notAgged) agg_data <- stats::aggregate(Response ~ Dose + Tank, data = data, FUN = mean) else{
    agg_data <- data
  }

  # Ensure Dose is a factor
  agg_data$Dose <- as.factor(agg_data$Dose)

  # Run test
  test_result <- drcHelper::dunnett_test(
    data = agg_data,
    response_var = "Response",
    dose_var = "Dose",
    include_random_effect = FALSE,
    variance_structure = "homoscedastic",
    alpha = alpha,
    return_model = TRUE,
    alternative = alternative
  )

  # Extract results
  result <- list(
    method = "LM Dunnett (Aggregated)",
    p_values = test_result$results_table$p.value,
    estimates = test_result$results_table$estimate,
    significant = test_result$results_table$significant,
    test_statistic = test_result$results_table$statistic,
    comparisons = test_result$results_table$comparison,
    model = test_result$model,
    additional_info = list(
      noec = test_result$noec,
      model_type = test_result$model_type
    )
  )

  return(result)
}

#' Wrapper for GLS with Dunnett Test on aggregated data
#'
#' @param data Data frame with columns: Response, Dose, Tank
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @param notAgged Whether the input data is already aggregated to tank level data
#' @return Standardized test results
gls_dunnett_agg <- function(data, alpha = 0.05, alternative = "less",notAgged = TRUE) {
  # Aggregate data by tank
  if(notAgged) agg_data <- stats::aggregate(Response ~ Dose + Tank, data = data, FUN = mean) else{
    agg_data <- data
  }

  # Ensure Dose is a factor
  agg_data$Dose <- as.factor(agg_data$Dose)

  # Run test
  test_result <- drcHelper::dunnett_test(
    data = agg_data,
    response_var = "Response",
    dose_var = "Dose",
    include_random_effect = FALSE,
    variance_structure = "heteroscedastic",
    alpha = alpha,
    return_model = TRUE,
    alternative = alternative
  )

  # Extract results
  result <- list(
    method = "GLS Dunnett (Aggregated)",
    p_values = test_result$results_table$p.value,
    estimates = test_result$results_table$estimate,
    significant = test_result$results_table$significant,
    test_statistic = test_result$results_table$statistic,
    comparisons = test_result$results_table$comparison,
    model = test_result$model,
    additional_info = list(
      noec = test_result$noec,
      model_type = test_result$model_type
    )
  )

  return(result)
}

#' Wrapper for Williams Test
#'
#' @param data Data frame with columns: Response, Dose, Tank
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @return Standardized test results
williams_test <- function(data, alpha = 0.05, alternative = "less") {
  # Aggregate data by tank
  agg_data <- stats::aggregate(Response ~ Dose + Tank, data = data, FUN = mean)

  # Ensure Dose is a factor
  agg_data$Dose <- as.factor(agg_data$Dose)

  # Run Williams test
  # Note: williamsTest doesn't return p-values, only decisions
  ## For williams' test there is only 0.05
  test_result <- PMCMRplus::williamsTest(
    Response ~ Dose,
    data = agg_data,
    alternative = alternative
  )

  # Extract test statistics and critical values
  test_summary <- summaryZG(test_result)

  # Calculate pseudo p-values based on test statistics and critical values
  # For Williams test, we'll use the ratio of t-value to t-critical as a proxy
  # If t-value > t-critical, reject H0; otherwise accept H0
  t_values <- test_summary$`t'-value`
  t_crits <- test_summary$`t'-crit`

  # Calculate pseudo p-values
  # If t-value > t-crit for "greater" (or t-value < -t-crit for "less"), p < alpha
  # It is proposed to use the ratio to approximate the p-value, but I don't see how

  pseudo_p_values <- ifelse(test_summary$decision == "reject", alpha-0.005,alpha+0.005)

  # Extract dose levels from test result
  comparisons <- rownames(test_summary)

  # Extract estimates (differences from control)
  # We need to calculate these from the data
  control_level <- levels(agg_data$Dose)[1]
  control_mean <- mean(agg_data$Response[agg_data$Dose == control_level])

  estimates <- sapply(levels(agg_data$Dose)[-1], function(dose) {
    mean(agg_data$Response[agg_data$Dose == dose]) - control_mean
  })

  # Create result list
  result <- list(
    method = "Williams Test",
    p_values = pseudo_p_values,
    estimates = estimates,
    significant = test_summary$decision == "reject",
    test_statistic = t_values,
    comparisons = comparisons,
    model = test_result,
    additional_info = list(
      p_value = "pseudo p_values are given for purpose of power analysis, do not use them",
      note = "It is possible to obtain p-value by interpolation."
    )
  )

  return(result)
}

#' Wrapper for Stepdown Jonckheere-Terpstra Test
#'
#' @param data Data frame with columns: Response, Dose, Tank
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @return Standardized test results
jonckheere_test <- function(data, alpha = 0.05, alternative = "less") {
  # Aggregate data by tank
  agg_data <- stats::aggregate(Response ~ Dose + Tank, data = data, FUN = mean)

  # Ensure Dose is a factor
  agg_data$Dose <- as.factor(agg_data$Dose)

  # Run test
  test_result <- PMCMRplus::stepDownTrendTest(
    Response ~ Dose,
    data = agg_data,
    test = "jonckheereTest",
    alternative = alternative
  )

  # Extract p-values and other information
  test_summary <- summary(test_result)
  p_values <- as.numeric(test_summary$p.value)
  comparisons <- getComparison(test_result)
  # Calculate estimates (differences from control)
  control_level <- levels(agg_data$Dose)[1]
  control_mean <- mean(agg_data$Response[agg_data$Dose == control_level])

  estimates <- sapply(levels(agg_data$Dose)[-1], function(dose) {
    mean(agg_data$Response[agg_data$Dose == dose]) - control_mean
  })

  # Create result list
  result <- list(
    method = "Jonckheere-Terpstra Test",
    p_values = p_values,
    estimates = estimates,
    significant = p_values < alpha,
    test_statistic = as.numeric(test_summary$statistic),
    comparisons = comparisons,
    model = test_result,
    additional_info = list(
      adjusted_method = test_summary$method
    )
  )

  return(result)
}

#' Wrapper for Dunn's Test
#'
#' @param data Data frame with columns: Response, Dose, Tank
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @return Standardized test results
manyone_dunns_test <- function(data, alpha = 0.05, alternative = "less",p.adjust.method = "holm") {
  # Aggregate data by tank
  agg_data <- stats::aggregate(Response ~ Dose + Tank, data = data, FUN = mean)

  # Ensure Dose is a factor
  agg_data$Dose <- as.factor(agg_data$Dose)

  # Run test
  test_result <- PMCMRplus::kwManyOneDunnTest(
    Response ~ Dose,
    data = agg_data,
    alternative = alternative,
    p.adjust.method = p.adjust.method
  )

  # Extract p-values and other information
  test_summary <- summary(test_result)
  p_values <- as.numeric(test_summary$p.value)
  comparisons <- getComparison(test_result,test="many-to-one")
  # Calculate estimates (differences from control)
  control_level <- levels(agg_data$Dose)[1]
  control_mean <- mean(agg_data$Response[agg_data$Dose == control_level])

  estimates <- sapply(levels(agg_data$Dose)[-1], function(dose) {
    mean(agg_data$Response[agg_data$Dose == dose]) - control_mean
  })

  # Create result list
  result <- list(
    method = "Dunn's Test",
    p_values = p_values,
    estimates = estimates,
    significant = p_values < alpha,
    test_statistic = as.numeric(test_summary$statistic),
    comparisons = comparisons,
    model = test_result,
    additional_info = list(
      adjusted_method = test_summary$p.adjust.method
    )
  )

  return(result)
}

#' Wrapper for Wilcoxon Test
#'
#' @param data Data frame with columns: Response, Dose, Tank
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @return Standardized test results
manyone_wilcox_test <- function(data, alpha = 0.05, alternative = "less",p.adjust.method = "holm") {
  # Aggregate data by tank
  agg_data <- stats::aggregate(Response ~ Dose + Tank, data = data, FUN = mean)

  # Ensure Dose is a factor
  agg_data$Dose <- as.factor(agg_data$Dose)

  # Get control level (first level)
  control_level <- levels(agg_data$Dose)[1]

  # Run test
  test_result <- rstatix::wilcox_test(
    data = agg_data,
    formula = Response ~ Dose,
    ref.group = control_level,
    alternative = alternative,
    p.adjust.method = p.adjust.method
  )

  # Extract results
  p_values <- test_result$p

  # Calculate estimates (differences from control)
  control_mean <- mean(agg_data$Response[agg_data$Dose == control_level])

  estimates <- sapply(levels(agg_data$Dose)[-1], function(dose) {
    mean(agg_data$Response[agg_data$Dose == dose]) - control_mean
  })

  # Create result list
  result <- list(
    method = "Wilcoxon Test",
    p_values = p_values,
    estimates = estimates,
    significant = p_values < alpha,
    test_statistic = test_result$statistic,
    comparisons = paste(test_result$group2, "-", test_result$group1),
    model = test_result,
    additional_info = list(
      method = "Wilcoxon rank sum test",
      p.adjust.method = p.adjust.method
    )
  )

  return(result)
}






#' Wrapper for LM with Dunnett Test on aggregated data
#'
#' @param data Data frame with columns: Response, Dose, Tank
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @param notAgged Whether the input data is already aggregated to tank level data
#' @return Standardized test results
lm_dunnett_agg_simple <- function(data, alpha = 0.05, alternative = "less") {
  # Aggregate data by tank
  #if(notAgged) agg_data <- stats::aggregate(Response ~ Dose + Tank, data = data, FUN = mean) else{
    agg_data <- data
  #}

  # Ensure Dose is a factor
  agg_data$Dose <- as.factor(agg_data$Dose)

  # Run test
  test_result <- drcHelper::dunnett_test(
    data = agg_data,
    response_var = "Response",
    dose_var = "Dose",
    include_random_effect = FALSE,
    variance_structure = "homoscedastic",
    alpha = alpha,
    return_model = TRUE,
    alternative = alternative
  )

  # Extract results
  result <- list(
    method = "LM Dunnett (Aggregated)",
    p_values = test_result$results_table$p.value,
    estimates = test_result$results_table$estimate,
    significant = test_result$results_table$significant,
    test_statistic = test_result$results_table$statistic,
    comparisons = test_result$results_table$comparison,
    model = test_result$model,
    additional_info = list(
      noec = test_result$noec,
      model_type = test_result$model_type
    )
  )

  return(result)
}







#' Wrapper for GLS with Dunnett Test on aggregated data
#'
#' @param data Data frame with columns: Response, Dose, Tank
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @param notAgged Whether the input data is already aggregated to tank level data
#' @return Standardized test results
gls_dunnett_agg_simple <- function(data, alpha = 0.05, alternative = "less") {
  # Aggregate data by tank
 #if(notAgged) agg_data <- stats::aggregate(Response ~ Dose + Tank, data = data, FUN = mean) else{
    agg_data <- data
  #}

  # Ensure Dose is a factor
  agg_data$Dose <- as.factor(agg_data$Dose)

  # Run test
  test_result <- drcHelper::dunnett_test(
    data = agg_data,
    response_var = "Response",
    dose_var = "Dose",
    include_random_effect = FALSE,
    variance_structure = "heteroscedastic",
    alpha = alpha,
    return_model = TRUE,
    alternative = alternative
  )

  # Extract results
  result <- list(
    method = "GLS Dunnett (Aggregated)",
    p_values = test_result$results_table$p.value,
    estimates = test_result$results_table$estimate,
    significant = test_result$results_table$significant,
    test_statistic = test_result$results_table$statistic,
    comparisons = test_result$results_table$comparison,
    model = test_result$model,
    additional_info = list(
      noec = test_result$noec,
      model_type = test_result$model_type
    )
  )

  return(result)
}

