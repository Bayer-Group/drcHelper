#' Visualize Expected Response Patterns
#'
#' @param n_doses Number of dose levels
#' @param dose_range Vector specifying min and max dose values
#' @param max_effect Maximum effect size
#' @param threshold_idx Index at which effects begin (for threshold response)
#' @return ggplot object showing response patterns
visualize_response_patterns <- function(n_doses = 5,
                                        dose_range = c(0, 20),
                                        max_effect = 20,
                                        threshold_idx = 3) {

  # Calculate doses
  doses <- seq(dose_range[1], dose_range[2], length.out = n_doses)

  # Define response functions
  decreasing_fn <- function(dose) {
    base_response <- 100
    max_dose <- max(doses)
    return(base_response - (dose / max_dose) * max_effect)
  }

  non_monotonic_fn <- function(dose) {
    base_response <- 100
    mid_dose <- mean(dose_range)
    return(base_response - max_effect * (1 - ((dose - mid_dose)/(mid_dose))^2))
  }

  threshold_fn <- function(dose) {
    base_response <- 100
    result <- rep(base_response, length(dose))
    threshold_dose <- doses[threshold_idx]
    high_doses <- dose >= threshold_dose
    if (any(high_doses)) {
      max_high_dose <- max(doses)
      relative_position <- (dose[high_doses] - threshold_dose) / (max_high_dose - threshold_dose)
      result[high_doses] <- base_response - relative_position * max_effect
    }
    return(result)
  }

  none_fn <- function(dose) {
    rep(100, length(dose))
  }

  oscilate_fun <- function(dose){
    oscillating_response(dose, max_effect = max_effect, frequency = 1, baseline = 100,dose_range = dose_range)
  }
  ##browser()
  # Create data frame for plotting
  plot_data <- data.frame(
    Dose = rep(doses, 5),
    Response = c(
      decreasing_fn(doses),
      non_monotonic_fn(doses),
      threshold_fn(doses),
      oscilate_fun(doses),
      none_fn(doses)
    ),
    Pattern = rep(c("Decreasing", "Non-monotonic", "Threshold", "Oscillating", "No Effect"), each = n_doses)
  )

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Dose, y = Response, color = Pattern, group = Pattern)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Expected Response Patterns",
      subtitle = paste("Max Effect:", max_effect, "%| Threshold at Dose:", doses[threshold_idx]),
      x = "Dose",
      y = "Response"
    )

  return(p)
}


# Visualize different response patterns
response_patterns <- visualize_response_patterns(
  n_doses = 5,
  dose_range = c(0, 20),
  max_effect = 20,
  threshold_idx = 3
)

# Display the plot
response_patterns






# Example of using the oscillating response function directly
doses <- seq(0, 20, length.out = 5)
freq <- 1
responses <- oscillating_response(doses, max_effect = 20, frequency = freq)

# Create a data frame to visualize
example_data <- data.frame(
  Dose = doses,
  Response = responses
)

# Plot the oscillating pattern
ggplot2::ggplot(example_data, ggplot2::aes(x = Dose, y = Response)) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::geom_point(size = 3) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Oscillating Response Pattern",
    subtitle = paste("Max Effect: 20 | Frequency:", freq),
    x = "Dose",
    y = "Response"
  )
