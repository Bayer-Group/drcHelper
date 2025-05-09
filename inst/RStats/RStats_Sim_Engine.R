


run_multiple_simulations_with_logging <- function(param_grid, test_methods, n_sim = 1000,
                                                  n_cores = parallel::detectCores() - 1,
                                                  seed = 123,
                                                  log_file = "simulation_log.txt",
                                                  checkpoint_file = "simulation_checkpoint.rds") {

  # Initialize log
  log_message(paste("Starting multiple simulations with", nrow(param_grid),
                    "parameter combinations"), log_file = log_file)
  log_message(paste("Using", n_cores, "CPU cores"), log_file = log_file)

  results_list <- list()

  # Check if checkpoint exists
  if (file.exists(checkpoint_file)) {
    checkpoint_data <- readRDS(checkpoint_file)
    results_list <- checkpoint_data$results_list
    start_idx <- checkpoint_data$next_idx
    log_message(paste("Resuming from checkpoint at index", start_idx), log_file = log_file)
  } else {
    start_idx <- 1
    log_message("No checkpoint found, starting from beginning", log_file = log_file)
  }

  # Track overall timing
  start_time <- Sys.time()
  log_message(paste("Start time:", start_time), log_file = log_file)

  for (i in start_idx:nrow(param_grid)) {
    params <- param_grid[i, ]

    # Log current parameter combination
    log_message(sprintf("Starting simulation %d of %d", i, nrow(param_grid)), log_file = log_file)
    log_message(paste("Parameters:",
                      "response_type =", params$response_type,
                      ", max_effect =", params$max_effect,
                      ", m_tanks =", params$m_tanks,
                      ", k_individuals =", params$k_individuals,
                      ", var_tank =", params$var_tank,
                      ", var_individual =", params$var_individual), log_file = log_file)

    # Track time for this simulation
    sim_start_time <- Sys.time()

    # Run simulation with these parameters
    tryCatch({
      sim_result <- run_power_simulation(
        n_sim = n_sim,
        n_doses = params$n_doses,
        dose_range = c(params$min_dose, params$max_dose),
        m_tanks = params$m_tanks,
        k_individuals = params$k_individuals,
        var_tank = params$var_tank,
        var_individual = params$var_individual,
        max_effect = params$max_effect,
        response_type = params$response_type,
        test_methods = test_methods,
        alpha = params$alpha,
        alternative = params$alternative,
        n_cores = n_cores,
        seed = seed + i
      )

      # Store result
      results_list[[i]] <- sim_result

      # Save checkpoint
      saveRDS(list(results_list = results_list, next_idx = i + 1),
              file = checkpoint_file)

      # Log completion and timing
      sim_end_time <- Sys.time()
      elapsed <- difftime(sim_end_time, sim_start_time, units = "mins")
      log_message(sprintf("Completed simulation %d in %.2f minutes",
                          i, as.numeric(elapsed)), log_file = log_file)

    }, error = function(e) {
      error_msg <- conditionMessage(e)
      log_message(paste("ERROR in simulation", i, ":", error_msg), log_file = log_file)
      ## if exists cl, do we need to close? close inside the function.
      # Save checkpoint even if there's an error
      saveRDS(list(results_list = results_list, next_idx = i),
              file = checkpoint_file)
    })

    # Estimate remaining time
    if (i < nrow(param_grid)) {
      elapsed_total <- difftime(Sys.time(), start_time, units = "mins")
      avg_time_per_sim <- as.numeric(elapsed_total) / (i - start_idx + 1)
      remaining_sims <- nrow(param_grid) - i
      est_remaining <- avg_time_per_sim * remaining_sims

      log_message(sprintf("Estimated remaining time: %.1f minutes (%.1f hours)",
                          est_remaining, est_remaining/60), log_file = log_file)
    }
  }

  # Combine all results, handling potential NULL elements
  valid_results <- results_list[!sapply(results_list, is.null)]
  all_results <- do.call(rbind, valid_results)

  # Log completion
  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "hours")
  log_message(paste("All simulations completed in",
                    sprintf("%.2f hours", as.numeric(total_time))),
              log_file = log_file)
  log_message(paste("Final results contain", nrow(all_results), "rows"),
              log_file = log_file)

  return(all_results)
}



# Incremental saving during simulation
run_multiple_simulations_with_checkpoints <- function(param_grid, test_methods, n_sim = 1000,
                                                      n_cores = parallel::detectCores() - 1,
                                                      seed = 123,
                                                      checkpoint_file = "simulation_checkpoint.rds") {

  results_list <- list()

  # Check if checkpoint exists
  if (file.exists(checkpoint_file)) {
    checkpoint_data <- readRDS(checkpoint_file)
    results_list <- checkpoint_data$results_list
    start_idx <- checkpoint_data$next_idx
    cat("Resuming from checkpoint at index", start_idx, "\n")
  } else {
    start_idx <- 1
  }

  for (i in start_idx:nrow(param_grid)) {
    params <- param_grid[i, ]

    cat(sprintf("Running simulation %d of %d: %s, max_effect = %f\n",
                i, nrow(param_grid), params$response_type, params$max_effect))

    # Run simulation with these parameters
    tryCatch({
      sim_result <- run_power_simulation(
        n_sim = n_sim,
        n_doses = params$n_doses,
        dose_range = c(params$min_dose, params$max_dose),
        m_tanks = params$m_tanks,
        k_individuals = params$k_individuals,
        var_tank = params$var_tank,
        var_individual = params$var_individual,
        max_effect = params$max_effect,
        response_type = params$response_type,
        test_methods = test_methods,
        alpha = params$alpha,
        alternative = params$alternative,
        n_cores = n_cores,
        seed = seed + i
      )

      # Store result
      results_list[[i]] <- sim_result

      # Save checkpoint
      saveRDS(list(results_list = results_list, next_idx = i + 1),
              file = checkpoint_file)

    }, error = function(e) {
      cat("Error in simulation", i, ":", conditionMessage(e), "\n")
      # Save checkpoint even if there's an error
      saveRDS(list(results_list = results_list, next_idx = i),
              file = checkpoint_file)
    })
  }

  # Combine all results, handling potential NULL elements
  valid_results <- results_list[!sapply(results_list, is.null)]
  all_results <- do.call(rbind, valid_results)

  return(all_results)
}


#' Simulation Engine for Power Analysis (With Custom Response Function)
#'
#' This function runs a power analysis simulation with multiple response pattern options,
#' including a custom pattern where only higher doses show effects.
#'
#' @param n_sim Number of simulation iterations
#' @param n_doses Number of dose levels
#' @param dose_range Vector specifying min and max dose values
#' @param m_tanks Number of tanks per dose
#' @param k_individuals Number of individuals per tank
#' @param var_tank Variance at tank level (single value or vector by dose)
#' @param var_individual Variance at individual level (single value or vector by dose)
#' @param max_effect Maximum effect size at highest dose
#' @param response_type Type of dose-response: "decreasing", "non_monotonic", "none", "threshold"
#' @param threshold_idx Index at which effects begin to appear (for threshold response)
#' @param test_methods List of test method functions to compare
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @param n_cores Number of CPU cores to use
#' @param seed Random seed for reproducibility
#'
#' @return Data frame with simulation results
run_power_simulation <- function(n_sim = 100,
                                 n_doses = 5,
                                 dose_range = c(0, 20),
                                 m_tanks = 3,
                                 k_individuals = 10,
                                 var_tank = 4,
                                 var_individual = 2,
                                 max_effect = 20,
                                 response_type = c("decreasing", "non_monotonic", "none", "threshold","oscillating"),
                                 threshold_idx = 3,  # Index at which effects begin (for threshold response)
                                 test_methods = list(),
                                 alpha = 0.05,
                                 alternative = "less",
                                 n_cores = parallel::detectCores() - 1,
                                 seed = 123) {
  log_message(paste("Starting simulation with", n_sim, "iterations"))
  # progress_tracker <- function(n) {
  #   if (n %% 10 == 0 || n == n_sim) {
  #     log_message(paste("Completed", n, "of", n_sim, "iterations",
  #                       sprintf("(%.1f%%)", n/n_sim*100)))
  #   }
  # }
  # Match response type argument
  response_type <- match.arg(response_type)

  # Calculate doses
  doses <- seq(dose_range[1], dose_range[2], length.out = n_doses)

  # Set up response function based on type
  if (response_type == "decreasing") {
    # Linear decreasing response
    response_fn <- function(dose, max_effect) {
      base_response <- 100
      max_dose <- max(doses)
      return(base_response - (dose / max_dose) * max_effect)
    }
  } else if (response_type == "non_monotonic") {
    # Non-monotonic response with maximum effect at middle dose
    response_fn <- function(dose, max_effect) {
      base_response <- 100
      mid_dose <- mean(dose_range)
      return(base_response - max_effect * (1 - ((dose - mid_dose)/(mid_dose))^2))
    }
  } else if (response_type == "threshold") {
    # Threshold response: first n doses same as control, then decreasing
    response_fn <- function(dose, max_effect) {
      base_response <- 100
      result <- rep(base_response, length(dose))

      # Find threshold dose (dose at index threshold_idx)
      if (threshold_idx >= n_doses) {
        return(result)  # All doses are at baseline if threshold is too high
      }

      threshold_dose <- doses[threshold_idx]

      # For doses >= threshold, apply decreasing effect
      high_doses <- dose >= threshold_dose
      if (any(high_doses)) {
        # Scale effect based on how far above threshold
        max_high_dose <- max(doses)
        relative_position <- (dose[high_doses] - threshold_dose) / (max_high_dose - threshold_dose)
        result[high_doses] <- base_response - relative_position * max_effect
      }

      return(result)
    }
  } else if (response_type == "oscillating") {
    # Oscillating response pattern
    response_fn <- function(dose, max_effect) {
      oscillating_response(dose, max_effect = max_effect, frequency = 1, baseline = 100)
    }
  }else { # "none"
    response_fn <- function(dose, max_effect) {
      rep(100, length(dose))
    }
  }

  # Set up parallel backend
  if (exists("cl")) {
    try(parallel::stopCluster(cl), silent = TRUE)
  }
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)

  # Create a function to run a single simulation
  run_single_sim <- function(sim_id, seed_offset) {
    # Set seed for this simulation
    set.seed(seed + seed_offset)

    # Generate data
    sim_data <- simulate_dose_response(
      n_doses = n_doses,
      dose_range = dose_range,
      m_tanks = m_tanks,
      k_individuals = k_individuals,
      var_tank = var_tank,
      var_individual = var_individual,
      include_individuals = TRUE,
      response_function = function(dose) response_fn(dose, max_effect)
    )

    # Apply each test method
    results <- list()
    for (method_name in names(test_methods)) {
      method_fn <- test_methods[[method_name]]
      test_result <- method_fn(sim_data, alpha = alpha, alternative = alternative)

      # Extract p-values and significant flags
      results[[method_name]] <- list(
        p_values = test_result$p_values,
        significant = test_result$significant
      )
    }

    return(list(
      sim_id = sim_id,
      results = results
    ))
  }

  # Run simulations in parallel
  sim_results <- foreach::foreach(i = 1:n_sim, .packages = c("PMCMRplus", "rstatix",
                                                             "nlme", "lme4", "multcomp","drcHelper")) %dopar% {
    run_single_sim(i, i)
  }

  # Stop cluster
  parallel::stopCluster(cl)
  log_message("Parallel processing completed")
  # Process results
  process_results <- function(sim_results) {
    # Initialize results storage
    method_names <- names(test_methods)
    n_methods <- length(method_names)

    # Calculate power (proportion of significant results) for each dose level and method
    # First, determine the number of dose levels from the first simulation
    n_dose_levels <- length(sim_results[[1]]$results[[method_names[1]]]$significant)

    # Initialize power matrix: methods x dose levels
    power_matrix <- matrix(0, nrow = n_methods, ncol = n_dose_levels)
    rownames(power_matrix) <- method_names
    colnames(power_matrix) <- paste0("Dose_", 1:n_dose_levels)

    # Calculate power for each method and dose level
    for (m in 1:n_methods) {
      method <- method_names[m]
      for (d in 1:n_dose_levels) {
        # Count significant results across all simulations for this dose level
        sig_count <- sum(sapply(sim_results, function(x) x$results[[method]]$significant[d]))
        power_matrix[m, d] <- sig_count / n_sim
      }
    }

    # Convert to data frame for easier handling
    power_df <- as.data.frame(power_matrix)
    power_df$Method <- rownames(power_df)

    # Reshape to long format
    power_long <- reshape2::melt(power_df, id.vars = "Method",
                                 variable.name = "Dose_Level",
                                 value.name = "Power")

    # Add simulation parameters
    power_long$n_sim <- n_sim
    power_long$n_doses <- n_doses
    power_long$m_tanks <- m_tanks
    power_long$k_individuals <- k_individuals
    power_long$var_tank <- if(length(var_tank) == 1) var_tank else "varying"
    power_long$var_individual <- if(length(var_individual) == 1) var_individual else "varying"
    power_long$max_effect <- max_effect
    power_long$response_type <- response_type
    power_long$alpha <- alpha
    power_long$alternative <- alternative

    # Add actual doses and expected responses
    doses <- seq(dose_range[1], dose_range[2], length.out = n_doses)
    power_long$Dose <- doses[as.numeric(gsub("Dose_", "", power_long$Dose_Level))+1]
    power_long$Expected_Response <- response_fn(power_long$Dose, max_effect)

    return(power_long)
  }

  results_df <- process_results(sim_results)
  log_message("Simulation completed")
  return(results_df)
}


#' Simulation Engine for Power Analysis
#'
#' This function runs a power analysis simulation comparing different statistical methods
#' for dose-response analysis using parallel processing.
#'
#' @param n_sim Number of simulation iterations
#' @param n_doses Number of dose levels
#' @param dose_range Vector specifying min and max dose values
#' @param m_tanks Number of tanks per dose
#' @param k_individuals Number of individuals per tank
#' @param var_tank Variance at tank level (single value or vector by dose)
#' @param var_individual Variance at individual level (single value or vector by dose)
#' @param effect_size Effect size parameter for response function
#' @param response_type Type of dose-response: "decreasing", "non_monotonic", "none"
#' @param test_methods List of test method functions to compare
#' @param alpha Significance level
#' @param alternative Direction of alternative hypothesis ("less" or "greater")
#' @param n_cores Number of CPU cores to use (default: detectCores() - 1)
#' @param seed Random seed for reproducibility
#'
#' @return Data frame with simulation results
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom stats aggregate
run_power_simulation_deprecated <- function(n_sim = 100,
                                 n_doses = 5,
                                 dose_range = c(0, 20),
                                 m_tanks = 3,
                                 k_individuals = 10,
                                 var_tank = 4,
                                 var_individual = 2,
                                 effect_size = 1,
                                 response_type = c("decreasing", "non_monotonic", "none"),
                                 test_methods = list(),
                                 alpha = 0.05,
                                 alternative = "less",
                                 n_cores = parallel::detectCores() - 1,
                                 seed = 123) {

  # Match response type argument
  response_type <- match.arg(response_type)

  # Set up response function based on type
  if (response_type == "decreasing") {
    response_fn <- function(dose, effect_size) {
      100 - dose * effect_size
    }
  } else if (response_type == "non_monotonic") {
    response_fn <- function(dose, effect_size) {
      100 - effect_size * (dose - 10)^2 / 5
    }
  } else { # "none"
    response_fn <- function(dose, effect_size) {
      rep(100, length(dose))
    }
  }

  # Set up parallel backend
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)

  # Create a function to run a single simulation
  run_single_sim <- function(sim_id, seed_offset) {
    # Set seed for this simulation
    set.seed(seed + seed_offset)

    # Generate data
    sim_data <- drcHelper::simulate_dose_response(
      n_doses = n_doses,
      dose_range = dose_range,
      m_tanks = m_tanks,
      k_individuals = k_individuals,
      var_tank = var_tank,
      var_individual = var_individual,
      include_individuals = TRUE,
      response_function = function(dose) response_fn(dose, effect_size)
    )

    # Apply each test method
    results <- list()
    for (method_name in names(test_methods)) {
      method_fn <- test_methods[[method_name]]
      test_result <- method_fn(sim_data, alpha = alpha, alternative = alternative)

      # Extract p-values and significant flags
      results[[method_name]] <- list(
        p_values = test_result$p_values,
        significant = test_result$significant
      )
    }

    return(list(
      sim_id = sim_id,
      results = results
    ))
  }

  # Run simulations in parallel
  sim_results <- foreach::foreach(i = 1:n_sim, .packages = c("PMCMRplus", "rstatix", "nlme", "lme4", "multcomp","drcHelper")) %dopar% {
    run_single_sim(i, i)
  }

  # Stop cluster
  parallel::stopCluster(cl)

  # Process results
  process_results <- function(sim_results) {
    # Initialize results storage
    method_names <- names(test_methods)
    n_methods <- length(method_names)

    # Calculate power (proportion of significant results) for each dose level and method
    # First, determine the number of dose levels from the first simulation
    n_dose_levels <- length(sim_results[[1]]$results[[method_names[1]]]$significant)

    # Initialize power matrix: methods x dose levels
    power_matrix <- matrix(0, nrow = n_methods, ncol = n_dose_levels)
    rownames(power_matrix) <- method_names
    colnames(power_matrix) <- paste0("Dose_", 1:n_dose_levels)

    # Calculate power for each method and dose level
    for (m in 1:n_methods) {
      method <- method_names[m]
      for (d in 1:n_dose_levels) {
        # Count significant results across all simulations for this dose level
        sig_count <- sum(sapply(sim_results, function(x) x$results[[method]]$significant[d]))
        power_matrix[m, d] <- sig_count / n_sim
      }
    }

    # Convert to data frame for easier handling
    power_df <- as.data.frame(power_matrix)
    power_df$Method <- rownames(power_df)

    # Reshape to long format
    power_long <- reshape2::melt(power_df, id.vars = "Method",
                                 variable.name = "Dose_Level",
                                 value.name = "Power")

    # Add simulation parameters
    power_long$n_sim <- n_sim
    power_long$n_doses <- n_doses
    power_long$m_tanks <- m_tanks
    power_long$k_individuals <- k_individuals
    power_long$var_tank <- if(length(var_tank) == 1) var_tank else "varying"
    power_long$var_individual <- if(length(var_individual) == 1) var_individual else "varying"
    power_long$effect_size <- effect_size
    power_long$response_type <- response_type
    power_long$alpha <- alpha
    power_long$alternative <- alternative

    return(power_long)
  }

  results_df <- process_results(sim_results)

  return(results_df)
}

#' Plot Power Analysis Results
#'
#' @param power_results Results data frame from run_power_simulation
#' @return ggplot object
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_wrap theme_minimal labs
plot_power_results <- function(power_results) {
  # Extract dose level numbers for proper ordering
  power_results$Dose_Num <- as.numeric(gsub("Dose_", "", power_results$Dose_Level))

  # Create plot
  p <- ggplot2::ggplot(power_results, ggplot2::aes(x = Dose_Num, y = Power, color = Method, group = Method)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Power Analysis:", power_results$response_type[1], "Response"),
      subtitle = paste("Effect Size:", power_results$max_effect[1],
                       "| Tanks:", power_results$m_tanks[1],
                       "| Individuals:", power_results$k_individuals[1]),
      x = "Dose Level",
      y = "Power (Proportion of Significant Results)"
    )

  return(p)
}

