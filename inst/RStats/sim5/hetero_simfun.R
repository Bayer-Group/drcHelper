run_hetero_variance_simulation <- function(n_sim = 100,
                                           n_doses = 5,
                                           dose_range = c(0, 20),
                                           m_tanks = 3,
                                           variance_pattern = "homogeneous",
                                           base_variance = 4,
                                           max_effect = 20,
                                           response_type = c("decreasing", "non_monotonic", "none", "threshold", "oscillating"),
                                           threshold_idx = 3,
                                           test_methods = list(),
                                           alpha = 0.05,
                                           alternative = "less",
                                           n_cores = parallel::detectCores() - 1,
                                           seed = 123) {

  # Define variance pattern functions internally
  local_variance_patterns <- list(
    homogeneous = function(n_doses, base_var = 4) {
      rep(base_var, n_doses)
    },

    increasing = function(n_doses, base_var = 4) {
      base_var * seq(0.5, 2, length.out = n_doses)
    },

    decreasing = function(n_doses, base_var = 4) {
      base_var * seq(2, 0.5, length.out = n_doses)
    },

    v_shaped = function(n_doses, base_var = 4) {
      mid_point <- ceiling(n_doses/2)
      c(
        base_var * seq(2, 1, length.out = mid_point),
        base_var * seq(1, 2, length.out = n_doses - mid_point + 1)[-1]
      )
    }
  )
  # Match response type argument
  response_type <- match.arg(response_type)

  # Calculate doses
  doses <- seq(dose_range[1], dose_range[2], length.out = n_doses)

  # Generate dose-specific variances based on pattern
  # Generate dose-specific variances based on pattern
  if (is.character(variance_pattern) && variance_pattern %in% names(local_variance_patterns)) {
    var_tank <- local_variance_patterns[[variance_pattern]](n_doses, base_variance)
  } else if (is.numeric(variance_pattern) && length(variance_pattern) == n_doses) {
    var_tank <- variance_pattern
  } else {
    stop("variance_pattern must be one of the predefined patterns or a numeric vector of length n_doses")
  }

  # Set up response function based on type (same as in run_power_simulation)
  if (response_type == "decreasing") {
    response_fn <- function(dose, max_effect) {
      base_response <- 100
      max_dose <- max(doses)
      return(base_response - (dose / max_dose) * max_effect)
    }
  } else if (response_type == "non_monotonic") {
    response_fn <- function(dose, max_effect) {
      base_response <- 100
      mid_dose <- mean(dose_range)
      return(base_response - max_effect * (1 - ((dose - mid_dose)/(mid_dose))^2)) ## Here I need a change.
    }
  } else if (response_type == "threshold") {
    response_fn <- function(dose, max_effect) {
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
  } else if (response_type == "oscillating") { ## direct definition without calling oscillatinng_response function from drcHelper
    response_fn <- function(dose, max_effect) {
      base_response <- 100
      normalized_doses <- (dose - min(doses)) / (max(doses) - min(doses))
      return(base_response + max_effect * sin(2 * pi * normalized_doses))
    }
  } else { # "none"
    response_fn <- function(dose, max_effect) {
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

    # Generate data (tank-level only, no individuals)
    sim_data <- simulate_dose_response(
      n_doses = n_doses,
      dose_range = dose_range,
      m_tanks = m_tanks,
      var_tank = var_tank,  # Pass the dose-specific variances, note here it should be a vector instead of a singe number.
      include_individuals = FALSE,  # Tank-level data only
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
  sim_results <- foreach::foreach(i = 1:n_sim,
                                  .packages = c("PMCMRplus", "rstatix", "nlme", "lme4", "multcomp","drcHelper")) %dopar% {
                                    run_single_sim(i, i)
                                  }

  # Stop cluster
  parallel::stopCluster(cl)

  # Process results (similar to run_power_simulation)
  process_results <- function(sim_results) {
    # Initialize results storage
    method_names <- names(test_methods)
    n_methods <- length(method_names)

    # Calculate power for each method and dose level
    n_dose_levels <- length(sim_results[[1]]$results[[method_names[1]]]$significant)

    power_matrix <- matrix(0, nrow = n_methods, ncol = n_dose_levels)
    rownames(power_matrix) <- method_names
    colnames(power_matrix) <- paste0("Dose_", 1:n_dose_levels)

    for (m in 1:n_methods) {
      method <- method_names[m]
      for (d in 1:n_dose_levels) {
        sig_count <- sum(sapply(sim_results, function(x) x$results[[method]]$significant[d]))
        power_matrix[m, d] <- sig_count / n_sim
      }
    }

    # Convert to data frame
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
    power_long$variance_pattern <- if(is.character(variance_pattern)) variance_pattern else "custom"
    power_long$base_variance <- base_variance
    power_long$max_effect <- max_effect
    power_long$response_type <- response_type
    power_long$alpha <- alpha
    power_long$alternative <- alternative

    # Add actual doses, expected responses, and variances
    doses <- seq(dose_range[1], dose_range[2], length.out = n_doses)
    power_long$Dose <- doses[as.numeric(gsub("Dose_", "", power_long$Dose_Level))+1]
    power_long$Expected_Response <- response_fn(power_long$Dose, max_effect)
    power_long$Variance <- var_tank[as.numeric(gsub("Dose_", "", power_long$Dose_Level))+1]

    return(power_long)
  }

  results_df <- process_results(sim_results)

  return(results_df)
}



run_multiple_hetero_simulations <- function(param_grid, test_methods,
                                            variance_patters,n_sim = 1000,
                                            n_cores = parallel::detectCores() - 1,
                                            seed = 123,
                                            log_file = "hetero_simulation_log.txt",
                                            checkpoint_file = "hetero_simulation_checkpoint.rds") {

  # Initialize log
  log_message(paste("Starting heterogeneous variance simulations with",
                    nrow(param_grid), "parameter combinations"),
              log_file = log_file)

  results_list <- list()

  # Check if checkpoint exists
  if (file.exists(checkpoint_file)) {
    checkpoint_data <- readRDS(checkpoint_file)
    results_list <- checkpoint_data$results_list
    start_idx <- checkpoint_data$next_idx
    log_message(paste("Resuming from checkpoint at index", start_idx), log_file = log_file)
  } else {
    start_idx <- 1
  }


  for (i in start_idx:nrow(param_grid)) {
    params <- param_grid[i, ]

    log_message(sprintf("Running simulation %d of %d: pattern = %s, max_effect = %f",
                        i, nrow(param_grid), params$variance_pattern, params$max_effect),
                log_file = log_file)

    # Run simulation with these parameters
    tryCatch({


      sim_result <- run_hetero_variance_simulation(
        n_sim = n_sim,
        n_doses = params$n_doses,
        dose_range = c(params$min_dose, params$max_dose),
        m_tanks = params$m_tanks,
        variance_pattern = params$variance_pattern,
        base_variance = params$base_variance,
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
      log_message(paste("ERROR in simulation", i, ":", conditionMessage(e)),
                  log_file = log_file)

      # Save checkpoint even if there's an error
      saveRDS(list(results_list = results_list, next_idx = i),
              file = checkpoint_file)
    })
  }

  # Combine all results
  valid_results <- results_list[!sapply(results_list, is.null)]
  all_results <- do.call(rbind, valid_results)

  return(all_results)
}
