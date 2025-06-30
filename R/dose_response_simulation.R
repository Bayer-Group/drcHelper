#' Generate Oscillating Response Pattern
#'
#' This function creates a response pattern that oscillates around the baseline,
#' with some doses showing increases and others showing decreases.
#'
#' @param doses Vector of dose values
#' @param max_effect Maximum magnitude of effect (both positive and negative)
#' @param frequency How many complete oscillations across the dose range
#' @param baseline Baseline response value
#' @param dose_range range of doses or concentrations
#' @return Vector of response values
#' @keywords simulation
#' @export
oscillating_response <- function(doses, max_effect = 20, frequency = 1, baseline = 100,dose_range=c(0,20)) {
  # Normalize doses to 0-1 range for easier calculation
  ## dose_range <- range(doses)  ## note that you cannot put in doses as a single number in this case, I would add to
  normalized_doses <- (doses - dose_range[1]) / (dose_range[2] - dose_range[1])

  # Calculate oscillating response using sine function
  # Multiply by 2π*frequency to get desired number of oscillations
  response <- baseline + max_effect * sin(2 * pi * frequency * normalized_doses)

  return(response)
}



#' Wrapper around rdrm
#'
#' Wrapper around rdrm to generate a data frame or a tibble object instead of a list of two matrices
#'
#' @param nosim numeric. The number of simulated curves to be returned.
#' @param fct list. Any built-in function in the package drc or a list with similar components.
#' @param mpar numeric. The model parameters to be supplied to fct.
#' @param xerror numeric or character. The distribution for the dose values.
#' @param xpar numeric vector supplying the parameter values defining the distribution for the dose values. If xerror is a distribution then remember that the number of dose values also is part of this argument (the first argument).
#' @param yerror numeric or character. The error distribution for the response values.
#' @param ypar numeric vector supplying the parameter values defining the error distribution for the response values.
#' @param onlyY logical. If TRUE then only the response values are returned (useful in simulations). Otherwise both dose values and response values (and for binomial data also the weights) are returned.
#'
#' @return a data frame
#' @seealso [drc::rdrm()], [MCPMod::genDFdata()]
#' @keywords simulation
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- rdrm(1, LL.3(), c(`b:(Intercept)` = 3,
#' `d:(Intercept)` = 8, `e:(Intercept)` = 3),
#' xerror=c(0, 0, 0, 0, 0, 0, 0.94, 0.94, 0.94, 1.88, 1.88, 1.88, 3.75,
#'          3.75, 3.75, 7.5, 7.5, 7.5, 15, 15, 15, 30, 30, 30),
#' yerror = "rnorm", ypar = c(0, 0.6))
#' dat <- data.frame(Dose = dat$x[1,], Response = dat$y[1,])
#' simDRdata(10, LL.3(), c(`b:(Intercept)` = 3,
#' `d:(Intercept)` = 8, `e:(Intercept)` = 3),
#' xerror=c(0, 0, 0, 0, 0, 0, 0.94, 0.94, 0.94, 1.88, 1.88, 1.88, 3.75,
#'          3.75, 3.75, 7.5, 7.5, 7.5, 15, 15, 15, 30, 30, 30),
#' yerror = "rnorm", ypar = c(0, 0.6))
#' }
simDRdata <- function(nosim, fct, mpar, xerror, xpar = 1, yerror = "rnorm",
                      ypar = c(0, 1), onlyY = FALSE){
  if(yerror=="rbinom"){
    dat <- drc::rdrm(nosim=nosim, fct=fct, mpar=mpar, xerror=xerror, xpar=xpar , yerror="rbinom" , ypar=ypar, onlyY=onlyY)
    if(onlyY){
      dat <- plyr::ldply(1:nosim,function(i) data.frame(Dose=xerror,Response=dat$y[i,],Weight=ypar,Sim=paste("Sim",i) ))
    } else
      dat <- plyr::ldply(1:nosim,function(i) data.frame(Dose=dat$x[i,],Response=dat$y[i,],Weight=dat$w[i,],Sim=paste("Sim",i) ))

  }else{
    if(yerror=="rnorm"){
      dat <- drc::rdrm(nosim=nosim, fct=fct, mpar=mpar, xerror=xerror, xpar=xpar , yerror="rnorm" , ypar=ypar, onlyY=onlyY)
      if(onlyY){
        dat <- plyr::ldply(1:nosim,function(i) data.frame(Dose=xerror,Response=dat$y[i,],Sim=paste("Sim",i) ))
      } else
        dat <- plyr::ldply(1:nosim,function(i) data.frame(Dose=dat$x[i,],Response=dat$y[i,],Sim=paste("Sim",i) ))

    }
  }
  return(dat)
}


#' Simulate Hierarchical Dose-Response Data with Inhomogeneous Variance
#'
#' This function simulates dose-response data with a hierarchical structure:
#' n doses, m tanks per dose, and optionally k individuals per tank, with variance components
#' that can vary by dose level.
#'
#' @param n_doses Number of dose levels
#' @param dose_range Vector of length 2 specifying the min and max dose values
#' @param m_tanks Number of tanks per dose
#' @param k_individuals Number of individuals per tank (only used if include_individuals = TRUE)
#' @param var_tank Variance at the tank level. Can be a single value or a vector of length n_doses.
#' @param var_individual Variance at the individual level. Can be a single value or a vector of length n_doses.
#' @param include_individuals Logical, whether to simulate individual-level data (TRUE) or only tank-level data (FALSE)
#' @param response_function Function that calculates the response given a dose
#' @param ... Additional parameters to pass to the response_function
#'
#' @return A data frame containing the simulated dose-response data
#' @keywords simulation
#' @export
simulate_dose_response <- function(n_doses,
                                   dose_range = c(0, 20),
                                   m_tanks = 3,
                                   k_individuals = 10,
                                   var_tank = 4,
                                   var_individual = 2,
                                   include_individuals = TRUE,
                                   response_function = NULL,
                                   ...) {

  # Set default response function if not provided
  if (is.null(response_function)) {
    response_function <- function(dose, lower = 0, upper = 100, ED50 = 10, slope = 1) {
      lower + (upper - lower) / (1 + exp(-slope * (dose - ED50)))
    }
  }

  # Define doses
  doses <- seq(dose_range[1], dose_range[2], length.out = n_doses)

  # Handle variance parameters
  # If var_tank is a single value, replicate it for each dose
  if (length(var_tank) == 1) {
    var_tank <- rep(var_tank, n_doses)
  } else if (length(var_tank) != n_doses) {
    stop("var_tank must be either a single value or a vector of length n_doses")
  }

  # If var_individual is a single value, replicate it for each dose
  if (length(var_individual) == 1) {
    var_individual <- rep(var_individual, n_doses)
  } else if (length(var_individual) != n_doses) {
    stop("var_individual must be either a single value or a vector of length n_doses")
  }

  # Create a lookup table for variances by dose
  var_lookup <- data.frame(
    Dose = doses,
    var_tank = var_tank,
    var_individual = var_individual
  )

  if (include_individuals) {
    # Simulate data with individuals

    # Create a data frame with all combinations of dose, tank, and individual
    simulated_data <- expand.grid(
      Dose = doses,
      Tank = 1:m_tanks,
      Individual = 1:k_individuals
    )

    # Calculate the base response for each dose (vectorized)
    simulated_data$BaseResponse <- response_function(simulated_data$Dose, ...)

    # Merge with variance lookup table to get the appropriate variances for each dose
    simulated_data <- merge(simulated_data, var_lookup, by = "Dose")

    # Generate tank-level random effects (vectorized)
    # Create a unique identifier for each dose-tank combination
    simulated_data$DoseTank <- paste(simulated_data$Dose, simulated_data$Tank, sep = "_")
    unique_dose_tanks <- unique(simulated_data[, c("DoseTank", "var_tank")])

    # Generate tank effects for each unique dose-tank combination
    tank_effects <- sapply(1:nrow(unique_dose_tanks), function(i) {
      stats::rnorm(1, mean = 0, sd = sqrt(unique_dose_tanks$var_tank[i]))
    })
    names(tank_effects) <- unique_dose_tanks$DoseTank

    # Add tank effects to the data frame
    simulated_data$TankEffect <- tank_effects[simulated_data$DoseTank]

    # Generate individual-level random effects (vectorized)
    simulated_data$IndividualEffect <- sapply(1:nrow(simulated_data), function(i) {
      stats::rnorm(1, mean = 0, sd = sqrt(simulated_data$var_individual[i]))
    })

    # Calculate the final response
    simulated_data$Response <- simulated_data$BaseResponse + simulated_data$TankEffect + simulated_data$IndividualEffect

    # Clean up the data frame by removing intermediate columns
    simulated_data <- simulated_data[, c("Dose", "Tank", "Individual", "Response")]

  } else {
    # Simulate data at tank level only

    # Create a data frame with all combinations of dose and tank
    simulated_data <- expand.grid(
      Dose = doses,
      Tank = 1:m_tanks
    )

    # Calculate the base response for each dose (vectorized)
    simulated_data$BaseResponse <- response_function(simulated_data$Dose, ...)

    # Merge with variance lookup table to get the appropriate variances for each dose
    simulated_data <- merge(simulated_data, var_lookup, by = "Dose")

    # Generate tank-level random effects with dose-specific variance
    simulated_data$TankEffect <- sapply(1:nrow(simulated_data), function(i) {
      stats::rnorm(1, mean = 0, sd = sqrt(simulated_data$var_tank[i]))
    })

    # Calculate the final response (no individual effects)
    simulated_data$Response <- simulated_data$BaseResponse + simulated_data$TankEffect

    # Clean up the data frame by removing intermediate columns
    simulated_data <- simulated_data[, c("Dose", "Tank", "Response")]
  }

  return(simulated_data)
}



#' Adding detailed logging
#'
#' @param message message to be written
#' @param log_file file to be appended to
#'
#' @return log_file with series of messages if specified in the simulation
#' @export
log_message <- function(message, log_file = "simulation_log.txt") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("[", timestamp, "] ", message)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}
