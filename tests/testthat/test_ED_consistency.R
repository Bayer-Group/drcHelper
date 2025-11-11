describe("EC value transformation in addECxCI", {
  # Load necessary data
  data(dat_medium)
  show_transformed_EC_levels <- function(object, respLev, trend = "Decrease") {
  coefs <- coef(object)
  if ("c:(Intercept)" %in% names(coefs)) cVal <- coefs["c:(Intercept)"] else cVal <- 0
  if ("d:(Intercept)" %in% names(coefs)) d <- coefs["d:(Intercept)"] else d <- 1
  
  if (trend == "Decrease") {
    x.relative <- d * respLev / (d - cVal)
  } else {
    x.relative <- (respLev - cVal * 100) / (d - cVal)
  }
  
  return(data.frame(
    original_respLev = respLev,
    transformed_x.relative = x.relative
  ))
}

  # Create models with different functions
  model_LN4 <- drm(Response ~ Dose, data = dat_medium, fct = LN.4())
  model_LL4 <- drm(Response ~ Dose, data = dat_medium, fct = LL.4()) 
  
  it("transforms response levels correctly for decreasing trend", {
    # Get transformed values for LN.4 model
    transformed_LN4 <- show_transformed_EC_levels(model_LN4, c(10, 20, 50))
    
    # Get model parameters to verify calculations
    coefs_LN4 <- coef(model_LN4)
    d_LN4 <- coefs_LN4["d:(Intercept)"]
    c_LN4 <- coefs_LN4["c:(Intercept)"]
    
    # Manually calculate expected transformations
    expected_transform_10 <- d_LN4 * 10 / (d_LN4 - c_LN4)
    expected_transform_50 <- d_LN4 * 50 / (d_LN4 - c_LN4)
    
    # Check results
    expect_equal(transformed_LN4$transformed_x.relative[1], as.numeric(expected_transform_10))
    expect_equal(transformed_LN4$transformed_x.relative[3], as.numeric(expected_transform_50))
    
    # Verify transformation formula is working correctly
    manual_calc <- d_LN4 * c(10, 20, 50) / (d_LN4 - c_LN4)
    expect_equal(transformed_LN4$transformed_x.relative, manual_calc)
  })
  
  it("produces different transformed values for different model types", {
    # Compare transformations between different model types
    transformed_LN4 <- show_transformed_EC_levels(model_LN4, c(10, 20, 50))
    transformed_LL4 <- show_transformed_EC_levels(model_LL4, c(10, 20, 50))
    
    # They should be different because the models estimate different parameters
    expect_false(identical(transformed_LN4$transformed_x.relative, 
                          transformed_LL4$transformed_x.relative))
  })
  
  it("explains the difference between addECxCI and ED function results", {
    # Get standard ED values
    ed_results <- as.data.frame(ED(model_LN4, respLev = c(10, 20, 50), 
                                  interval = "delta"))
    
    # Get transformed values that would be used by addECxCI
    transformed_values <- show_transformed_EC_levels(model_LN4, c(10, 20, 50))
    
    # Get EC values using the transformed response levels
    transformed_ed_results <- as.data.frame(ED(model_LN4, 
                                              respLev = transformed_values$transformed_x.relative,
                                              interval = "delta"))
    
    # Calculate EC values using ED.plus function (which is used by addECxCI)
    ed_plus_results <- as.data.frame(ED.plus(model_LN4, respLev = c(10, 20, 50), 
                                           trend = "Decrease"))
    
    # The transformed_ed_results should closely match ed_plus_results
    expect_equal(transformed_ed_results$Estimate, ed_plus_results$Estimate, 
                tolerance = 1e-5)
    
    # But standard ED results should be different
    expect_false(identical(ed_results$Estimate, ed_plus_results$Estimate))
    
    # Print the comparison for clarity
    cat("\nOriginal ED response levels vs. transformed values used by addECxCI:\n")
    comparison <- data.frame(
      ResponseLevel = c(10, 20, 50),
      TransformedLevel = round(transformed_values$transformed_x.relative, 2),
      StandardED = round(ed_results$Estimate, 4),
      TransformedED = round(transformed_ed_results$Estimate, 4)
    )
    print(comparison)
  })
  
  it("handles increasing trend differently", {
    transformed_decrease <- show_transformed_EC_levels(model_LN4, c(10, 20, 50), 
                                                    trend = "Decrease")
    transformed_increase <- show_transformed_EC_levels(model_LN4, c(10, 20, 50), 
                                                     trend = "Increase")
    
    # The transformation formula is different for increasing vs decreasing trends
    expect_false(identical(transformed_decrease$transformed_x.relative, 
                          transformed_increase$transformed_x.relative))
  })
})