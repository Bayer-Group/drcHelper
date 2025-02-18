describe("calcSteepnessOverlap", {
  datTn<- subset(oecd201,Time==72)
  mod <- drm(Yield~Concentration,data=datTn,fct=LL.3())
  it("calculates steepness and overlap correctly for valid model", {
    result <- calcSteepnessOverlap(mod = mod, trend = "Decrease")
    expect_type(result, "character")
    expect_length(result, 2)
    expect_equal(result,c("High","Medium"))
  })

  it("handles missing EC50 values gracefully", {
    ## need a dataset with no effects greater than 50%.

    result <- calcSteepnessOverlap(mod = mod, trend = "Decrease")
    expect_equal(result[2], "Not Defined")
  })

  it("returns NA when both mod and obj are NULL", {
    result <- calcSteepnessOverlap(mod = NULL, obj = NULL)
    expect_equal(result, c(NA, NA))
  })

  it("returns correct steepness for shallow, medium, and steep", {
    # Create specific test cases for known inputs and expected outputs
    mod_shallow <- create_test_drc_model_shallow()  # Replace with actual test model creation
    result_shallow <- calcSteepnessOverlap(mod = mod_shallow, trend = "Decrease")
    expect_equal(result_shallow[2], "Shallow")

    mod_medium <- create_test_drc_model_medium()  # Replace with actual test model creation
    result_medium <- calcSteepnessOverlap(mod = mod_medium, trend = "Decrease")
    expect_equal(result_medium[2], "Medium")

    mod_steep <- create_test_drc_model_steep()  # Replace with actual test model creation
    result_steep <- calcSteepnessOverlap(mod = mod_steep, trend = "Decrease")
    expect_equal(result_steep[2], "Steep")
  })

  # Additional tests can be added as necessary
})
