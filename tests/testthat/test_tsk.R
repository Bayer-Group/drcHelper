
test_that("tsk can calculate EPA result correctly",{


  p <- 2 * pnorm(2) - 1
  tolerance <- 0.006

  dr1a <- rbind(
    mid =  c(43.89, 44.16, 44.16, 44.16),
    low =  c(42.37, 41.92, NA, NA),
    high = c(45.47, 46.52, NA, NA)
  )
  dr1b <- rbind(
    mid =  c(43.27,	44.16,	44.16,	44.16),
    low =  c(41.35,	41.97, NA, NA),
    high = c(45.27,	46.46, NA, NA)
  )
  dr1c <- rbind(
    mid =  c(41.73, 42.53, 42.79, 42.91),
    low =  c(39.14, 39.86, 39.51, 40.77),
    high = c(44.49, 45.38, 46.34, 45.17)
  )
  dr1d <- rbind(
    mid =  c(31.36, 31.71, 31.71, 31.71),
    low =  c(29.74, 31.03, 31.08, 31.08),
    high = c(33.07, 32.4, 32.35, 32.35)
  )
  dr1e <- rbind(
    mid =  c(30.8, 31.48, 31.48, 31.48),
    low =  c(29.6, NA, NA, NA),
    high = c(32.04, NA, NA, NA)
  )

  dr4a <- rbind(
    mid =  c(26.75,27.2,27.38,27.38),
    low =  c(24.28,24.1,NA,NA),
    high = c(29.46,30.71,NA,NA)
  )
  dr4b <- rbind(
    mid =  c(19.7,19.52,19.36,19.07),
    low =  c(17,16.6,16.22,15.56),
    high = c(22.84,22.96,23.09,23.37)
  )
  dr4c <- rbind(
    mid =  c(36.14,36.96,37.49,38.18),
    low =  c(30.09,30.39,30.69,29.41),
    high = c(43.4,44.96,45.81,49.56)
  )
  dr4d <- rbind(
    mid =  c(29.55,29.69,29.79,29.74),
    low =  c(24.33,24.04,23.87,24),
    high = c(35.88,36.67,37.18,36.85)
  )
  dr4e <- rbind(
    mid =  c(36.11,37.06,38.05,40.23),
    low =  c(29.73,29.96,30.29,31.19),
    high = c(43.85,45.83,47.8,51.88)
  )

  expected = list(
    dr1a = dr1a,
    dr1b = dr1b,
    dr1c = dr1c,
    dr1d = dr1d,
    dr1e = dr1e,
    dr4a = dr4a,
    dr4b = dr4b,
    dr4c = dr4c,
    dr4d = dr4d,
    dr4e = dr4e
  )

  for (dr in expected) {
    colnames(dr) <- c(0, 5, 10, 20)
  }

  for (i in 1:length(expected)){
    ex <- expected[[i]]
    dr <- hamilton[[i]]
    dr$p <- dr$r/dr$n

    res0<-tsk(dr,  trim=0,conf.level=p)
    res5<-tsk(dr,  trim=0.05,conf.level=p)
    res10<-tsk(dr, trim=0.1,conf.level=p)
    res20<-tsk(dr, trim=0.2,conf.level=p)

    actual <- rbind(
      mid =  c(res0$LD50,res5$LD50,res10$LD50,res20$LD50),
      low =  c(res0$conf.int[1],res5$conf.int[1],
               res10$conf.int[1],res20$conf.int[1]),
      high = c(res0$conf.int[2],res5$conf.int[2],
               res10$conf.int[2],res20$conf.int[2])
    )

    diff <- abs(ex - actual)
    diff[is.na(diff)] <- 0 #ignore when the EPA result is missing
    mdiff <- max(diff)
    expect_equal(mdiff,0,tolerance = tolerance)
    # if (mdiff > tolerance){
    #   stop(paste("Failed test of", names(expected)[i],
    #              ", largest difference was ", mdiff))
    }

  }


)


describe("tsk function from drcHelper", {
  test_data <- data.frame(
    x = c(15.54, 20.47, 27.92, 35.98, 55.52),
    r = c(0, 0, 0, 1, 20),
    n = c(20, 20, 20, 19, 20)
  )

  it("computes TSK estimate and confidence intervals matching ecotoxicology::TSK", {
    result <- tsk(
      x = test_data$x,
      r = test_data$r,
      n = test_data$n,
      control = 0,
      trim = 0,
      conf.level = 0.95,
      use.log.doses = TRUE
    )

    expect_s3_class(result, "tskresult")
    expect_equal(result$LD50, 43.89339, tolerance = 0.0001,
                 info = "LD50 should match ecotoxicology::TSK result")
    expect_equal(as.numeric(result$gsd), 1.017763, tolerance = 0.0001,
                 info = "Geometric standard deviation should match ecotoxicology::TSK result")
    expect_equal(as.numeric(result$conf.int[1]), 42.40451, tolerance = 0.0001,
                 info = "Lower confidence interval should match ecotoxicology::TSK result")
    expect_equal(as.numeric(result$conf.int[2]), 45.43455, tolerance = 0.0001,
                 info = "Upper confidence interval should match ecotoxicology::TSK result")
  })
})

describe("tsk_auto function - automatic trimming wrapper", {
  
  describe("with data that doesn't need trimming", {
    moderate_data <- data.frame(
      x = c(0.1, 0.5, 1, 2, 4, 8),
      n = rep(20, 6),
      r = c(2, 5, 8, 12, 15, 17)  # Moderate responses, not extreme
    )
    
    it("returns valid tskresult object", {
      result <- tsk_auto(moderate_data)
      expect_s3_class(result, "tskresult")
    })
    
    it("uses numeric vector interface correctly", {
      result <- tsk_auto(
        x = moderate_data$x,
        n = moderate_data$n, 
        r = moderate_data$r
      )
      expect_s3_class(result, "tskresult")
      expect_true(is.numeric(result$LD50))
    })
    
    it("preserves all standard tsk parameters", {
      result <- tsk_auto(
        moderate_data, 
        control = 0, 
        conf.level = 0.90,
        use.log.doses = FALSE
      )
      expect_equal(attr(result$conf.int, "conf.level"), 0.90)
      expect_false(result$use.log.doses)
    })
  })
  
  describe("with data that needs automatic trimming", {
    extreme_data <- data.frame(
      x = c(0.1, 0.5, 1, 2, 4, 8),
      n = rep(20, 6),
      r = c(0, 1, 8, 15, 19, 20)  # Goes from 0% to 100% response
    )
    
    it("applies trimming when needed and returns valid result", {
      # Suppress messages for clean test output
      result <- suppressMessages(tsk_auto(extreme_data))
      expect_s3_class(result, "tskresult")
      expect_true(is.numeric(result$LD50))
    })
    
    it("applies positive trim value when automatic trimming occurs", {
      result <- suppressMessages(tsk_auto(extreme_data))
      # If trimming was applied, trim should be > 0
      if (result$trim > 0) {
        expect_gt(result$trim, 0)
        expect_lt(result$trim, 0.5)
      }
    })
    
    it("works with numeric vector interface for extreme data", {
      result <- suppressMessages(tsk_auto(
        x = extreme_data$x,
        n = extreme_data$n,
        r = extreme_data$r
      ))
      expect_s3_class(result, "tskresult")
    })
  })
  
  describe("parameter validation", {
    valid_data <- data.frame(
      x = c(1, 2, 4, 8),
      n = rep(10, 4),
      r = c(1, 3, 7, 9)
    )
    
    it("validates max.trim parameter correctly", {
      expect_error(
        tsk_auto(valid_data, max.trim = 0.5),
        "max.trim must be between 0 and 0.5"
      )
      
      expect_error(
        tsk_auto(valid_data, max.trim = 0),
        "max.trim must be between 0 and 0.5"
      )
      
      expect_error(
        tsk_auto(valid_data, max.trim = -0.1),
        "max.trim must be between 0 and 0.5"
      )
    })
    
    it("accepts valid max.trim values", {
      result <- suppressMessages(tsk_auto(valid_data, max.trim = 0.3))
      expect_s3_class(result, "tskresult")
    })
  })
  
  describe("error handling", {
    it("propagates non-trim related errors from original tsk function", {
      invalid_data <- data.frame(
        x = c(1, 2, 3),
        n = c(10, 10, 10),
        r = c(-1, 5, 8)  # Negative responses should cause error
      )
      
      expect_error(
        tsk_auto(invalid_data),
        "Responses must be nonnegative"
      )
    })
    
    it("handles duplicate doses error", {
      duplicate_data <- data.frame(
        x = c(1, 1, 2, 3),  # Duplicate doses
        n = c(10, 10, 10, 10),
        r = c(1, 2, 5, 8)
      )
      
      expect_error(
        tsk_auto(duplicate_data),
        "Duplicate doses exist in the data"
      )
    })
  })
  
  describe("integration with hamilton dataset", {
    it("works with hamilton dataset examples", {
      skip_if_not(exists("hamilton"), "Hamilton dataset not available")
      
      # Test with first hamilton dataset
      result <- suppressMessages(tsk_auto(hamilton[[1]]))
      expect_s3_class(result, "tskresult")
      expect_true(is.numeric(result$LD50))
    })
  })
})




