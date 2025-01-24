# test_visualization.R
context("Testing visualization")

library(testthat)

test_that("Q-Q plot is generated", {
  # Simulate some data
  y=c(0.131517109035102, 0.117455425985384, 0.130835155683102, 0.12226548296818,
      0.127485057136569, 0.128828137633933, 0.122888192029009, 0.126866725094641,
      0.128467082586674, 0.116653888503673,
      0.0906079518188219, 0.102060998252763, 0.107240263636048, 0.0998663441976353,
      0.0584507373938537, 0.066439126181113, 0.0806046608441279, 0.0828404794158172,
      0.0462632004630849, 0.0461876546375037, 0.0512317665813575, 0.0416533060961155,
      0.0267638178172436, 0.0314508456741666, 0.0237960318948956, 0.0295133681572911,
      0.0273565894468647, 0.0324226779638651, 0.0289617934362975, 0.0305317153447814)
  Rate = c(0,0,0,0,0,0,
           0.0448, 0.0448, 0.0448, 0.0448,
           0.132, 0.132, 0.132, 0.132,
           0.390, 0.390, 0.390, 0.390,
           1.15,1.15,1.15,1.15,
           3.39,3.39,3.39,3.39,
           10.0,10.0,10.0,10.0)
  Response <- data.frame(Rate, y)

  # Perform data processing
  res <- matrix(nrow = dim(Response)[1], ncol = 1)
  VGL <- matrix(nrow = dim(Response)[1], ncol = 1)
  j <- 1
  Rate0 <- Rate[1]
  for (i in 1:dim(Response)[1]) {
    control <- Response[Response$Rate == Rate[i],]
    y0 <- mean(control$y)
    res[i] = y[i] - y0
    if (Rate[i] != Rate0) {
      j <- j + 1
      Rate0 <- Rate[i]
    }
    VGL[i] <- j
  }
  Response <- data.frame(Rate, y, res, VGL)

  genQQ <- function(Response){
    # Generate Q-Q plot
    qq <- qqnorm(Response$res, plot.it = TRUE)  # Create Q-Q plot without plotting
    qql <- qqline(Response$res, col = "steelblue", lwd = 2)
  }

  save_png <- function(code, width = 400, height = 400) {
    path <- tempfile(fileext = ".png")
    png(path, width = width, height = height)
    on.exit(dev.off())
    code

    path
  }

  expect_snapshot_file(save_png(genQQ(Response)),"plot.png")
})
