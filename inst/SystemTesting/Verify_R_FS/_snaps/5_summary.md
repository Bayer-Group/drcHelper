# Mean and other summary statistics can be calculated correctly

    Code
      tmp_data
    Output
      # A tibble: 7 x 11
           Rate     n  mean    var std.dev      W    CV Inhibition Reduction var_mdd
          <dbl> <int> <dbl>  <dbl>   <dbl>  <dbl> <dbl>      <dbl>     <dbl>   <dbl>
      1  0          6 37.6  12.7     3.56   0.474  9.46      NA        NA     63.4  
      2  0.0448     4 36.5  10.0     3.16   0.400  8.68       3.14     -3.14  30.0  
      3  0.132      4 21.2  17.1     4.13   0.234 19.5       43.7     -43.7   51.2  
      4  0.39       4 11.4   2.73    1.65   1.47  14.5       69.8     -69.8    8.19 
      5  1.15       4  6.75  0.120   0.347 33.2    5.14      82.1     -82.1    0.361
      6  3.39       4  3.54  0.396   0.629 10.1   17.8       90.6     -90.6    1.19 
      7 10          4  3.92  0.194   0.441 20.6   11.3       89.6     -89.6    0.583
      # i 1 more variable: se_mdd <dbl>

