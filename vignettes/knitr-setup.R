# This file sets global knitr options for all vignettes in the project.
# It ensures that figures are saved with unique names to prevent overwrites
# during the pkgdown build process.

knitr::opts_chunk$set(
  # Create a unique prefix for each figure based on the Rmd filename.
  # e.g., for 'My-Vignette.Rmd', figures will be named 'My-Vignette-chunk-label-1.png'
  # and placed in the 'articles' directory during the pkgdown build.
  ## fig.path = paste0(tools::file_path_sans_ext(basename(knitr::current_input())), "-"),
  
  # Provide a default, non-descriptive alt text.
  # This is better than no alt text, but it's highly recommended to
  # provide specific, descriptive alt text for each plot using fig.alt in the chunk options.
  fig.alt = "A plot generated from an R code chunk."
)
