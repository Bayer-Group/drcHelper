---
description: Add a new dataset to the drcHelper package
---

# Add Dataset

Add a new dataset to the drcHelper package following standard conventions.

## Steps

1. Create or update the data preparation script in `data-raw/DATASET.R`
2. Save the dataset with `usethis::use_data({{dataset_name}}, overwrite = TRUE)`
3. Document the dataset in `R/data_description.R` with roxygen2:
   ```r
   #' {{dataset_title}}
   #'
   #' {{description}}
   #'
   #' @format A data frame with X rows and Y variables:
   #' \describe{
   #'   \item{col1}{Description of col1}
   #'   \item{col2}{Description of col2}
   #' }
   #' @source Source of the data
   "{{dataset_name}}"
   ```
4. Run `devtools::document()`
5. Verify with `devtools::check()`

## Dataset: {{dataset_name}}
