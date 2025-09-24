# Check which endpoints have count vs continuous data
load('data/test_cases_data.rda')
load('data/test_cases_res.rda')

# Get endpoints with Dunnett results
dunnett_results <- test_cases_res[grepl('Dunnett', test_cases_res[['Brief description']]), ]
dunnett_endpoints <- unique(dunnett_results[c('Study ID', 'Endpoint')])

cat("=== ENDPOINT DATA TYPE ANALYSIS ===\n\n")

for(i in 1:nrow(dunnett_endpoints)) {
  study_id <- dunnett_endpoints[i, 'Study ID']
  endpoint <- dunnett_endpoints[i, 'Endpoint']
  
  # Get data for this specific study + endpoint combination
  endpoint_data <- test_cases_data[
    test_cases_data[['Study ID']] == study_id & 
    test_cases_data[['Endpoint']] == endpoint, ]
  
  has_total <- any(!is.na(endpoint_data[['Total']]))
  has_alive <- any(!is.na(endpoint_data[['Alive']]))
  has_dead <- any(!is.na(endpoint_data[['Dead']]))
  is_count_data <- has_total || has_alive || has_dead
  
  cat(sprintf("Study: %s\n", study_id))
  cat(sprintf("Endpoint: %s\n", endpoint))
  cat(sprintf("  Rows: %d\n", nrow(endpoint_data)))
  cat(sprintf("  Has Total column data: %s\n", has_total))
  cat(sprintf("  Has Alive column data: %s\n", has_alive))
  cat(sprintf("  Has Dead column data: %s\n", has_dead))
  cat(sprintf("  Is COUNT data: %s\n", is_count_data))
  cat(sprintf("  Response values: %s\n", paste(head(endpoint_data$Response, 3), collapse=", ")))
  cat("\n")
}