library(drcHelper)

simple_data <- data.frame(
  Response = c(10.2, 9.8, 10.5, 10.1, 8.1, 7.9, 8.0, 6.2, 6.0, 6.5, 4.1, 4.3, 3.9),          
  Dose = c(0, 0, 0, 0, 1, 1, 1, 5, 5, 5, 10, 10, 10),   
  Tank = c(1, 1, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2)       
)

cat('Testing step by step...\n')
result <- dunnett_test(simple_data, response_var = 'Response', dose_var = 'Dose', 
                      tank_var = 'Tank', control_level = 0, alternative = 'less')

cat('Result structure:\n')
cat('- results_table is null:', is.null(result$results_table), '\n')
if(!is.null(result$results_table)) {
  cat('- results_table nrows:', nrow(result$results_table), '\n')
}
cat('- noec is null:', is.null(result$noec), '\n')
cat('- model_type is null:', is.null(result$model_type), '\n')

print(names(result))

# Test the logical conditions
has_results_table <- !is.null(result$results_table) && nrow(result$results_table) > 0
has_noec <- !is.null(result$noec)
has_model_type <- !is.null(result$model_type)

cat('Conditions:\n')
cat('has_results_table:', has_results_table, '\n')
cat('has_noec:', has_noec, '\n')
cat('has_model_type:', has_model_type, '\n')
cat('All passed:', has_results_table && has_noec && has_model_type, '\n')