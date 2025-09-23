# Statistical Test Report Generator
# ================================

# Load configuration
# Load configuration
source("inst/SystemTesting/config/test_framework_config.R")

generate_test_report <- function(test_name, output_dir = "Detailed_Testing_Reports") {
  
  if(!test_name %in% names(STATISTICAL_TESTS)) {
    stop("Unknown test name: ", test_name, ". Available tests: ", paste(names(STATISTICAL_TESTS), collapse = ", "))
  }
  
  test_config <- STATISTICAL_TESTS[[test_name]]
  
  # Read template
  template_path <- "inst/SystemTesting/templates/statistical_test_template.Rmd"
  if(!file.exists(template_path)) {
    stop("Template file not found: ", template_path)
  }
  
  template_content <- readLines(template_path, warn = FALSE)
  template_text <- paste(template_content, collapse = "\n")
  
  # Define replacement values
  replacements <- list(
    "\\{TEST_NAME\\}" = test_name,
    "\\{TEST_NAME_LOWER\\}" = test_name,
    "\\{TEST_TITLE\\}" = test_config$name,
    "\\{FUNCTION_GROUPS\\}" = paste(test_config$function_groups, collapse = ", "),
    "\\{TEST_ALTERNATIVES\\}" = ifelse(is.null(test_config$alternatives), "N/A", paste(test_config$alternatives, collapse = ", ")),
    "\\{KEY_METRICS\\}" = paste(test_config$key_metrics, collapse = ", "),
    "\\{IMPLEMENTATION_STATUS\\}" = ifelse(test_config$implemented, "IMPLEMENTED", "PENDING IMPLEMENTATION"),
    "\\{IMPLEMENTATION_CONCLUSION\\}" = ifelse(
      test_config$implemented, 
      "The test implementation is complete and validation results demonstrate the accuracy of the statistical calculations.",
      "The test implementation is pending. This framework provides the structure for validation once the test function is implemented."
    ),
    "\\{NEXT_STEPS\\}" = ifelse(
      test_config$implemented,
      "1. Review validation results\n2. Address any failing test cases\n3. Update test parameters if needed",
      paste(
        "1. Implement", test_config$test_function, "function",
        "2. Add result validation logic", 
        "3. Implement basic functionality tests",
        "4. Run full validation suite",
        sep = "\n"
      )
    )
  )
  
  # Apply replacements
  final_content <- template_text
  for(pattern in names(replacements)) {
    final_content <- gsub(pattern, replacements[[pattern]], final_content)
  }
  
  # Create output directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generate output filename
  output_filename <- paste0(tools::toTitleCase(test_name), "_Test_Cases.Rmd")
  output_path <- file.path(output_dir, output_filename)
  
  # Write the file
  writeLines(final_content, output_path)
  
  cat("Generated test report:", output_path, "\n")
  
  return(output_path)
}

# Function to generate reports for all tests
generate_all_test_reports <- function(output_dir = "Detailed_Testing_Reports", implemented_only = FALSE) {
  
  tests_to_generate <- get_available_tests(implemented_only)
  generated_files <- character()
  
  for(test_name in names(tests_to_generate)) {
    cat("\nGenerating report for:", test_name, "\n")
    
    tryCatch({
      output_path <- generate_test_report(test_name, output_dir)
      generated_files <- c(generated_files, output_path)
    }, error = function(e) {
      cat("Error generating", test_name, "report:", e$message, "\n")
    })
  }
  
  cat("\n=== Generation Summary ===\n")
  cat("Generated", length(generated_files), "test reports:\n")
  for(file in generated_files) {
    cat("-", file, "\n")
  }
  
  return(generated_files)
}

# Function to render a specific test report to HTML
render_test_report <- function(test_name, output_dir = "Detailed_Testing_Reports") {
  
  # Generate the Rmd file first if it doesn't exist
  rmd_filename <- paste0(tools::toTitleCase(test_name), "_Test_Cases.Rmd")
  rmd_path <- file.path(output_dir, rmd_filename)
  
  if(!file.exists(rmd_path)) {
    cat("Generating report file first...\n")
    generate_test_report(test_name, output_dir)
  }
  
  # Render to HTML
  cat("Rendering", rmd_path, "to HTML...\n")
  
  tryCatch({
    rmarkdown::render(rmd_path, output_format = "html_document")
    html_path <- gsub("\\.Rmd$", ".html", rmd_path)
    cat("Successfully rendered:", html_path, "\n")
    return(html_path)
  }, error = function(e) {
    cat("Error rendering report:", e$message, "\n")
    return(NULL)
  })
}

# Function to get test summary information
get_test_summary <- function() {
  
  all_tests <- get_available_tests()
  implemented_tests <- get_available_tests(implemented_only = TRUE)
  
  summary_df <- data.frame(
    Test_Name = names(all_tests),
    Display_Name = sapply(all_tests, function(x) x$name),
    Function_Groups = sapply(all_tests, function(x) paste(x$function_groups, collapse = ", ")),
    Status = sapply(names(all_tests), function(name) ifelse(name %in% names(implemented_tests), "✅ Implemented", "⏳ Pending")),
    Test_Function = sapply(all_tests, function(x) x$test_function),
    stringsAsFactors = FALSE
  )
  
  return(summary_df)
}

# Main execution when run directly
if(!interactive()) {
  cat("Statistical Test Report Generator\n")
  cat("=================================\n")
  
  # Show available tests
  summary <- get_test_summary()
  cat("\nAvailable Tests:\n")
  print(summary)
  
  # Check command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  if(length(args) == 0) {
    cat("\nUsage:\n")
    cat("  Rscript generate_test_reports.R [test_name|all|implemented]\n")
    cat("\nExamples:\n")
    cat("  Rscript generate_test_reports.R dunn          # Generate Dunn test report\n")
    cat("  Rscript generate_test_reports.R all           # Generate all test reports\n") 
    cat("  Rscript generate_test_reports.R implemented   # Generate implemented tests only\n")
  } else {
    command <- args[1]
    
    if(command == "all") {
      generate_all_test_reports()
    } else if(command == "implemented") {
      generate_all_test_reports(implemented_only = TRUE)
    } else if(command %in% names(STATISTICAL_TESTS)) {
      generate_test_report(command)
    } else {
      cat("Unknown command:", command, "\n")
      cat("Available tests:", paste(names(STATISTICAL_TESTS), collapse = ", "), "\n")
    }
  }
}