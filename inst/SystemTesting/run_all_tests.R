# This script runs all test report generation

source("inst/SystemTesting/generate_test_reports.R")

# Generate all reports
generate_all_test_reports(output_dir = "inst/SystemTesting/Detailed_Testing_Reports")
