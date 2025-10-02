<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# drcHelper 0.0.4.9001

## Features

- Track knitr-setup.R and update .gitignore.

## Uncategorized

- Merge branch 'dev' of https://github.com/Bayer-Group/drcHelper into dev.

- Introduced USER_QUESTIONS_ANSWERED.md for detailed answers to user queries regarding validation results and data quality issues.

- Implemented comprehensive_multi_study_analysis.R to analyze multiple studies with Dunnett tests, summarizing results and endpoints.

- Created detailed_individual_analysis.R for in-depth validation of individual function groups, including actual vs expected comparisons.

- Developed detailed_validation_functions.R with enhanced validation functions providing detailed row-by-row results for Dunnett tests.

- Generated final_validation_summary.R to summarize the overall validation results, including success rates and study breakdowns.

- Added working_detailed_analysis.R for a practical demonstration of validation processes and handling of data quality issues.

- Created definitive_multi_study_multi_endpoint_demo.R to showcase the package's capability to handle multiple studies and endpoints effectively.

- Implemented `run_dunnett_validation` function to support multi-endpoint validation.

- Created scripts to check study IDs, data columns, and expected results for function groups.

- Added debugging scripts to investigate issues with expected results and data linking for FG00225.

- Developed tests for individual function groups and multi-endpoint validation.

- Enhanced error handling and output reporting in validation functions.

- Included checks for European decimal notation in dose conversion.

- Established a framework for validating T-statistics, P-values, and means against expected results.

- Created a comprehensive validation summary for the Dunnett Test, detailing data quality issues and actions taken.

- Implemented a test script (`test_dunnett_call.R`) to evaluate the functionality of the `dunnett_test` function with specific study data.

- Developed a validation script (`test_fixed_patterns.R`) to compare actual results against expected values using corrected patterns.

- Added a script (`test_updated_tolerances.R`) to test the Dunnett Test with updated tolerances for t-values and p-values.

- Included data cleaning steps to handle invalid placeholders and ensure proper statistical comparisons.

- Enhanced error handling in test scripts to capture and report issues during validation.

- Implemented a new script to analyze which endpoints have count vs continuous data.

- Added detailed output for each endpoint, including data presence checks for Total, Alive, and Dead columns.

- Developed a corrected validation function that checks count data per endpoint instead of per study.

- Enhanced the function to handle both count and continuous data appropriately, including control level determination.

- Tested the fixed validation logic across multiple function groups with comprehensive output.

- Implemented a comprehensive test script (test_comprehensive.R) to verify the correct matching logic for data, focusing on the Myriophyllum study (MOCK0065) and other studies.

- Created a focused test script (test_matching_issue.R) to demonstrate specific matching problems and the correct logic for handling measurement variables.

- Developed a test script (test_matching_logic.R) to verify the structure of test data and results, ensuring the matching logic is correctly applied.

- Added a validation test script (test_validation.R) to simulate actual testing functions and confirm the matching logic works as intended.

- Enhanced documentation and comments throughout the scripts to clarify the matching rules and logic applied.

- Introduced a comprehensive guide for the Statistical Test Validation Framework in `Statistical_Test_Framework_Guide.Rmd`.

- Created configuration file `test_framework_config.R` to define available statistical tests, their properties, and tolerance settings.

- Implemented report generation script `generate_test_reports.R` for creating validation reports for statistical tests.

- Developed a master template `statistical_test_template.Rmd` for generating detailed validation reports with dynamic content.

- Established a modular design for easy extension and integration of new statistical tests within the framework.

- Merge pull request #20 from Bayer-Group/dev.

  Dev

- Merge pull request #17 from Bayer-Group/dev.

- Merge pull request #13 from Bayer-Group/copilot/fix-7be30736-01c8-4e5e-9ad6-508c41c5d437.

- Merge pull request #11 from Bayer-Group/dev.

  added quantal example data so that page deployment can be run

- Initial CRAN submission preparation.

