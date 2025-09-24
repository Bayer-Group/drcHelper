---
applyTo: '**/*.md'
---

# Project Context and Coding Guidelines
This project is an R-package dessigned to facilitate the analysis of dose-response data. The package provides functions for data preprocessing, visualization, dose-response model fitting, NOEC calculations. 
The package is intended for use by researchers and practitioners in toxicology, pharmacology, and related fields.

## Coding Guidelines
1. **Language**: All code should be written in R, following the tidyverse style guide.
2. **Documentation**: Use Roxygen2 for documenting functions, including descriptions, parameters, return values, and examples.
3. **Testing**: Implement unit tests using the testthat package to ensure code reliability and correctness. Use describe and it blocks for clarity.
4. **Version Control**: Use Git for version control, with clear and descriptive commit messages.
5. **Code Style**: Follow consistent naming conventions (snake_case for variables and functions), indentation, and spacing.
6. **Dependencies**: Minimize external dependencies and ensure all required packages are listed in the DESCRIPTION file.
7. **Error Handling**: Implement robust error handling and input validation to ensure functions behave predictably.
8. **Performance**: Optimize code for performance, especially for large datasets, while maintaining readability.
9. **Collaboration**: Encourage code reviews and collaborative development practices to maintain code quality.
10. **Licensing**: Ensure all code complies with the project's licensing terms (GPL-3).
11. **Data Privacy**: Ensure that any data used or shared complies with relevant data privacy regulations and guidelines.
12. **Continuous Integration**: Set up CI/CD pipelines to automate testing and deployment processes.
13. **Examples**: Provide clear and concise examples in the documentation to illustrate function usage.
14. **Changelog**: Maintain a changelog to document significant changes, enhancements, and bug fixes.
15. **Community Standards**: Adhere to community standards and best practices for R package development.
16. **Sustainability**: Write code that is maintainable and easy to understand for future developers.
17. **Reproducibility**: Ensure that analyses and results can be reproduced by others using the package.


## Project-Specific Context
1. **Dose-Response Models**: Familiarize yourself with common dose-response models (e.g., logistic, probit) and their applications in toxicology.
2. **NOEC Calculations**: Understand the methodologies for calculating No Observed Effect Concentrations (NOEC) and their significance in risk assessment.
3. **Data Formats**: Be aware of common data formats used in dose-response studies and ensure compatibility with the package functions.
4. **Visualization**: Utilize ggplot2 for creating informative and publication-quality visualizations of dose-response data.
5. **User Base**: Consider the needs and expertise of the target user base, which may include researchers with varying levels of statistical knowledge.
6. **Regulatory Standards**: Be aware of relevant regulatory standards and guidelines that may impact the analysis and interpretation of dose-response data.
7. **Interdisciplinary Collaboration**: Recognize that users may come from diverse scientific backgrounds and ensure the package is accessible to a broad audience.
8. **Updates and Maintenance**: Plan for regular updates to the package to incorporate new methodologies, address user feedback, and ensure compatibility with evolving R standards.
9. **Educational Resources**: Consider providing tutorials, vignettes, or other educational resources to help users understand dose-response analysis concepts and effectively utilize the package.


When generating code, answering questions, or reviewing changes, please adhere to these guidelines and context to ensure consistency and quality across the project. Please clean up any temporary file, comments or notes before finalizing the code. Keep the code efficient, readable, and well-documented.