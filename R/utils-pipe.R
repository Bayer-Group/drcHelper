# Utility: suppress R CMD check NOTEs for non-standard evaluation symbols
# (e.g., `.` used in purrr/dplyr formula notation)
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."), add = TRUE)
}
