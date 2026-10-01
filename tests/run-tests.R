# Run the unit tests. From the project root:
#   Rscript tests/run-tests.R

if (!file.exists(file.path("R", "helpers.R"))) {
  stop("Run tests/run-tests.R from the project root directory.", call. = FALSE)
}
testthat::test_dir(file.path("tests", "testthat"), stop_on_failure = TRUE)
