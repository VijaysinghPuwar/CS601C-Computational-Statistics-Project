# testthat runs helper files first, from tests/testthat.
project_root <- normalizePath(file.path("..", ".."))
source(file.path(project_root, "R", "helpers.R"))
source(file.path(project_root, "R", "analysis.R"))
data_dir <- file.path(project_root, "data")

# Write a data frame to a temporary CSV and return the path.
temp_csv <- function(df, ...) {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(df, path, ...)
  path
}
