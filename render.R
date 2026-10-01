# Render the analysis report to _site/index.html and refresh the README
# figures in assets/. Run from the project root:
#   Rscript render.R

if (!file.exists(file.path("R", "helpers.R"))) {
  stop("Run render.R from the project root directory.", call. = FALSE)
}

source(file.path("R", "helpers.R"))
check_packages()

rmarkdown::render(
  input = file.path("analysis", "report.Rmd"),
  output_file = "index.html",
  output_dir = "_site",
  quiet = TRUE
)
message("Report written to ", file.path("_site", "index.html"))
