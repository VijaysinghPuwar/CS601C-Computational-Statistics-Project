# The README quotes numbers from the analysis. These tests fail if a change
# to the data or models makes any of those numbers stale.

readme <- readLines(file.path(project_root, "README.md"), encoding = "UTF-8")
res <- run_analysis(data_dir)
f2 <- function(x) sprintf("%.2f", x)

test_that("every row of the README results table matches the analysis", {
  rows <- grep("^\\| (Auto|Boston|College) \\(`", readme, value = TRUE)
  expect_equal(length(rows), 9)
  keys <- c(Auto = "auto", Boston = "boston", College = "college")
  for (row in rows) {
    cells <- trimws(strsplit(row, "|", fixed = TRUE)[[1]])[-1]
    cmp <- res[[keys[[sub(" .*", "", cells[1])]]]]$eval$comparison
    hit <- cmp[cmp$Model == cells[2], ]
    expect_equal(nrow(hit), 1, info = row)
    expected <- f2(unlist(hit[, c("CV_RMSE", "Test_RMSE", "Test_MAE",
                                  "Test_R2")]))
    expected[expected == "-0.00"] <- "0.00"
    expect_identical(cells[3:6], unname(expected), info = row)
  }
})

test_that("numbers quoted in the README key findings match the analysis", {
  text <- paste(readme, collapse = " ")
  auto <- res$auto$eval$comparison
  bos <- res$boston$eval$comparison
  col <- res$college$eval$comparison
  sw <- function(m) unname(stats::shapiro.test(stats::rstandard(m))$statistic)
  p_chas <- summary(res$boston$crime_log)$coefficients["chas", "Pr(>|t|)"]
  quoted <- c(
    f2(auto$Test_RMSE[auto$Model == "OLS, log(mpg) ~ weight + year + origin"]),
    f2(auto$Test_RMSE[1]),
    f2(auto$Test_R2[auto$Model == "OLS, log(mpg) ~ weight + year + origin"]),
    f2(auto$Test_RMSE[auto$Model == "OLS, 4 coursework predictors"]),
    sprintf("%.1f", max(res$auto$vif_size)),
    f2(min(bos$Test_RMSE)),
    f2(bos$Test_RMSE[bos$Model == "OLS, all 12 predictors"]),
    f2(bos$Test_RMSE[1]),
    f2(sw(res$boston$crime_raw)), f2(sw(res$boston$crime_log)),
    f2(p_chas), f2(max(col$Test_R2))
  )
  for (q in quoted) expect_true(grepl(q, text, fixed = TRUE), info = q)
  expect_true(grepl("between 79 and 155|79 to 155", text))
  expect_identical(range(c(nrow(res$auto$eval$test),
                           nrow(res$college$eval$test))), c(79L, 155L))
  expect_identical(bos$Model[which.min(bos$Test_RMSE)],
                   "Regression tree, all 12 predictors")
})

test_that("README links point at files that exist", {
  links <- unlist(regmatches(readme, gregexpr("\\]\\(([^)]+)\\)", readme)))
  targets <- sub("^\\]\\(([^)]+)\\)$", "\\1", links)
  local <- targets[!grepl("^https?://", targets)]
  expect_gt(length(local), 0)
  for (t in local) {
    expect_true(file.exists(file.path(project_root, t)), info = t)
  }
})
