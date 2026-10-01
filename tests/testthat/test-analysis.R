res <- run_analysis(data_dir)

test_that("datasets load with the expected shape", {
  expect_equal(nrow(res$college$raw), 777)
  expect_equal(nrow(res$college$data), 775)
  expect_equal(nrow(res$auto$data), 392)
  expect_equal(dim(res$boston$data), c(506, 13))
  expect_false("black" %in% names(res$boston$data))
})

test_that("Boston.csv matches MASS::Boston apart from the black column", {
  skip_if_not_installed("MASS")
  mass <- MASS::Boston
  expect_equal(res$boston$data, mass[names(res$boston$data)],
               ignore_attr = TRUE)
})

test_that("invalid College percentages are excluded, not altered", {
  expect_setequal(res$college$invalid$Name,
                  c("Cazenovia College", "Texas A&M University at Galveston"))
  expect_true(all(res$college$data$Grad.Rate <= 100))
  expect_true(all(res$college$data$PhD <= 100))
  raw <- utils::read.csv(file.path(data_dir, "College.csv"))
  expect_equal(max(raw$Grad.Rate), 118)
})

test_that("train and test sets are disjoint and cover every row", {
  for (d in c("college", "auto", "boston")) {
    ev <- res[[d]]$eval
    expect_length(intersect(rownames(ev$train), rownames(ev$test)), 0)
    expect_equal(nrow(ev$train) + nrow(ev$test), nrow(res[[d]]$data))
  }
})

test_that("comparison tables are complete and every model beats the baseline", {
  for (d in c("college", "auto", "boston")) {
    cmp <- res[[d]]$eval$comparison
    expect_false(anyNA(cmp))
    expect_equal(cmp$Model[1], "Mean baseline")
    expect_true(all(cmp$Test_RMSE[-1] < cmp$Test_RMSE[1]))
    expect_true(all(cmp$CV_RMSE[-1] < cmp$CV_RMSE[1]))
    expect_true(all(cmp$Test_MAE > 0))
  }
})

test_that("the analysis is reproducible run to run", {
  again <- run_analysis(data_dir)
  for (d in c("college", "auto", "boston")) {
    expect_identical(again[[d]]$eval$comparison, res[[d]]$eval$comparison)
  }
})
