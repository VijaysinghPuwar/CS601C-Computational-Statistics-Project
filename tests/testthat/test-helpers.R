test_that("check_packages names missing packages and how to fix it", {
  expect_true(check_packages("stats"))
  expect_error(check_packages(c("stats", "notARealPackage123")),
               "notARealPackage123.*renv::restore")
})

test_that("regression metrics match hand calculations", {
  actual <- c(1, 2, 3, 4)
  predicted <- c(1, 2, 4, 6)
  expect_equal(rmse(actual, predicted), sqrt((0 + 0 + 1 + 4) / 4))
  expect_equal(mae(actual, predicted), (0 + 0 + 1 + 2) / 4)
  expect_equal(r_squared(actual, predicted), 1 - 5 / 5)
  expect_equal(r_squared(actual, actual), 1)
  expect_equal(r_squared(actual, rep(mean(actual), 4)), 0)
  expect_lt(r_squared(actual, rev(actual)), 0)
  expect_named(regression_metrics(actual, predicted), c("RMSE", "MAE", "R2"))
})

test_that("regression_metrics rejects mismatched or missing predictions", {
  expect_error(regression_metrics(1:3, 1:2))
  expect_error(regression_metrics(1:3, c(1, NA, 3)))
})

test_that("train_indices and make_folds are deterministic and well formed", {
  idx <- train_indices(100, 0.8, seed = 1)
  expect_identical(idx, train_indices(100, 0.8, seed = 1))
  expect_length(idx, 80)
  expect_false(anyDuplicated(idx) > 0)
  expect_true(all(idx >= 1 & idx <= 100))
  expect_false(identical(idx, train_indices(100, 0.8, seed = 2)))

  folds <- make_folds(103, 10, seed = 1)
  expect_identical(folds, make_folds(103, 10, seed = 1))
  expect_setequal(unique(folds), 1:10)
  expect_lte(diff(range(table(folds))), 1)
})

test_that("seeded helpers do not change the caller's random stream", {
  set.seed(42)
  expected <- runif(3)
  set.seed(42)
  invisible(train_indices(50, 0.5, seed = 99))
  invisible(make_folds(50, 5, seed = 99))
  expect_identical(runif(3), expected)
})

test_that("vif_values matches the inverse correlation matrix definition", {
  set.seed(1)
  x1 <- rnorm(200)
  df <- data.frame(x1 = x1, x2 = x1 + rnorm(200, sd = 0.3), x3 = rnorm(200))
  expected <- diag(solve(stats::cor(df)))
  expect_equal(unname(vif_values(df)), unname(expected), tolerance = 1e-8)
  expect_gt(vif_values(df)[["x1"]], 5)
  expect_lt(vif_values(df)[["x3"]], 1.1)
})

test_that("validate_dataset rejects missing columns, NAs and text", {
  df <- data.frame(a = 1:3, b = c(1, 2, 3))
  expect_silent(validate_dataset(df, c("a", "b"), "toy"))
  expect_error(validate_dataset(df, c("a", "z"), "toy"), "missing required")
  df_na <- df
  df_na$b[2] <- NA
  expect_error(validate_dataset(df_na, c("a", "b"), "toy"), "missing values")
  df_chr <- df
  df_chr$b <- c("1", "x", "3")
  expect_error(validate_dataset(df_chr, c("a", "b"), "toy"), "non-numeric")
})

test_that("loaders fail clearly on missing files and corrupted data", {
  expect_error(read_auto(file.path(tempdir(), "nope.csv")), "not found")

  auto <- utils::read.csv(file.path(data_dir, "Auto.csv"))
  auto$horsepower <- as.character(auto$horsepower)
  auto$horsepower[5] <- "?"
  expect_error(read_auto(temp_csv(auto, row.names = FALSE)),
               "missing values in: horsepower")

  auto_bad_origin <- utils::read.csv(file.path(data_dir, "Auto.csv"))
  auto_bad_origin$origin[1] <- 4
  expect_error(read_auto(temp_csv(auto_bad_origin, row.names = FALSE)),
               "origin")

  boston <- utils::read.csv(file.path(data_dir, "Boston.csv"), row.names = 1)
  expect_error(read_boston(temp_csv(boston[, names(boston) != "medv"])),
               "missing required column")

  college <- utils::read.csv(file.path(data_dir, "College.csv"))
  college$Private[1] <- "Maybe"
  expect_error(read_college(temp_csv(college, row.names = FALSE)), "Private")
})

test_that("invalid_percent_rows flags values outside 0 to 100", {
  df <- data.frame(p = c(0, 50, 100, 101, -1), q = c(10, 10, 10, 10, 10))
  expect_identical(invalid_percent_rows(df, c("p", "q")),
                   c(FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("fitters predict as expected", {
  train <- data.frame(x = 1:20, y = 2 * (1:20) + 1)
  new <- data.frame(x = c(21, 22))
  expect_equal(fit_mean_baseline("y")(train)(new), rep(mean(train$y), 2))
  expect_equal(fit_linear(y ~ x)(train)(new), c(43, 45))
  expect_equal(fit_linear(log(y) ~ x, inverse = exp)(train)(new),
               exp(unname(predict(lm(log(y) ~ x, train), new))))
  set.seed(3)
  train2 <- data.frame(x1 = rnorm(80), x2 = rnorm(80))
  train2$y <- 3 * train2$x1 + rnorm(80, sd = 0.1)
  lasso_pred <- fit_lasso(y ~ x1 + x2, seed = 1)(train2)(train2)
  tree_pred <- fit_tree(y ~ x1 + x2, seed = 1)(train2)(train2)
  expect_length(lasso_pred, 80)
  expect_length(tree_pred, 80)
  expect_gt(stats::cor(lasso_pred, train2$y), 0.99)
  expect_identical(lasso_pred, fit_lasso(y ~ x1 + x2, seed = 1)(train2)(train2))
  expect_identical(tree_pred, fit_tree(y ~ x1 + x2, seed = 1)(train2)(train2))
})

test_that("cv_rmse of the mean baseline matches a manual calculation", {
  df <- data.frame(y = c(1, 3, 5, 7, 9, 11))
  folds <- c(1, 1, 2, 2, 3, 3)
  manual <- unlist(lapply(1:3, function(k) {
    df$y[folds == k] - mean(df$y[folds != k])
  }))
  expect_equal(cv_rmse(fit_mean_baseline("y"), df, "y", folds),
               sqrt(mean(manual^2)))
})
