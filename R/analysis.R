# Full analysis pipeline. run_analysis() loads the raw CSVs, applies the
# documented cleaning steps, fits every model and returns all results in one
# list. The report only presents what this function returns, so every number
# in the report comes from a single run.

analysis_settings <- list(
  train_prop = 0.8,  # share of rows used for fitting and tuning
  split_seed = 601,  # seed for the train/test split
  cv_folds = 10,     # folds for cross-validation on the training set
  cv_seed = 2025,    # seed for fold assignment (shared by all models)
  tune_seed = 7      # seed for the inner folds used by lasso and tree tuning
)

college_predictors <- setdiff(college_numeric_cols, "Grad.Rate")
auto_predictors <- c("cylinders", "displacement", "horsepower", "weight",
                     "acceleration", "year", "origin")
boston_predictors <- setdiff(boston_numeric_cols, "medv")

# Fit `models` on one train/test split of `data` and compare them.
evaluate_dataset <- function(data, response, models, settings) {
  idx <- train_indices(nrow(data), settings$train_prop, settings$split_seed)
  train <- data[idx, , drop = FALSE]
  test <- data[-idx, , drop = FALSE]
  folds <- make_folds(nrow(train), settings$cv_folds, settings$cv_seed)
  list(
    train = train,
    test = test,
    comparison = compare_models(models, train, test, response, folds)
  )
}

run_analysis <- function(data_dir = "data", settings = analysis_settings) {
  check_packages()
  tune <- settings$tune_seed

  # College ---------------------------------------------------------------
  college_raw <- read_college(file.path(data_dir, "College.csv"))
  college_raw$Elite <- factor(ifelse(college_raw$Top10perc > 50, "Yes", "No"),
                              levels = c("No", "Yes"))
  college_invalid <- invalid_percent_rows(college_raw, college_percent_cols)
  college <- college_raw[!college_invalid, ]

  college_all <- stats::reformulate(college_predictors, "Grad.Rate")
  college_course <- Grad.Rate ~ Outstate + Top10perc + Expend + S.F.Ratio
  college_eval <- evaluate_dataset(college, "Grad.Rate", list(
    "Mean baseline" = fit_mean_baseline("Grad.Rate"),
    "OLS, 4 coursework predictors" = fit_linear(college_course),
    "OLS, all 17 predictors" = fit_linear(college_all),
    "Lasso, all 17 predictors" = fit_lasso(college_all, tune),
    "Regression tree, all 17 predictors" = fit_tree(college_all, tune)
  ), settings)
  college_lm <- stats::lm(college_all, data = college_eval$train)

  # Auto --------------------------------------------------------------------
  auto <- read_auto(file.path(data_dir, "Auto.csv"))
  auto_all <- stats::reformulate(auto_predictors, "mpg")
  auto_course <- mpg ~ horsepower + weight + displacement + acceleration
  auto_reduced <- mpg ~ weight + year + origin
  auto_log <- log(mpg) ~ weight + year + origin
  auto_eval <- evaluate_dataset(auto, "mpg", list(
    "Mean baseline" = fit_mean_baseline("mpg"),
    "OLS, 4 coursework predictors" = fit_linear(auto_course),
    "OLS, weight + year + origin" = fit_linear(auto_reduced),
    "OLS, log(mpg) ~ weight + year + origin" = fit_linear(auto_log,
                                                          inverse = exp),
    "Lasso, all 7 predictors" = fit_lasso(auto_all, tune),
    "Regression tree, all 7 predictors" = fit_tree(auto_all, tune)
  ), settings)

  # Boston: housing values ------------------------------------------------
  boston <- read_boston(file.path(data_dir, "Boston.csv"))
  boston_all <- stats::reformulate(boston_predictors, "medv")
  boston_eval <- evaluate_dataset(boston, "medv", list(
    "Mean baseline" = fit_mean_baseline("medv"),
    "OLS, all 12 predictors" = fit_linear(boston_all),
    "Lasso, all 12 predictors" = fit_lasso(boston_all, tune),
    "Regression tree, all 12 predictors" = fit_tree(boston_all, tune)
  ), settings)

  # Boston: crime rate (association, fitted on all rows) ------------------
  crime_rhs <- ~ tax + ptratio + lstat + rm + chas
  crime_raw <- stats::lm(stats::update(crime_rhs, crim ~ .), data = boston)
  crime_log <- stats::lm(stats::update(crime_rhs, log1p(crim) ~ .),
                         data = boston)

  list(
    settings = settings,
    college = list(
      raw = college_raw,
      data = college,
      invalid = college_raw[college_invalid, ],
      eval = college_eval,
      lm_course = stats::lm(college_course, data = college_eval$train),
      lm_all = college_lm,
      vif = vif_values(college_eval$train[college_predictors])
    ),
    auto = list(
      data = auto, eval = auto_eval,
      lm_course = stats::lm(auto_course, data = auto_eval$train),
      lm_reduced = stats::lm(auto_reduced, data = auto_eval$train),
      lm_log = stats::lm(auto_log, data = auto_eval$train),
      vif_course = vif_values(auto[c("horsepower", "weight", "displacement",
                                     "acceleration")]),
      vif_size = vif_values(auto[c("cylinders", "displacement", "horsepower",
                                   "weight")])
    ),
    boston = list(
      data = boston, eval = boston_eval,
      lm_all = stats::lm(boston_all, data = boston_eval$train),
      crime_raw = crime_raw, crime_log = crime_log
    )
  )
}
