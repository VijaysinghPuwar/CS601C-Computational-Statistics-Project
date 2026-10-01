# Helper functions shared by analysis/report.Rmd and the unit tests.
# Base R plus ggplot2, glmnet and rpart. No function here touches files
# outside the paths it is given.

# ---------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------

required_packages <- c("ggplot2", "glmnet", "rpart", "knitr", "rmarkdown")

# Stop early, with instructions, if a package the analysis needs is missing.
check_packages <- function(pkgs = required_packages) {
  installed <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (!all(installed)) {
    stop(sprintf(paste("Missing R package(s): %s. Run renv::restore() from",
                       "the project root to install the pinned versions."),
                 paste(pkgs[!installed], collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Data loading and validation
# ---------------------------------------------------------------------------

# Stop with a clear message if a data frame is missing columns or has NAs.
validate_dataset <- function(df, required, name) {
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("%s is missing required column(s): %s",
                 name, paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  na_counts <- colSums(is.na(df[required]))
  if (any(na_counts > 0)) {
    bad <- na_counts[na_counts > 0]
    stop(sprintf("%s has missing values in: %s", name,
                 paste(sprintf("%s (%d)", names(bad), bad), collapse = ", ")),
         call. = FALSE)
  }
  non_numeric <- required[!vapply(df[required], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop(sprintf("%s has non-numeric values in: %s",
                 name, paste(non_numeric, collapse = ", ")), call. = FALSE)
  }
  invisible(df)
}

read_checked_csv <- function(path, ...) {
  if (!file.exists(path)) {
    stop(sprintf("Data file not found: %s (run from the project root)", path),
         call. = FALSE)
  }
  utils::read.csv(path, ...)
}

college_numeric_cols <- c(
  "Apps", "Accept", "Enroll", "Top10perc", "Top25perc", "F.Undergrad",
  "P.Undergrad", "Outstate", "Room.Board", "Books", "Personal", "PhD",
  "Terminal", "S.F.Ratio", "perc.alumni", "Expend", "Grad.Rate"
)

# Columns in College that are percentages and must lie in [0, 100].
college_percent_cols <- c("Top10perc", "Top25perc", "PhD", "Terminal",
                          "perc.alumni", "Grad.Rate")

read_college <- function(path) {
  df <- read_checked_csv(path, check.names = TRUE)
  names(df)[1] <- "Name"
  validate_dataset(df, college_numeric_cols, "College")
  if (!all(df$Private %in% c("Yes", "No"))) {
    stop("College$Private must contain only 'Yes' or 'No'", call. = FALSE)
  }
  df$Private <- factor(df$Private, levels = c("No", "Yes"))
  df
}

auto_numeric_cols <- c("mpg", "cylinders", "displacement", "horsepower",
                       "weight", "acceleration", "year", "origin")

read_auto <- function(path) {
  # The raw ISLR file marks missing horsepower as "?". This copy has those
  # rows removed already, but treat "?" as NA so validation would catch them.
  df <- read_checked_csv(path, na.strings = "?")
  validate_dataset(df, auto_numeric_cols, "Auto")
  if (!all(df$origin %in% 1:3)) {
    stop("Auto$origin must be coded 1, 2 or 3", call. = FALSE)
  }
  df$origin <- factor(df$origin, levels = 1:3,
                      labels = c("American", "European", "Japanese"))
  df
}

boston_numeric_cols <- c("crim", "zn", "indus", "chas", "nox", "rm", "age",
                         "dis", "rad", "tax", "ptratio", "lstat", "medv")

read_boston <- function(path) {
  df <- read_checked_csv(path, row.names = 1)
  validate_dataset(df, boston_numeric_cols, "Boston")
  df
}

# Logical vector: TRUE where any of `cols` falls outside [0, 100].
invalid_percent_rows <- function(df, cols) {
  Reduce(`|`, lapply(df[cols], function(x) x < 0 | x > 100))
}

# ---------------------------------------------------------------------------
# Reproducible resampling
# ---------------------------------------------------------------------------

# Evaluate `expr` with a fixed seed without disturbing the caller's RNG state.
with_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = globalenv())
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  })
  set.seed(seed)
  expr
}

# Row indices of a training set holding `prop` of n rows.
train_indices <- function(n, prop, seed) {
  stopifnot(n >= 2, prop > 0, prop < 1)
  with_seed(seed, sort(sample.int(n, size = floor(prop * n))))
}

# Balanced fold labels 1..k in random order.
make_folds <- function(n, k, seed) {
  stopifnot(k >= 2, n >= k)
  with_seed(seed, sample(rep_len(seq_len(k), n)))
}

# ---------------------------------------------------------------------------
# Metrics
# ---------------------------------------------------------------------------

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae <- function(actual, predicted) mean(abs(actual - predicted))

# Out-of-sample R squared: 1 - SSE / SST, with SST taken around the mean of
# `actual`. It is negative when a model does worse than predicting that mean.
r_squared <- function(actual, predicted) {
  1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
}

regression_metrics <- function(actual, predicted) {
  stopifnot(length(actual) == length(predicted), !anyNA(predicted))
  c(RMSE = rmse(actual, predicted), MAE = mae(actual, predicted),
    R2 = r_squared(actual, predicted))
}

# ---------------------------------------------------------------------------
# Multicollinearity
# ---------------------------------------------------------------------------

# Variance inflation factor of each column of a numeric data frame:
# VIF_j = 1 / (1 - R2_j), where R2_j comes from regressing predictor j on
# the other predictors. It measures how much predictor j's coefficient
# variance is inflated by collinearity. It is not a measure of importance.
vif_values <- function(predictors) {
  stopifnot(ncol(predictors) >= 2)
  vapply(names(predictors), function(v) {
    others <- setdiff(names(predictors), v)
    r2 <- summary(stats::lm(stats::reformulate(others, v),
                            data = predictors))$r.squared
    1 / (1 - r2)
  }, numeric(1))
}

# ---------------------------------------------------------------------------
# Model fitters
#
# Each fitter takes a training data frame and returns a function that maps
# new data to numeric predictions, so every model is evaluated the same way.
# ---------------------------------------------------------------------------

fit_mean_baseline <- function(response) {
  function(train) {
    mu <- mean(train[[response]])
    function(newdata) rep(mu, nrow(newdata))
  }
}

# Linear model. When the formula's response is transformed (for example
# log(mpg)), `inverse` maps predictions back to the original scale.
fit_linear <- function(formula, inverse = identity) {
  function(train) {
    model <- stats::lm(formula, data = train)
    function(newdata) inverse(unname(stats::predict(model, newdata)))
  }
}

# Lasso with the penalty chosen by 10-fold CV inside the training data.
# Uses lambda.1se, the largest penalty within one standard error of the best.
fit_lasso <- function(formula, seed, nfolds = 10) {
  function(train) {
    x <- stats::model.matrix(formula, train)[, -1, drop = FALSE]
    y <- stats::model.response(stats::model.frame(formula, train))
    foldid <- make_folds(nrow(x), nfolds, seed)
    cv <- glmnet::cv.glmnet(x, y, alpha = 1, foldid = foldid)
    terms_rhs <- stats::delete.response(stats::terms(formula))
    function(newdata) {
      newx <- stats::model.matrix(terms_rhs, newdata)[, -1, drop = FALSE]
      as.numeric(stats::predict(cv, newx = newx, s = "lambda.1se"))
    }
  }
}

# Regression tree grown with rpart, then pruned to the complexity parameter
# with the lowest cross-validated error.
fit_tree <- function(formula, seed, nfolds = 10) {
  function(train) {
    foldid <- make_folds(nrow(train), nfolds, seed)
    tree <- rpart::rpart(formula, data = train, method = "anova",
                         control = rpart::rpart.control(cp = 0.001,
                                                        xval = foldid))
    best_cp <- tree$cptable[which.min(tree$cptable[, "xerror"]), "CP"]
    pruned <- rpart::prune(tree, cp = best_cp)
    function(newdata) unname(stats::predict(pruned, newdata))
  }
}

# ---------------------------------------------------------------------------
# Evaluation
# ---------------------------------------------------------------------------

# k-fold cross-validated RMSE of a fitter on the training data.
cv_rmse <- function(fitter, train, response, folds) {
  stopifnot(length(folds) == nrow(train))
  sq_err <- numeric(nrow(train))
  for (k in sort(unique(folds))) {
    hold <- folds == k
    predict_fn <- fitter(train[!hold, , drop = FALSE])
    sq_err[hold] <- (train[[response]][hold] -
                       predict_fn(train[hold, , drop = FALSE]))^2
  }
  sqrt(mean(sq_err))
}

# Fit every model on the training set, report CV RMSE on the training set
# and RMSE / MAE / R squared on the untouched test set.
compare_models <- function(models, train, test, response, folds) {
  rows <- lapply(names(models), function(label) {
    fitter <- models[[label]]
    predict_fn <- fitter(train)
    test_metrics <- regression_metrics(test[[response]], predict_fn(test))
    data.frame(
      Model = label,
      CV_RMSE = cv_rmse(fitter, train, response, folds),
      Test_RMSE = test_metrics[["RMSE"]],
      Test_MAE = test_metrics[["MAE"]],
      Test_R2 = test_metrics[["R2"]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Plotting
# ---------------------------------------------------------------------------

palette_main <- c(primary = "#2C6E91", secondary = "#D08C3A",
                  neutral = "#7A7A7A", light = "#BFD3DF")

theme_report <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(colour = "grey40", hjust = 0),
      plot.caption.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      strip.text = ggplot2::element_text(face = "bold")
    )
}

# Residuals vs fitted, normal Q-Q and scale-location panels for an lm fit,
# drawn as one faceted ggplot.
diagnostic_data <- function(model) {
  std_res <- stats::rstandard(model)
  fitted_vals <- stats::fitted(model)
  qq <- stats::qqnorm(std_res, plot.it = FALSE)
  panels <- c("Residuals vs fitted", "Normal Q-Q", "Scale-location")
  data.frame(
    panel = factor(rep(panels, each = length(std_res)), levels = panels),
    x = c(fitted_vals, qq$x, fitted_vals),
    y = c(stats::residuals(model), qq$y, sqrt(abs(std_res)))
  )
}

plot_diagnostics <- function(model, title) {
  d <- diagnostic_data(model)
  smooth_d <- d[d$panel != "Normal Q-Q", ]
  ref <- data.frame(
    panel = factor(c("Residuals vs fitted", "Normal Q-Q"),
                   levels = levels(d$panel)),
    intercept = c(0, 0), slope = c(0, 1)
  )
  ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_abline(data = ref,
                         ggplot2::aes(intercept = intercept, slope = slope),
                         colour = palette_main[["neutral"]],
                         linetype = "dashed") +
    ggplot2::geom_point(alpha = 0.45, size = 1.2,
                        colour = palette_main[["primary"]]) +
    ggplot2::geom_smooth(data = smooth_d, method = "loess", formula = y ~ x,
                         se = FALSE, colour = palette_main[["secondary"]],
                         linewidth = 0.9) +
    ggplot2::facet_wrap(~panel, scales = "free", nrow = 1) +
    ggplot2::labs(title = title, x = NULL, y = NULL,
                  caption = paste("Left: residuals vs fitted values.",
                                  "Middle: standardized residuals vs normal",
                                  "quantiles. Right: sqrt(|standardized",
                                  "residual|) vs fitted values.")) +
    theme_report(base_size = 11)
}

# Correlation matrix as a ggplot heatmap (lower triangle).
plot_correlation <- function(df, title) {
  cm <- stats::cor(df)
  keep <- lower.tri(cm, diag = TRUE)
  d <- data.frame(
    row = factor(rownames(cm)[row(cm)[keep]], levels = rev(colnames(cm))),
    col = factor(colnames(cm)[col(cm)[keep]], levels = colnames(cm)),
    r = cm[keep]
  )
  ggplot2::ggplot(d, ggplot2::aes(col, row, fill = r)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", r)), size = 3) +
    ggplot2::scale_fill_gradient2(low = palette_main[["secondary"]],
                                  mid = "white",
                                  high = palette_main[["primary"]],
                                  limits = c(-1, 1), name = "Correlation") +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    theme_report(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1),
                   panel.grid = ggplot2::element_blank())
}
