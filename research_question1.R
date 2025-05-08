
# 1. Install & Load Required Libraries

required_packages <- c(
  "dplyr", "ggplot2", "corrplot", "caret", "randomForest",
  "reshape2", "kableExtra", "xgboost", "Metrics", "tidyr"
)

install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
install_and_load(required_packages)


# 2. Load Data

data <- read.csv(file.choose())  # Choose your CSV file
numeric_columns <- names(data)[sapply(data, is.numeric)]


# 3. Descriptive Statistics

descriptive_stats <- data.frame(Statistic = c("mean", "median", "min", "max", "sd", "variance"))
for (col in numeric_columns) {
  descriptive_stats[[col]] <- c(
    mean(data[[col]], na.rm = TRUE),
    median(data[[col]], na.rm = TRUE),
    min(data[[col]], na.rm = TRUE),
    max(data[[col]], na.rm = TRUE),
    sd(data[[col]], na.rm = TRUE),
    var(data[[col]], na.rm = TRUE)
  )
}
print(kable(descriptive_stats, format = "simple"))


# 4. Distribution Plots

melted_data <- melt(data[, c("popularity", "energy", "danceability", "loudness", "valence", "tempo")])
ggplot(melted_data, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of Key Audio Features", x = "Value", y = "Count")


# 5. Correlation Analysis

corr_mat <- cor(data[, numeric_columns], use = "complete.obs")
corrplot(corr_mat, type = "upper", tl.cex = 0.8, tl.col = "black", title = "Correlation Heatmap", mar = c(0, 0, 1, 0))

pop_cor <- corr_mat["popularity", ]
pop_corr_df <- data.frame(Feature = names(pop_cor), Correlation = pop_cor)
ggplot(pop_corr_df, aes(x = reorder(Feature, Correlation), y = Correlation)) +
  geom_col(fill = "coral") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Correlation of Features with Popularity", x = "Audio Feature", y = "Correlation")

# 6. PCA Visualization

features <- data %>%
  select(danceability, energy, loudness, speechiness, acousticness,
         instrumentalness, liveness, valence, tempo, duration_ms) %>%
  na.omit()

pca_model <- prcomp(features, center = TRUE, scale. = TRUE)
pca_scores <- data.frame(pca_model$x[, 1:2], genre = data$track_genre[complete.cases(data)])
ggplot(pca_scores, aes(x = PC1, y = PC2, color = genre)) +
  geom_point(alpha = 0.5) +
  labs(title = "PCA of Audio Features (by Genre)", x = "PC1", y = "PC2") +
  theme_minimal()


# 7. Modeling: Linear, Random Forest, XGBoost

selected_features <- c("danceability", "energy", "valence", "acousticness", "liveness")
data_model <- data %>% select(popularity, all_of(selected_features)) %>% na.omit()

set.seed(42)
trainIndex <- createDataPartition(data_model$popularity, p = 0.8, list = FALSE)
train_data <- data_model[trainIndex, ]
test_data <- data_model[-trainIndex, ]
formula <- as.formula(paste("popularity ~", paste(selected_features, collapse = " + ")))

# Linear Regression
lm_model <- lm(formula, data = train_data)
lm_preds <- predict(lm_model, test_data)

# Random Forest
rf_model <- randomForest(formula, data = train_data, ntree = 100)
rf_preds <- predict(rf_model, test_data)

# XGBoost
xgb_model <- xgboost(
  data = xgb.DMatrix(data = as.matrix(train_data[, selected_features]), label = train_data$popularity),
  max.depth = 4, eta = 0.1, nrounds = 100, objective = "reg:squarederror", verbose = 0
)
xgb_preds <- predict(xgb_model, as.matrix(test_data[, selected_features]))


# 8. XGBoost (Tuned)

xgb_features <- c(selected_features, "tempo", "speechiness")
xgb_data <- data %>% select(popularity, all_of(xgb_features)) %>% na.omit()
trainIndex <- createDataPartition(xgb_data$popularity, p = 0.8, list = FALSE)
train_data <- xgb_data[trainIndex, ]
test_data <- xgb_data[-trainIndex, ]

ctrl <- trainControl(method = "cv", number = 5)
grid <- expand.grid(
  nrounds = c(50, 100),
  max_depth = c(3, 4, 6),
  eta = c(0.05, 0.1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

xgb_tuned <- train(
  x = train_data[, xgb_features], y = train_data$popularity,
  method = "xgbTree", trControl = ctrl, tuneGrid = grid, verbose = FALSE
)
xgb_preds_tuned <- predict(xgb_tuned, test_data)


# 9. Evaluation Metrics

actual <- test_data$popularity
metrics <- data.frame(
  Model = c("Linear Regression", "Random Forest", "XGBoost", "Tuned XGBoost"),
  RMSE = c(rmse(actual, lm_preds), rmse(actual, rf_preds), rmse(actual, xgb_preds), rmse(actual, xgb_preds_tuned)),
  MAE = c(mae(actual, lm_preds), mae(actual, rf_preds), mae(actual, xgb_preds), mae(actual, xgb_preds_tuned)),
  R_squared = c(cor(actual, lm_preds)^2, cor(actual, rf_preds)^2, cor(actual, xgb_preds)^2, cor(actual, xgb_preds_tuned)^2),
  Correlation = c(cor(actual, lm_preds), cor(actual, rf_preds), cor(actual, xgb_preds), cor(actual, xgb_preds_tuned))
)
print(metrics)


# 10. Log-Transformed Models

data$log_popularity <- log1p(data$popularity)
log_data <- data %>% select(log_popularity, all_of(selected_features)) %>% na.omit()
trainIndex <- createDataPartition(log_data$log_popularity, p = 0.8, list = FALSE)
train_data <- log_data[trainIndex, ]
test_data <- log_data[-trainIndex, ]
true_vals <- expm1(test_data$log_popularity)

# Log-LM
lm_log <- lm(log_popularity ~ ., data = train_data)
lm_log_preds <- expm1(predict(lm_log, test_data))

# Log-RF
rf_log <- randomForest(log_popularity ~ ., data = train_data, ntree = 100)
rf_log_preds <- expm1(predict(rf_log, test_data))

# Log-XGB
xgb_log <- xgboost(
  data = xgb.DMatrix(data = as.matrix(train_data[, selected_features]), label = train_data$log_popularity),
  max.depth = 4, eta = 0.1, nrounds = 100, objective = "reg:squarederror", verbose = 0
)
xgb_log_preds <- expm1(predict(xgb_log, as.matrix(test_data[, selected_features])))

log_metrics <- data.frame(
  Model = c("Log-LM", "Log-RF", "Log-XGB"),
  RMSE = c(rmse(true_vals, lm_log_preds), rmse(true_vals, rf_log_preds), rmse(true_vals, xgb_log_preds)),
  MAE = c(mae(true_vals, lm_log_preds), mae(true_vals, rf_log_preds), mae(true_vals, xgb_log_preds)),
  R_squared = c(cor(true_vals, lm_log_preds)^2, cor(true_vals, rf_log_preds)^2, cor(true_vals, xgb_log_preds)^2),
  Correlation = c(cor(true_vals, lm_log_preds), cor(true_vals, rf_log_preds), cor(true_vals, xgb_log_preds))
)
print(log_metrics)


# 11. Combined Model Performance Table

all_metrics <- rbind(metrics, log_metrics)
print(all_metrics)


# 12. Visualization: Actual vs Predicted

# For standard models
actual_preds_df <- data.frame(
  Actual = actual,
  Linear_Regression = lm_preds,
  Random_Forest = rf_preds,
  XGBoost = xgb_preds,
  Tuned_XGBoost = xgb_preds_tuned
)
actual_preds_long <- pivot_longer(actual_preds_df, cols = -Actual, names_to = "Model", values_to = "Predicted")

ggplot(actual_preds_long, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "#4682B4") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ Model, scales = "free") +
  theme_minimal() +
  labs(title = "Actual vs Predicted Popularity", x = "Actual", y = "Predicted")

# For log-transformed models
log_pred_df <- data.frame(
  Actual = true_vals,
  Log_LM = lm_log_preds,
  Log_RF = rf_log_preds,
  Log_XGB = xgb_log_preds
)
log_preds_long <- pivot_longer(log_pred_df, cols = -Actual, names_to = "Model", values_to = "Predicted")

ggplot(log_preds_long, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "#4682B4") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ Model, scales = "free") +
  theme_minimal() +
  labs(title = "Actual vs Predicted Popularity (Log-Transformed)", x = "Actual", y = "Predicted")
