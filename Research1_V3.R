# Install necessary packages (run only once)
install.packages(c("gbm", "dplyr", "magrittr", "ranger", "reshape2"))

# Load required libraries
library(caret)
library(glmnet)
library(ranger)         # Faster Random Forest
library(gbm)
library(dplyr)
library(ggplot2)
library(reshape2)

# 1. Read in the cleaned data
data <- read.csv("/Users/trisharane/Cleaned_SpotifyTrack.csv", stringsAsFactors = FALSE)

# 2. Select only the audio features shown to contribute to popularity
data <- data %>%
  select(popularity, energy, danceability, valence, tempo)

# 3. Impute any missing values with the column mean
data <- data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# 4. Split into training (80%) and test (20%) sets
set.seed(123)
train_idx  <- createDataPartition(data$popularity, p = 0.8, list = FALSE)
train_data <- data[train_idx, ]
test_data  <- data[-train_idx, ]

# 5. Set up 5-fold cross‐validation
ctrl <- trainControl(method = "cv", number = 5)

# 6. Define the modeling formula
fm <- popularity ~ energy + danceability + valence + tempo

# 7a. Ordinary least squares
lm_mod <- train(fm, data = train_data, method = "lm", trControl = ctrl)

# 7b. Ridge regression (alpha = 0) with scaling
ridge_mod <- train(
  fm, data = train_data, method = "glmnet", trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 1, length = 10))
)

# 7c. Lasso regression (alpha = 1) with scaling
lasso_mod <- train(
  fm, data = train_data, method = "glmnet", trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 1, length = 10))
)

# 7d. Random Forest using ranger (fast)
rf_mod <- train(
  fm, data = train_data, method = "ranger", trControl = ctrl,
  tuneGrid = data.frame(mtry = 2, splitrule = "variance", min.node.size = 5)
)

# 7e. Gradient boosting
gbm_mod <- train(
  fm, data = train_data, method = "gbm", trControl = ctrl, verbose = FALSE,
  tuneGrid = expand.grid(
    n.trees = c(100, 200),
    interaction.depth = c(1, 3),
    shrinkage = c(0.01, 0.1),
    n.minobsinnode = 10
  )
)

# 8. Evaluate all models on the test set
models <- list(
  Linear       = lm_mod,
  Ridge        = ridge_mod,
  Lasso        = lasso_mod,
  RandomForest = rf_mod,
  GBM          = gbm_mod
)

results <- lapply(models, function(mod) {
  preds <- predict(mod, newdata = test_data)
  rmse  <- sqrt(mean((preds - test_data$popularity)^2))
  r2    <- cor(preds, test_data$popularity)^2
  c(RMSE = rmse, R2 = r2)
})

# 9. Print test‐set performance
results_df <- do.call(rbind, results)
print(round(results_df, 3))

# 10. Visualize model performance
results_df$model <- rownames(results_df)
results_long <- melt(results_df, id.vars = "model")

ggplot(results_long, aes(x = model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Metric Value") + xlab("Model") +
  ggtitle("Model Performance on Test Set (RMSE & R²)") +
  theme_minimal()
