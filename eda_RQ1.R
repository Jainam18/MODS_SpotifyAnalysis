# Install and load required packages
required_packages <- c("dplyr", "ggplot2", "corrplot", "stats", "tidyr", "kableExtra")

install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

install_and_load(required_packages)
#data <- read.csv(file.choose())

# Load the cleaned Spotify dataset
data <- read.csv("/Users/meghna/Downloads/MODS_SpotifyAnalysis-main/Cleaned_SpotifyTrack.csv")

# Descriptive statistics for numerical columns
numeric_columns <- names(data)[sapply(data, is.numeric)]

# Compute descriptive statistics
descriptive_stats <- data.frame(
  Statistic = c("mean", "median", "min", "max", "sd", "variance")
)

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

# Print descriptive statistics
print(kable(descriptive_stats, format = "simple"))

# Visualizing distributions of key features including loudness, valence, and tempo
ggplot(data, aes(x=popularity)) +
  geom_histogram(bins=30, fill="blue", color="black", alpha=0.7) +
  labs(title="Distribution of Popularity", x="Popularity", y="Count")

ggplot(data, aes(x=energy)) +
  geom_histogram(bins=30, fill="green", color="black", alpha=0.7) +
  labs(title="Distribution of Energy", x="Energy", y="Count")

ggplot(data, aes(x=danceability)) +
  geom_histogram(bins=30, fill="purple", color="black", alpha=0.7) +
  labs(title="Distribution of Danceability", x="Danceability", y="Count")

ggplot(data, aes(x=loudness)) +
  geom_histogram(bins=30, fill="red", color="black", alpha=0.7) +
  labs(title="Distribution of Loudness", x="Loudness (dB)", y="Count")

ggplot(data, aes(x=valence)) +
  geom_histogram(bins=30, fill="orange", color="black", alpha=0.7) +
  labs(title="Distribution of Valence", x="Valence", y="Count")

ggplot(data, aes(x=tempo)) +
  geom_histogram(bins=30, fill="yellow", color="black", alpha=0.7) +
  labs(title="Distribution of Tempo", x="Tempo (BPM)", y="Count")

# Compute the correlation matrix for numerical features
corr_mat <- cor(data[, numeric_columns], use="complete.obs")

# Plot the correlation matrix using corrplot
corrplot(corr_mat, type="upper", tl.cex=0.8, tl.col="black", 
         title="Correlation Heatmap: Popularity vs Audio Features", mar=c(0,0,1,0))

# Calculate average feature values by genre
genre_features <- data %>%
  group_by(track_genre) %>%
  summarize(across(c(danceability, energy, loudness, speechiness, acousticness, 
                     instrumentalness, liveness, valence, tempo), mean, na.rm=TRUE))

# Examine selected genres for feature breakdown
genre_features %>%
  filter(track_genre %in% c("pop", "rock", "hip-hop", "metal", "classical", "jazz")) %>%
  select(track_genre, danceability, energy, loudness, acousticness, valence)

# Popularity distribution for explicit vs non-explicit songs
ggplot(data, aes(x=explicit, y=popularity, fill=explicit)) +
  geom_boxplot() +
  labs(title="Popularity by Explicit vs Non-Explicit Songs", x="Explicit Content", y="Song Popularity")

# Compute mean popularity for explicit vs non-explicit
explicit_popularity <- data %>%
  group_by(explicit) %>%
  summarize(mean_popularity = mean(popularity), median_popularity = median(popularity))
print(explicit_popularity)


# PCA for Feature Reduction
# Select numerical features for PCA
features <- data %>%
  select(danceability, energy, loudness, speechiness, acousticness, instrumentalness, 
         liveness, valence, tempo, duration_ms) %>%
  drop_na()

# Perform PCA
pca_model <- prcomp(features, center=TRUE, scale.=TRUE)

# Get the proportion of variance explained by each component
summary(pca_model)

# Plot the first two principal components
pca_scores <- data.frame(pca_model$x[, 1:2], genre = data$track_genre[complete.cases(data)])
ggplot(pca_scores, aes(x=PC1, y=PC2, color=genre)) +
  geom_point(alpha=0.5) +
  labs(title="PCA of Audio Features (Colored by Genre)", x="PC1", y="PC2")

# Step 1: Data Preprocessing

# Load necessary libraries for model building
library(caret)  # For data splitting
#library(tidyverse)  # For data manipulation

# Check for missing values and handle them (remove rows with NA for simplicity)
#data_clean <- na.omit(data)

# Split data into training and test sets (80% training, 20% test)
set.seed(42)  # Set seed for reproducibility
trainIndex <- createDataPartition(data$popularity, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Step 2: Train the Linear Regression Model

# Train the linear regression model
lm_model <- lm(popularity ~ energy + danceability + loudness + tempo + valence, data = train_data)

# Print model summary
summary(lm_model)

# Step 3: Evaluate the Model on Test Data

# Make predictions on the test set
predictions <- predict(lm_model, newdata = test_data)

# Evaluate the model performance
# RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - test_data$popularity)^2))

# R-squared (Coefficient of Determination)
rsq <- 1 - sum((predictions - test_data$popularity)^2) / sum((test_data$popularity - mean(test_data$popularity))^2)

# Print evaluation metrics
cat("RMSE: ", rmse, "\n")
cat("R-squared: ", rsq, "\n")

# Step 4: Model Interpretation
# Print coefficients to interpret the influence of each feature on popularity
coef(lm_model)

# Get the coefficients from the linear regression model
coefficients <- coef(lm_model)

# Convert coefficients to a data frame for easy plotting
coef_df <- data.frame(
  Feature = names(coefficients),
  Coefficient = coefficients
)

# Remove the intercept for the plot (usually we don't plot the intercept)
coef_df <- coef_df[coef_df$Feature != "(Intercept)", ]

# Plot the coefficients using ggplot2
ggplot(coef_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Linear Regression Coefficients", x = "Feature", y = "Coefficient Value") +
  theme_minimal()

