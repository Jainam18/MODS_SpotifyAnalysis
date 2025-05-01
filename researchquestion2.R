required_packages <- c(
  "dplyr","readr","stats","knitr","tidyr","caret","randomForest","MLmetrics","xgboost"
)

install_and_load <- function(packages) {
    for (pkg in packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
      }
      library(pkg, character.only = TRUE)
    }
  }

install_and_load(required_packages)


# Research Question 2 
# Can we classify songs into genres based on their audio features? 

# Approach 1   - Using 113 genres directly 

print("1st Approach: ")
data <- read.csv("Cleaned_SpotifyTrack.csv")

features <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo","track_genre")

data <- data[features]
# View(data)
# print(dim(data))
# Remove duplicate tracks, keeping only the first genre
data <- data <- data %>% distinct(across(-track_genre), .keep_all = TRUE)
# # print(dim(data))
# View(data)

# Check the number of unique genres and their counts
num_genres <- length(unique(data$track_genre))
print(paste("Number of unique genres using Approach 1:", num_genres))
data$track_genre <- as.factor(data$track_genre)

set.seed(123)
trainIndex <- createDataPartition(data$track_genre, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train a Random Forest model
set.seed(123)
model1 <- randomForest(track_genre ~ ., data = trainData, ntree = 100)

# Predict on test data
predictions1 <- predict(model1, testData)

# Evaluate model accuracy
conf_matrix1 <- confusionMatrix(predictions1, testData$track_genre)
# print(conf_matrix)

calculate_f1_scores <- function(actual, predicted) {
  labels <- unique(actual)  # Get unique class labels
  macro_f1 <- mean(sapply(labels, function(class) {
    F1_Score(y_true = actual == class, y_pred = predicted == class, positive = TRUE)
  }), na.rm = TRUE)

  # Compute weighted F1-score
  support <- table(actual)  # Class frequencies
  weighted_f1 <- sum(sapply(labels, function(class) {
    F1_Score(y_true = actual == class, y_pred = predicted == class, positive = TRUE) * support[class] / sum(support)
  }), na.rm = TRUE)

  return(list(macro_f1 = macro_f1, weighted_f1 = weighted_f1))
}

f1_results_1 <- calculate_f1_scores(testData$track_genre, predictions1)

print(paste("Macro F1-score:", f1_results_1$macro_f1))
print(paste("Weighted F1-score:", f1_results_1$weighted_f1))

print("2nd Approach: ")


# 2. Second Approach merging genres into bigger musical categories and considering fewer main category 
data <- read.csv("Cleaned_SpotifyTrack.csv")

features <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo","track_genre")

data <- data[features]
# print(dim(data))
# Remove duplicate tracks, keeping only the first genre
data <- data <- data %>% distinct(across(-track_genre), .keep_all = TRUE)
# # print(dim(data))

genre_to_category <- c(
  # EDM
  "edm" = "Electronic Dance Music", 
  "house" = "Electronic Dance Music", 
  "electro" = "Electronic Dance Music", 
  "trance" = "Electronic Dance Music", 
  "techno" = "Electronic Dance Music", 
  "dubstep" = "Electronic Dance Music", 
  "drum-and-bass" = "Electronic Dance Music", 
  "deep-house" = "Electronic Dance Music", 
  "detroit-techno" = "Electronic Dance Music", 
  "minimal-techno" = "Electronic Dance Music", 
  "progressive-house" = "Electronic Dance Music", 
  "breakbeat" = "Electronic Dance Music",
  
  # Rock
  "alt-rock" = "Rock", 
  "rock" = "Rock", 
  "indie" = "Rock", 
  "indie-pop" = "Rock", 
  "punk" = "Rock", 
  "punk-rock" = "Rock", 
  "hard-rock" = "Rock", 
  "metal" = "Rock", 
  "heavy-metal" = "Rock", 
  "black-metal" = "Rock", 
  "death-metal" = "Rock", 
  "grunge" = "Rock",
  
  # Hip-Hop and R&B
  "hip-hop" = "Hip-Hop and R&B", 
  "r-n-b" = "Hip-Hop and R&B", 
  "trap" = "Hip-Hop and R&B",
  
  # Pop
  "pop" = "Pop", 
  "electro-pop" = "Pop", 
  "synth-pop" = "Pop", 
  "k-pop" = "Pop", 
  "pop-film" = "Pop", 
  "power-pop" = "Pop",
  
  # Latin & Reggae/Dancehall
  "latin" = "Latin & Reggae/Dancehall", 
  "reggaeton" = "Latin & Reggae/Dancehall", 
  "salsa" = "Latin & Reggae/Dancehall", 
  "samba" = "Latin & Reggae/Dancehall", 
  "reggae" = "Latin & Reggae/Dancehall", 
  "dancehall" = "Latin & Reggae/Dancehall",
  
  # Funk and Disco
  "funk" = "Funk and Disco", 
  "disco" = "Funk and Disco", 
  "groove" = "Funk and Disco",
  
  # Indian
  "indian" = "Indian"
)

# Apply mapping to dataframe
data$music_category <- ifelse(data$track_genre %in% names(genre_to_category), 
                            genre_to_category[data$track_genre], 
                            "Other")
# View(data)

# Check the number of unique genres and their counts
num_genres <- length(unique(data$music_category))
print(paste("Number of unique genres using Approach 2:", num_genres))

data$music_category <- as.factor(data$music_category)
data <- data %>% select(-track_genre)
set.seed(123)
trainIndex <- createDataPartition(data$music_category, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]

testData <- data[-trainIndex, ]

# Train a Random Forest model
set.seed(123)
model2 <- randomForest(music_category ~ ., data = trainData, ntree = 100)

# Predict on test data
predictions2 <- predict(model2, testData)

# Evaluate model accuracy
conf_matrix <- confusionMatrix(predictions2, testData$music_category)
# print(conf_matrix)


# # Compute Macro F1 Score
# macro_f1 <- F1_Score(y_pred = predictions, y_true = testData$track_genre, positive = NULL, average = "macro")
# print(paste("Macro F1 Score:", macro_f1))

calculate_f1_scores <- function(actual, predicted) {
  labels <- unique(actual)  # Get unique class labels
  macro_f1 <- mean(sapply(labels, function(class) {
    F1_Score(y_true = actual == class, y_pred = predicted == class, positive = TRUE)
  }), na.rm = TRUE)

  # Compute weighted F1-score
  support <- table(actual)  # Class frequencies
  weighted_f1 <- sum(sapply(labels, function(class) {
    F1_Score(y_true = actual == class, y_pred = predicted == class, positive = TRUE) * support[class] / sum(support)
  }), na.rm = TRUE)

  return(list(macro_f1 = macro_f1, weighted_f1 = weighted_f1))
}

f1_results2 <- calculate_f1_scores(testData$music_category, predictions2)

print(paste("Macro F1-score:", f1_results2$macro_f1))
print(paste("Weighted F1-score:", f1_results2$weighted_f1))


# Plot feature importance
importance <- importance(model2)
featureImportance <- data.frame(Feature = row.names(importance), Importance = importance[, 1])

# Sort by importance
featureImportance <- featureImportance %>% arrange(desc(Importance))
print(featureImportance)

# Plot
library(ggplot2)
ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance from Random Forest", x = "Features", y = "Importance")



# XG Boost  

data <- read.csv("Cleaned_SpotifyTrack.csv")

features <- c("danceability", "energy", "loudness", "speechiness", "acousticness",
              "instrumentalness", "liveness", "valence", "tempo", "track_genre")

data <- data[features]

# Remove duplicate tracks
data <- data %>% distinct(across(-track_genre), .keep_all = TRUE)

# Map genres to categories
genre_to_category <- c(
  "edm" = "Electronic Dance Music", "house" = "Electronic Dance Music", "electro" = "Electronic Dance Music", 
  "trance" = "Electronic Dance Music", "techno" = "Electronic Dance Music", "dubstep" = "Electronic Dance Music", 
  "drum-and-bass" = "Electronic Dance Music", "deep-house" = "Electronic Dance Music", 
  "detroit-techno" = "Electronic Dance Music", "minimal-techno" = "Electronic Dance Music", 
  "progressive-house" = "Electronic Dance Music", "breakbeat" = "Electronic Dance Music",
  "alt-rock" = "Rock", "rock" = "Rock", "indie" = "Rock", "indie-pop" = "Rock", "punk" = "Rock", 
  "punk-rock" = "Rock", "hard-rock" = "Rock", "metal" = "Rock", "heavy-metal" = "Rock", 
  "black-metal" = "Rock", "death-metal" = "Rock", "grunge" = "Rock",
  "hip-hop" = "Hip-Hop and R&B", "r-n-b" = "Hip-Hop and R&B", "trap" = "Hip-Hop and R&B",
  "pop" = "Pop", "electro-pop" = "Pop", "synth-pop" = "Pop", "k-pop" = "Pop", "pop-film" = "Pop", 
  "power-pop" = "Pop",
  "latin" = "Latin & Reggae/Dancehall", "reggaeton" = "Latin & Reggae/Dancehall", 
  "salsa" = "Latin & Reggae/Dancehall", "samba" = "Latin & Reggae/Dancehall", 
  "reggae" = "Latin & Reggae/Dancehall", "dancehall" = "Latin & Reggae/Dancehall",
  "funk" = "Funk and Disco", "disco" = "Funk and Disco", "groove" = "Funk and Disco",
  "indian" = "Indian"
)

data$music_category <- ifelse(data$track_genre %in% names(genre_to_category),
                              genre_to_category[data$track_genre], "Other")

data$music_category <- factor(data$music_category)
data <- data %>% select(-track_genre)

# -------------------------
# Train/Test Split
# -------------------------
set.seed(123)
trainIndex <- createDataPartition(data$music_category, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Save levels for decoding predictions
label_levels <- levels(trainData$music_category)

# Convert to numeric labels starting at 0
y_train <- as.numeric(trainData$music_category) - 1
y_test <- as.numeric(testData$music_category) - 1

# Convert features to matrix
x_train <- as.matrix(trainData %>% select(-music_category))
x_test <- as.matrix(testData %>% select(-music_category))

# -------------------------
# Train XGBoost Model
# -------------------------
class_weights <- 1 / table(y_train)
weights_vector <- class_weights[y_train + 1]

xgb_model <- xgboost(
  data = x_train,
  label = y_train,
  weight = weights_vector,
  objective = "multi:softprob",  # <-- changed here
  num_class = length(label_levels),
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  eval_metric = "mlogloss",
  verbose = 0
)

# -------------------------
# Prediction and Evaluation
# -------------------------
pred_probs <- predict(xgb_model, x_test)  

# XGBoost outputs a flat vector (not a nice 2D matrix), so you must reshape it:
# Each row = a sample
# Each column = probability for one class

pred_matrix <- matrix(pred_probs, 
                      ncol = length(label_levels), 
                      byrow = TRUE)

# Now pick class with highest probability
predicted_class_indices <- max.col(pred_matrix) - 1  # 0-indexed labels
predicted_labels <- factor(label_levels[predicted_class_indices + 1], 
                           levels = label_levels)

calculate_f1_scores <- function(actual, predicted) {
  labels <- unique(actual)
  macro_f1 <- mean(sapply(labels, function(class) {
    F1_Score(y_true = actual == class, y_pred = predicted == class, positive = TRUE)
  }), na.rm = TRUE)

  support <- table(actual)
  weighted_f1 <- sum(sapply(labels, function(class) {
    F1_Score(y_true = actual == class, y_pred = predicted == class, positive = TRUE) * support[class] / sum(support)
  }), na.rm = TRUE)

  return(list(macro_f1 = macro_f1, weighted_f1 = weighted_f1))
}

f1_results_xgb <- calculate_f1_scores(testData$music_category, predicted_labels)

print(paste("XGBoost Macro F1-score:", f1_results_xgb$macro_f1))
print(paste("XGBoost Weighted F1-score:", f1_results_xgb$weighted_f1))

# -------------------------
# Feature Importance Plot
# -------------------------
importance_matrix <- xgb.importance(model = xgb_model)
print(importance_matrix)

xgb.plot.importance(importance_matrix, top_n = 10, rel_to_first = TRUE,
                    xlab = "Relative Importance", main = "XGBoost Feature Importance")
