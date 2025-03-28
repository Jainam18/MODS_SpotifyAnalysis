required_packages <- c(
  "dplyr","readr","stats","knitr","tidyr","caret","randomForest","MLmetrics"
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

print("1st Approach considerting all the 113 categories")
data <- read.csv("Cleaned_SpotifyTrack.csv")

features <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo","track_genre")

data <- data[features]
# print(dim(data))
# Remove duplicate tracks, keeping only the first genre
data <- data %>% group_by(across(-track_genre)) %>% slice(1) %>% ungroup()
# # print(dim(data))
# View(data)

# Check the number of unique genres and their counts
num_genres <- length(unique(data$track_genre))
print(paste("Number of unique genres:", num_genres))
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
data <- data %>% group_by(across(-track_genre)) %>% slice(1) %>% ungroup()
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
num_genres <- length(unique(data$track_genre))
# print(paste("Number of unique genres:", num_genres))

data$music_category <- as.factor(data$music_category)

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

