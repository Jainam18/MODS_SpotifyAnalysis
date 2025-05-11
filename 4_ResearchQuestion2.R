# Installing Libraries 
required_packages <- c(
  "dplyr","readr","stats","knitr","tidyr","tidyverse","rpart","rpart.plot","caret","randomForest","MLmetrics","xgboost","ggplot2","corrplot","ModelMetrics","purrr"
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

# Genre Classification 

# Initial Approach using all the 113 genres and checking the accuracy and f1 score 
print("-------------------------------------------------------------------------------")

print("Mutliclass Classification - ")

print("-------------------------------------")

print("Using all the 113 genres:")

data <- read.csv("Cleaned_SpotifyTrack.csv")

features <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo","track_genre")

data <- data[features]
# View(data)
# print(dim(data))
# Remove duplicate tracks, keeping only the first genre
data <- data <- data %>% distinct(across(-track_genre), .keep_all = TRUE)
# # print(dim(data))
# View(data)

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


# Second Approach creating broader musical categories by merging the musical genres 

print("-------------------------------------")

print("Using 8 broader musical categories:")

print("---------------------------")
print("Random Forest Algorithm --")

# Random Forest Algorithm

data <- read.csv("Cleaned_SpotifyTrack.csv")

features <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo","track_genre")

data <- data[features]
# View(data)
# glimpse(data,width = 60)

data <- data <- data %>% distinct(across(-track_genre), .keep_all = TRUE)

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

data$music_category <- ifelse(data$track_genre %in% names(genre_to_category), 
                            genre_to_category[data$track_genre], 
                            "Other")

data = data%>%select(-track_genre)

features <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")

p <- data %>%
  select(c('music_category', features)) %>%
  pivot_longer(cols = features) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = music_category), alpha = 0.5) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Spotify Audio Feature Density - by Genre',
       x = '', y = 'density') +
  theme(axis.text.y = element_blank())

data <- data <- data %>% distinct(across(-music_category), .keep_all = TRUE)

c <- data %>%
  select(features) %>%
  scale() %>%
  cor() %>%
  corrplot::corrplot(method = 'color', 
                     order = 'hclust', 
                     type = 'upper', 
                     diag = FALSE, 
                     tl.col = 'black',
                     addCoef.col = "grey30",
                     number.cex = 0.6,
                     main = 'Audio Feature Correlation',
                     mar = c(2,2,2,2),
                     family = 'Avenir')

# ggsave(filename = "Plots/RQ2_Plots/spotify_audio_feature_correlation.png", plot = c, width = 12, height = 8, dpi = 300)

data_standardized <- data %>%
  mutate(across(all_of(features), scale))

View(data_standardized)

set.seed(1234)
training_songs <- sample(1:nrow(data_standardized), nrow(data_standardized)*.80, replace = FALSE)
train_set <- data_standardized[training_songs, c('music_category', features)] 
test_set <- data_standardized[-training_songs, c('music_category', features)] 

train_resp <- data_standardized[training_songs, 'music_category']
test_resp <- data_standardized[-training_songs, 'music_category']


set.seed(123)
model_rf <- randomForest(as_factor(music_category) ~ ., data = train_set, ntree = 100)

predict_rf <- predict(model_rf, test_set)

compare_rf <- data.frame(true_value = test_resp,
                         predicted_value = predict_rf,
                         model = 'random_forest',
                         stringsAsFactors = FALSE) 

model_accuracy_calc <- function(df, model_name) {
  df %>% 
    mutate(match = ifelse(true_value == predicted_value, TRUE, FALSE)) %>% 
    count(match) %>% 
    mutate(accuracy = n/sum(n),
           model = model_name)
}
accuracy_rf <- model_accuracy_calc(df = compare_rf, model_name = 'random_forest')

# print(accuracy_rf)

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

f1_scores_rf <- calculate_f1_scores(actual = compare_rf$true_value, predicted = compare_rf$predicted_value)

# Print the results
print(f1_scores_rf)

print("--------------------")
print("XG Boost Algorithm")
# XG Boost 
matrix_train_gb <- xgb.DMatrix(data = as.matrix(train_set[,-1]), label = as.integer(as.factor(train_set[,1])))
matrix_test_gb <- xgb.DMatrix(data = as.matrix(test_set[,-1]), label = as.integer(as.factor(test_set[,1])))

model_gb <- xgboost(data = matrix_train_gb, 
                    nrounds = 50,
                    verbose = FALSE,
                    params = list(objective = "multi:softmax",
                                  num_class = 8 + 1))

predict_gb <- predict(model_gb, matrix_test_gb)
predict_gb <- levels(as.factor(test_set$music_category))[predict_gb]

compare_gb <- data.frame(true_value = test_resp,
                         predicted_value = predict_gb,
                         model = 'xgboost',
                         stringsAsFactors = FALSE) 

accuracy_gb <- model_accuracy_calc(df = compare_gb, model_name = 'xgboost')


# print(accuracy_gb)  

f1_scores_gb <- calculate_f1_scores(actual = compare_gb$true_value, predicted = compare_gb$predicted_value)
print(f1_scores_gb)

print("--------------------------------------------------------------------------------")
# Multi-Label Classification
print("Multi-Label Classification: ")
data_2 = read.csv("Cleaned_SpotifyTrack.csv")

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
data_2$music_category <- ifelse(data_2$track_genre %in% names(genre_to_category), 
                            genre_to_category[data_2$track_genre], 
                            "Other")
multilabel_data <- data_2 %>%
  group_by(track_id) %>%
  summarize(
    genres = list(unique(music_category)),  
    across(all_of(features), mean)       
  )

# multilabel_data = multilabel_data%>%select(-c('track_genre'))

multilabel_data_flat <- multilabel_data %>%
  unnest_longer(genres)

# Step 2: create a column "present" with value 1
multilabel_data_flat <- multilabel_data_flat %>%
  mutate(present = 1)

# Step 3: pivot_wider to wide format
multilabel_matrix <- multilabel_data_flat %>%
  pivot_wider(
    names_from = genres,
    values_from = present,
    values_fill = 0
  )


multilabel_matrix = multilabel_matrix%>%select(-c('track_id'))

features <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")

genre_cols <- setdiff(names(multilabel_matrix), c(features))

# models <- map(genre_cols, function(genre) {
#   randomForest(
#     x = multilabel_matrix %>% select(all_of(features)),
#     y = as.factor(multilabel_matrix[[genre]]),
#     ntree = 100
#   )
# })

names(models) <- genre_cols


set.seed(123)

# Create an 80-20 train-test split
train_indices <- sample(1:nrow(multilabel_matrix), size = 0.8 * nrow(multilabel_matrix))

train_set <- multilabel_matrix[train_indices, ]
test_set <- multilabel_matrix[-train_indices, ]

models <- map(genre_cols, function(genre) {
  randomForest(
    x = train_set %>% select(all_of(features)),
    y = as.factor(train_set[[genre]]),
    ntree = 100
  )
})
names(models) <- genre_cols
# Predict probabilities or classes
predictions <- map(models, function(model) {
  predict(model, newdata = test_set %>% select(all_of(features)), type = "response")
})

predicted_matrix <- as.data.frame(predictions)
calculate_f1_scores <- function(actual, predicted) {
  labels <- names(actual)
  
  macro_f1 <- mean(sapply(labels, function(class) {
    F1_Score(y_true = actual[[class]], y_pred = predicted[[class]], positive = "1")
  }), na.rm = TRUE)
  
  support <- colSums(actual)
  weighted_f1 <- sum(sapply(labels, function(class) {
    F1_Score(y_true = actual[[class]], y_pred = predicted[[class]], positive = "1") * support[[class]] / sum(support)
  }), na.rm = TRUE)
  
  return(list(macro_f1 = macro_f1, weighted_f1 = weighted_f1))
}

names(test_set) <- make.names(names(test_set))

# Now redefine genre columns safely
genre_cols_cleaned <- names(predicted_matrix)

# Now you can select actuals and predictions cleanly
test_actual <- test_set %>% select(all_of(genre_cols_cleaned))
test_predicted <- predicted_matrix %>% select(all_of(genre_cols_cleaned))

# Now calculate F1 scores
f1_scores <- calculate_f1_scores(test_actual, test_predicted)

# Print F1 scores
print(f1_scores)

# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Create a dataframe from your model F1 results
f1_scores_df <- data.frame(
  Model = c("RF (113 Genres)", "RF (8 Categories)", "XGBoost (8 Categories)", "Multilabel RF"),
  Macro_F1 = c(f1_results_1$macro_f1, f1_scores_rf$macro_f1, f1_scores_gb$macro_f1, f1_scores$macro_f1),
  Weighted_F1 = c(f1_results_1$weighted_f1, f1_scores_rf$weighted_f1, f1_scores_gb$weighted_f1, f1_scores$weighted_f1)
)

# Convert to long format for ggplot
f1_scores_long <- pivot_longer(f1_scores_df, 
                               cols = c("Macro_F1", "Weighted_F1"), 
                               names_to = "Metric", 
                               values_to = "Score")

# Plot
ggplot(f1_scores_long, aes(x = Model, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "F1 Score Comparison Across Models",
       y = "F1 Score", x = "Model") +
  ylim(0, 1) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 15, hjust = 1))

# Optional: Save the plot
ggsave("f1_score_comparison_plot.png", width = 10, height = 6, dpi = 300)
