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

spotify_data <- read.csv("/Users/jashbhatia/Downloads/MODS_SpotifyAnalysis/dataset.csv")

# Script: Highest Overall Popularity Across Different Genres

artist_popularity <- spotify_data %>%
  group_by(artists) %>%
  summarize(
    mean_popularity = mean(popularity, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_popularity))

top_artist <- artist_popularity[1, ]

cat("=====================================\n")
cat("HIGHEST OVERALL POPULARITY ACROSS GENRES\n")
cat("=====================================\n")
cat("Artist: ", top_artist$artists, "\n")
cat("Average Popularity Score: ", round(top_artist$mean_popularity, 2), "\n")
cat("=====================================\n")

# Script: Most Popular Genre Universally

genre_popularity <- spotify_data %>%
  group_by(track_genre) %>%
  summarize(
    mean_popularity = mean(popularity, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_popularity))

top_genre <- genre_popularity[1, ]

cat("=====================================\n")
cat("MOST POPULAR GENRE UNIVERSALLY\n")
cat("=====================================\n")
cat("Genre: ", top_genre$track_genre, "\n")
cat("Average Popularity Score: ", round(top_genre$mean_popularity, 2), "\n")
cat("=====================================\n")


# Script: Does Explicit Content Affect Popularity?

pop_by_explicit <- spotify_data %>%
  group_by(explicit) %>%
  summarize(
    avg_popularity = mean(popularity, na.rm = TRUE),
    track_count    = n()
  )

# Print out summary table
cat("=====================================================\n")
cat("AVERAGE POPULARITY BY EXPLICIT CONTENT\n")
cat("=====================================================\n")
print(pop_by_explicit)
cat("=====================================================\n\n")

################################################################
# PART B: Statistical Test (Two-Sample T-Test)
################################################################

# A two-sample t-test (assuming popularity is roughly normally 
# distributed or sample size is large enough for CLT).
# Null Hypothesis (H0): Mean popularity of explicit = Mean popularity of non-explicit
# Alternative (H1): Mean popularity of explicit != Mean popularity of non-explicit

t_test_result <- t.test(popularity ~ explicit, data = spotify_data)

cat("=====================================================\n")
cat("T-TEST RESULTS FOR EXPLICIT VS NON-EXPLICIT POPULARITY\n")
cat("=====================================================\n")
print(t_test_result)
cat("=====================================================\n\n")

################################################################
# PART C: Visualization
################################################################

# Simple boxplot comparing distributions
boxplot(popularity ~ explicit,
        data = spotify_data,
        main = "Song Popularity by Explicit Content",
        xlab = "Explicit",
        ylab = "Popularity")


# Script: Exploring Valence vs. Other Audio Features

features_of_interest <- c("valence", "danceability", "energy", "loudness",
                          "speechiness", "acousticness", "instrumentalness",
                          "liveness", "tempo")

# Filter the data to include only these columns and remove rows with NAs
audio_data <- spotify_data %>%
  select(all_of(features_of_interest)) %>%
  na.omit()

# Step 4: Calculate and visualize correlation matrix
cor_matrix <- cor(audio_data)
cat("========================================================\n")
cat("CORRELATION MATRIX (Valence & Other Audio Features)\n")
cat("========================================================\n")
print(cor_matrix)
cat("========================================================\n\n")

# Step 5: Pairwise scatter plot
# This will open a multi-plot window of scatter plots for each feature pair.
# Be aware that if you have many features, it can be quite large.
pairs(audio_data,
      main = "Pairwise Scatter Plots: Valence vs Other Audio Features")
