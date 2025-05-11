required_packages <- c(
  "dplyr","readr","stats","knitr","tidyr","caret","randomForest","MLmetrics","ggplot2","reshape2"
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

spotify_data <- read.csv("Cleaned_SpotifyTrack.csv")

# Script: Highest Overall Popularity Across Different Genres

artist_popularity <- spotify_data %>%
  group_by(artists) %>%
  summarize(
    mean_popularity = mean(popularity, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_popularity))

top_artist <- artist_popularity[1, ]

top_artists_plot <- artist_popularity %>%
  top_n(10, mean_popularity) %>%
  ggplot(aes(x = reorder(artists, mean_popularity), y = mean_popularity)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Artists by Average Popularity", x = "Artist", y = "Average Popularity")

print(top_artists_plot)

ggsave("Plots/EDA/top_10_artists_popularity.png", plot = top_artists_plot, width = 8, height = 6, dpi = 300)


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

top_genres_plot <- genre_popularity %>%
  top_n(10, mean_popularity) %>%
  ggplot(aes(x = reorder(track_genre, mean_popularity), y = mean_popularity)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Genres by Average Popularity", x = "Genre", y = "Average Popularity")

print(top_genres_plot)

ggsave("Plots/EDA/top_10_genres_popularity.png", plot = top_genres_plot, width = 8, height = 6, dpi = 300)


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
pop_summary <- spotify_data %>%
  group_by(explicit) %>%
  summarize(
    avg_popularity = mean(popularity, na.rm = TRUE),
    se = sd(popularity, na.rm = TRUE) / sqrt(n()),  # Standard Error
    count = n()
  )
  
ggplot(pop_summary, aes(x = as.factor(explicit), y = avg_popularity, fill = as.factor(explicit))) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
  geom_errorbar(aes(ymin = avg_popularity - se, ymax = avg_popularity + se), width = 0.2) +
  scale_fill_manual(values = c("grey60", "tomato")) +
  labs(
    title = "Average Popularity by Explicit Content",
    x = "Explicit Content (0 = No, 1 = Yes)",
    y = "Average Popularity",
    fill = "Explicit"
  ) +
  theme_minimal(base_size = 14)

ggsave("Plots/EDA/explicit_vs_popularity.png", width = 8, height = 6, dpi = 300)

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

melted_cor <- melt(cor_matrix)

cor_heatmap <- ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/EDA/valence_audio_correlation_heatmap.png", plot = cor_heatmap, width = 8, height = 6, dpi = 300)

