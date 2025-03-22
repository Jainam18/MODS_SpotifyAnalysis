required_packages <- c(
  "dplyr","readr","stats","knitr","tidyr"
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

data = read.csv('dataset.csv')

print("Dataset overview")

# print(str(data))
# print(summary(data))

# The length of the data is 114000
# 2. Exploratory Data Analysis ----

# 2.1 Descriptive Statistics ----

print("\n--- Descriptive Statistics ---\n")
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

# Except for Loudness all other column have positive values
# Checking whether it is okay to have negative values in Loudness column or not
# Loudness is The overall loudness of a track in decibels (dB)
# Decibels can be negative, and it simply means the sound level is lower than the reference level

# So keep decide to keep the values of loudness as it is 

# Null values analysis
null_values <- colSums(is.na(data))
null_values_table <- data.frame(
  Column = names(null_values),
  NullCount = null_values
)

print("\n--- Null Values ---\n")
print(kable(null_values_table, format = "simple"))
# Hence there are no null values present in the data 

detect_and_handle_outliers <- function(data, columns) {
  outlier_results <- list()
  for (col in columns) {
    numeric_column <- data[[col]]
    # Calculate IQR and bounds
    Q1 <- quantile(numeric_column, 0.25, na.rm = TRUE)
    Q3 <- quantile(numeric_column, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    # Create boxplot
    png(paste0("Plots/boxplot_", col, ".png"))
    boxplot(numeric_column, main = paste("Boxplot for", col), ylab = "Value")
    dev.off()
    # Identify and replace outliers
    new_column_name <- paste0(col, "_no_outliers")
    data[[new_column_name]] <- ifelse(
      numeric_column >= lower_bound & numeric_column <= upper_bound,
      numeric_column,
      NA
    )
    # Store outlier statistics
    outlier_results[[col]] <- list(
      total_values = length(numeric_column),
      outliers = sum(is.na(data[[new_column_name]])),
      outlier_percentage = (sum(is.na(data[[new_column_name]])) / length(numeric_column)) * 100
    )
  }
  return(list(data = data, outlier_summary = outlier_results))
}
outlier_output <- detect_and_handle_outliers(data, numeric_columns)
print("\n--- Outlier Analysis ---\n")
print(outlier_output$outlier_summary)

# We remove outliers from columns Duration_ms, and tempo

# duration_ms: Extreme track durations (e.g., unusually short or long tracks) might indicate errors or non-standard entries (e.g., intros, interludes, or extended versions). Outlier removal ensures consistency.

# tempo: Extremely high or low tempos may reflect errors or niche genres. Removing outliers can help focus on mainstream patterns.

remove_outliers <- function(data, column_name) {
  # Calculate IQR and bounds
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Filter rows within bounds
  data <- subset(data, data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound)
  
  return(data)
}

# Remove outliers from 'duration_ms'
new_data <- remove_outliers(new_data, "duration_ms")

# Remove outliers from 'tempo'
new_data <- remove_outliers(new_data, "tempo")


# print(dim(data))
# print(dim(new_data))


subset_col_data <- subset(new_data, select=-1)
new_unique_data = unique(subset_col_data)
print(dim(new_unique_data))

write.csv(new_unique_data, "Cleaned_SpotifyTrack.csv")