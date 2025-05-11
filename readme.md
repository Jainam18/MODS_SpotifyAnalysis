# 🎵 Spotify Music Analysis & Genre Classification

This project explores the musical landscape of Spotify tracks using data science techniques such as exploratory data analysis (EDA), supervised classification (multiclass & multilabel), and clustering.

The goal is to classify songs into meaningful genres based on their audio features, analyze explicit content influence on popularity, and discover hidden song clusters to aid music recommendation systems.

---

## 📂 Project Structure

├── 1_DataCleaning.R # Script for cleaning and preprocessing the raw dataset
├── 2_DataAnalysis.R # EDA and visualization (feature density, correlation)
├── 3_ResearchQuestion1.R # Analysis for: Does explicit content impact track popularity?
├── 4_ResearchQuestion2.R # Multiclass and multilabel genre classification using ML models
├── 5_ResearchQuestion3.R # Clustering songs using K-Means and visualizing clusters
├── Cleaned_SpotifyTrack.csv # Final cleaned dataset used for modeling
├── dataset.csv # Original/raw dataset before cleaning
├── Plots/ # Directory to save generated plots
├── readme.md # Project overview and structure (this file)
└── .Rapp.history # RStudio history file (can be ignored)


---

## 🔬 Key Research Questions

1. **Explicit Content Analysis**  
   _Does explicit content impact track popularity?_  
   → Performed hypothesis testing (t-test) and EDA.

2. **Genre Classification**  
   _Can we classify songs into genres based on audio features?_  
   → Implemented:
   - Multiclass classification (Random Forest, XGBoost)
   - Multilabel classification (one-vs-rest RF models)
   - Compared results using accuracy and F1 scores

3. **Clustering for Recommendation**  
   _Can we uncover natural groupings of songs using audio features?_  
   → Used K-Means clustering with PCA visualization and silhouette analysis.

---

## 🧪 Techniques Used

- Data Cleaning & Feature Engineering (`dplyr`, `tidyverse`)
- Visualization (`ggplot2`, `corrplot`, `factoextra`)
- Machine Learning (`randomForest`, `xgboost`, `caret`)
- Statistical Testing (`t.test`)
- Clustering (`kmeans`, `silhouette`, `PCA`)
