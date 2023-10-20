# Load necessary libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(factoextra)
data <- read.csv('spotify_data.csv')
set.seed(123)  # Set seed for reproducibility


# Select relevant features
features <- data[, c('danceability', 'energy', 'key')]

# Standardize the features
scaled_features <- scale(features)

# Perform KMeans clustering
kmeans_model <- kmeans(scaled_features, centers = 3, nstart = 20)
cluster_labels <- kmeans_model$cluster
data$cluster <- factor(cluster_labels)

# 3D Cluster Plot
cluster_plot <- plot_ly(data, x = ~danceability, y = ~energy, z = ~key, color = ~cluster, 
                        type = "scatter3d", mode = "markers", marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = 'Danceability'),
                      yaxis = list(title = 'Energy'),
                      zaxis = list(title = 'Key')))
print(cluster_plot)

# Elbow Diagram
fviz_nbclust(scaled_features, kmeans, method = "wss") +
  labs(title = "Elbow Diagram for KMeans Clustering")

# Cluster analysis
cluster_analysis <- data %>% group_by(cluster) %>% summarise(genres = toString(unique(genre)))

# Display cluster analysis
print(cluster_analysis)

# Top 10 Most Listened Genres
genre_counts <- table(data$genre)
top_10_genres <- names(sort(genre_counts, decreasing = TRUE)[1:10])

# Plot top 10 most listened genres
barplot(genre_counts[top_10_genres], col = rainbow(length(top_10_genres)), 
        main = 'Top 10 Most Listened Genres', xlab = 'Genre', ylab = 'Count', las = 2, cex.names = 0.7)

# Top 10 Least Listened Genres
genre_counts_tail <- table(data$genre)
bottom_10_genres <- names(sort(genre_counts_tail)[1:10])

# Plot top 10 least listened genres
barplot(genre_counts_tail[bottom_10_genres], col = rainbow(length(bottom_10_genres)), 
        main = 'Top 10 Least Listened Genres', xlab = 'Genre', ylab = 'Count', las = 2, cex.names = 0.7)

# Top Genres by Highest Average Instrumentalness
genre_avg_instrumentalness <- data %>% group_by(genre) %>% summarise(avg_instrumentalness = mean(instrumentalness))
top_genres_inst_high <- genre_avg_instrumentalness %>% arrange(desc(avg_instrumentalness)) %>% head(10)

# Plot top genres by highest average instrumentalness
barplot(top_genres_inst_high$avg_instrumentalness, col = rainbow(length(top_genres_inst_high)), 
        names.arg = top_genres_inst_high$genre, main = 'Top Genres by Highest Average Instrumentalness', 
        xlab = 'Genre', ylab = 'Average Instrumentalness')

# Top Genres by Lowest Average Instrumentalness
top_genres_inst_low <- genre_avg_instrumentalness %>% arrange(avg_instrumentalness) %>% head(10)

# Plot top genres by lowest average instrumentalness
barplot(top_genres_inst_low$avg_instrumentalness, col = rainbow(length(top_genres_inst_low)), 
        names.arg = top_genres_inst_low$genre, main = 'Top Genres by Lowest Average Instrumentalness', 
        xlab = 'Genre', ylab = 'Average Instrumentalness')

# Top Genres by Highest Average Acousticness
genre_avg_acousticness <- data %>% group_by(genre) %>% summarise(avg_acousticness = mean(acousticness))
top_genres_acoustic_high <- genre_avg_acousticness %>% arrange(desc(avg_acousticness)) %>% head(10)

# Plot top genres by highest average acousticness
barplot(top_genres_acoustic_high$avg_acousticness, col = rainbow(length(top_genres_acoustic_high)), 
        names.arg = top_genres_acoustic_high$genre, main = 'Top Genres by Highest Average Acousticness', 
        xlab = 'Genre', ylab = 'Average Acousticness')

# Top Genres by Lowest Average Acousticness
top_genres_acoustic_low <- genre_avg_acousticness %>% arrange(avg_acousticness) %>% head(10)

# Plot top genres by lowest average acousticness
barplot(top_genres_acoustic_low$avg_acousticness, col = rainbow(length(top_genres_acoustic_low)), 
        names.arg = top_genres_acoustic_low$genre, main = 'Top Genres by Lowest Average Acousticness', 
        xlab = 'Genre', ylab = 'Average Acousticness')

# Top Genres by Highest Average Speechiness
genre_avg_speechiness <- data %>% group_by(genre) %>% summarise(avg_speechiness = mean(speechiness))
top_genres_speechiness_high <- genre_avg_speechiness %>% arrange(desc(avg_speechiness)) %>% head(10)

# Plot top genres by highest average speechiness
barplot(top_genres_speechiness_high$avg_speechiness, col = rainbow(length(top_genres_speechiness_high)), 
        names.arg = top_genres_speechiness_high$genre, main = 'Top Genres by Highest Average Speechiness', 
        xlab = 'Genre', ylab = 'Average Speechiness')

# Top Genres by Lowest Average Speechiness
top_genres_speechiness_low <- genre_avg_speechiness %>% arrange(avg_speechiness) %>% head(10)

# Plot top genres by lowest average speechiness
barplot(top_genres_speechiness_low$avg_speechiness, col = rainbow(length(top_genres_speechiness_low)), 
        names.arg = top_genres_speechiness_low$genre, main = 'Top Genres by Lowest Average Speechiness', 
        xlab = 'Genre', ylab = 'Average Speechiness')

# 3D Cluster Plot
plot_ly(data, x = ~danceability, y = ~energy, z = ~key, color = ~cluster, type = "scatter3d", mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = 'Danceability'),
                      yaxis = list(title = 'Energy'),
                      zaxis = list(title = 'Key')))
# Perform KMeans clustering for different values of k
wcss <- numeric(10)  # Set an arbitrary maximum value of k (adjust as needed)
for (i in 1:10) {
  kmeans_model <- kmeans(scaled_features, centers = i, nstart = 20)
  wcss[i] <- sum(kmeans_model$withinss)
}

# Plot Elbow Diagram
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WCSS)",
     main = "Elbow Diagram for KMeans Clustering")


library(cluster)
library(factoextra)

# Assuming 'scaled_features' and 'cluster_labels' are available
silhouette_score <- silhouette(cluster_labels, dist(scaled_features))

# Plot Silhouette Plot with Color
silhouette_plot <- fviz_silhouette(silhouette_score, palette = "jco", ggtheme = theme_minimal())
print(silhouette_plot)


