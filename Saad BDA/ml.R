# Load necessary libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(factoextra)

# Read the data
data <- read.csv('spotify_data.csv')

# Set seed for reproducibility
set.seed(123)

# Adjust the sample size as needed
sample_size <- 1000
sampled_data <- data[sample(nrow(data), sample_size), ]

# Select relevant features
features <- sampled_data[, c('danceability', 'energy', 'key')]

# Standardize the features
scaled_features <- scale(features)

# Perform KMeans clustering
kmeans_model <- kmeans(scaled_features, centers = 3, nstart = 20)
cluster_labels <- kmeans_model$cluster
sampled_data$cluster <- factor(cluster_labels)

# 3D Cluster Plot
cluster_plot <- plot_ly(sampled_data, x = ~danceability, y = ~energy, z = ~key, color = ~cluster, 
                        type = "scatter3d", mode = "markers", marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = 'Danceability'),
                      yaxis = list(title = 'Energy'),
                      zaxis = list(title = 'Key')))
print(cluster_plot)

# Elbow Diagram
elbow_plot <- fviz_nbclust(scaled_features, kmeans, method = "wss") +
  labs(title = "Elbow Diagram for KMeans Clustering")
print(elbow_plot)




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
# Violin Plot for Danceability and Energy by Cluster
violin_plot <- ggplot(sampled_data, aes(x = as.factor(cluster), y = danceability, fill = as.factor(cluster))) +
  geom_violin() +
  labs(title = "Violin Plot for Danceability by Cluster",
       x = "Cluster",
       y = "Danceability") +
  theme_minimal()
print(violin_plot)


# Heatmap for Correlation Matrix
cor_matrix <- cor(sampled_data[, c('danceability', 'energy', 'key')])

heatmap_plot <- plot_ly(z = cor_matrix, 
                        type = "heatmap", 
                        colorscale = "Viridis",
                        x = colnames(cor_matrix), 
                        y = colnames(cor_matrix),
                        colorbar = list(title = 'Correlation', tickvals = c(-1, 0, 1), ticktext = c('-1', '0', '1')),
                        showscale = TRUE) %>%
  layout(title = "Correlation Matrix Heatmap")

print(heatmap_plot)



head(data)

# Genre analysis and trends
genre_counts <- table(sampled_data$year, sampled_data$genre)
matplot(row.names(genre_counts), genre_counts, type='l', col=1:ncol(genre_counts), lty=1, lwd=2, xlab='Year', ylab='Number of Songs', main='Genre Distribution Over the Years')
legend('topright', legend=colnames(genre_counts), fill=1:ncol(genre_counts), title='Genre')

# Key analysis
key_counts <- table(sampled_data$key)
barplot(key_counts, col='skyblue', main='Key Analysis', xlab='Key', ylab='Number of Songs', names.arg=LETTERS[1:12], ylim=c(0, max(key_counts) + 10))
grid()

# Mode analysis pie chart
mode_counts <- table(sampled_data$mode)
pie(mode_counts, labels=c('Major', 'Minor'), col=c('skyblue', 'lightcoral'), main='Mode Analysis (Pie Chart)', cex.main=0.8)





