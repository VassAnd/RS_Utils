library(cluster)
library(fpc)

# Generate random synthetic data simulating a multi-band image with 5 bands

image_data <- data.frame(
  band1 = runif(1000),
  band2 = runif(1000),
  band3 = runif(1000),
  band4 = runif(1000),
  band5 = runif(1000)
)

# Define the range of the number of clusters to evaluate

k_range <- 2:15


# Compute the average silhouette width for each value of k

system.time({ 

silhouette_scores <- sapply(k_range, function(k) {
  # Perform k-means clustering
  kmeans_result <- kmeans(image_data, centers = k, nstart = 25)
  
  # Calculate the silhouette score
  silhouette_score <- cluster.stats(dist(image_data), kmeans_result$cluster)$avg.silwidth
  
  return(silhouette_score)
})

})

# Find the optimal number of clusters

optimal_k <- k_range[which.max(silhouette_scores)]
