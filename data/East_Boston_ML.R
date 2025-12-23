library(dplyr)
library(ggplot2)
library(car)        
library(lmtest)     
library(sandwich)   
library(cluster)
library(factoextra)
library(NbClust)

# Initial Model Fit

# loading dataset
data <- read_csv("C:/Users/ctjia/Downloads/East_Boston_Panel_Data.csv")

# remove missing values
data <- data%>%mutate(across(where(is.character), ~na_if(.x, "NA")))
data <- data%>%drop_na()


model_full <- lm(Median_Gross_Rent ~ Year + Median_Household_Income +
                   Labor_Force_Participation_Rate + Degree_Attainment_Rate + Public_Assistance_Rate,
                 data = data)

# Summary of the model
summary(model_full)

# Multicollinearity test
vif_values <- vif(model_full)
vif_values

# Residual Plot
plot(model_full$fitted.values, model_full$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "black")

# Histogram of Residuals
hist(model_full$residuals,
     breaks = 30,
     main = "Histogram of Residuals",
     xlab = "Residuals")

# QQ Plot of Residuals
qqnorm(model_full$residuals)
qqline(model_full$residuals, col = "red")

# Shapiro-Wilk Test for Normality
shapiro_result <- shapiro.test(model_full$residuals)
shapiro_result

# Breusch-Pagan Test for Homoscedasticity
bptest_result <- bptest(model_full)
bptest_result

# 1. Clustering Preparation

# Prepare data for clustering (exclude the removed variables)
clustering_vars <- data %>%
  select(Median_Household_Income, Labor_Force_Participation_Rate,
         Degree_Attainment_Rate, Public_Assistance_Rate)

# Scale the data
clustering_vars_scaled <- scale(clustering_vars)

# 2. Determine Optimal Number of Clusters

# Elbow Method
fviz_nbclust(clustering_vars_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters")

# Silhouette Method
fviz_nbclust(clustering_vars_scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal Clusters")

# NbClust Method
set.seed(123) # For reproducibility
nbclust_result <- NbClust(clustering_vars_scaled, distance = "euclidean",
                          min.nc = 2, max.nc = 10, method = "kmeans")

# Extract the suggested number of clusters from NbClust
suggested_clusters <- nbclust_result$Best.nc[1, ]

# Calculate the mode (most frequent number)
optimal_clusters <- as.numeric(names(which.max(table(suggested_clusters))))
optimal_clusters

# K-Means Clustering
set.seed(123)
kmeans_result <- kmeans(clustering_vars_scaled, centers = optimal_clusters, nstart = 25)

# Add cluster assignments to the data
data$kmeans_cluster <- as.factor(kmeans_result$cluster)

# Visualize k-means clusters
fviz_cluster(kmeans_result, data = clustering_vars_scaled) +
  labs(title = "K-Means Clustering Results")

# 3. Hierarchical Clustering

# Compute distance matrix
dist_matrix <- dist(clustering_vars_scaled, method = "euclidean")

# Perform hierarchical clustering using Ward's method
hc_result <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc_result, labels = FALSE, hang = -1, main = "Dendrogram of Hierarchical Clustering")

# Cut tree to obtain clusters (use the same number as k-means for comparison)
hc_clusters <- cutree(hc_result, k = optimal_clusters)

# Add hierarchical cluster assignments to the data
data$hc_cluster <- as.factor(hc_clusters)

# Visualize hierarchical clusters
fviz_dend(hc_result, k = optimal_clusters, rect = TRUE) +
  labs(title = "Hierarchical Clustering Dendrogram")

# 4. Run Regression Models Within Each Cluster (Updated)

# Function to run regression and print summary for clusters
run_cluster_regression <- function(cluster_variable, method_name) {
  cluster_levels <- sort(unique(na.omit(data[[cluster_variable]])))
  models <- list()
  
  for (cluster in cluster_levels) {
    cluster_data <- data%>%filter(.data[[cluster_variable]] == cluster)
    if (nrow(cluster_data) >= 5) { # Ensure sufficient data points
      model <- lm(Median_Gross_Rent ~ Median_Household_Income +
                  Labor_Force_Participation_Rate + Degree_Attainment_Rate +
                  Public_Assistance_Rate, 
                  data = cluster_data)
      models[[cluster]] <- model
      cat("\nRegression results for", method_name, "cluster", cluster, ":\n")
      print(summary(model))
    } else {
      cat("\nNot enough data to run regression for", method_name, "cluster", cluster, "\n")
    }
  }
  
  return(models)
}

# Regression for K-Means Clusters
kmeans_models <- run_cluster_regression("kmeans_cluster", "k-means")

# Regression for Hierarchical Clusters
hc_models <- run_cluster_regression("hc_cluster", "hierarchical")

# 5. Compare R-Squared Values Across Clusters

# Function to extract R-squared from models
get_r_squared <- function(model_list) {
  sapply(model_list, function(x) if (!is.null(x)) summary(x)$r.squared else NA)
}

# R-squared values for k-means clusters
kmeans_r_squared <- get_r_squared(kmeans_models)
kmeans_r_squared

# R-squared values for hierarchical clusters
hc_r_squared <- get_r_squared(hc_models)
hc_r_squared

# Combine R-squared values into a data frame for plotting
r_squared_df <- data.frame(
  Cluster = c(names(kmeans_r_squared), names(hc_r_squared)),
  R_Squared = c(kmeans_r_squared, hc_r_squared),
  Method = rep(c("K-Means", "Hierarchical"), each = length(kmeans_r_squared))
)

# Remove NA values
r_squared_df <- na.omit(r_squared_df)

# Plot R-squared values for comparison
ggplot(r_squared_df, aes(x = Cluster, y = R_Squared, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of R-Squared Values by Cluster",
       x = "Cluster",
       y = "R-Squared") +
  theme_minimal()

# 6. Summary Statistics for Each Cluster

# K-Means Clusters Summary
kmeans_summary <- data %>%
  group_by(kmeans_cluster) %>%
  summarise(across(c(Median_Gross_Rent, Median_Household_Income,
                     Labor_Force_Participation_Rate, Degree_Attainment_Rate,
                     Public_Assistance_Rate),
                   ~mean(.x, na.rm = TRUE)))
kmeans_summary

# Hierarchical Clusters Summary
hc_summary <- data %>%
  group_by(hc_cluster) %>%
  summarise(across(c(Median_Gross_Rent, Median_Household_Income,
                     Labor_Force_Participation_Rate, Degree_Attainment_Rate,
                     Public_Assistance_Rate),
                   ~mean(.x, na.rm = TRUE)))
hc_summary

