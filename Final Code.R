# Install required packages if not installed
# install.packages("mice")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("patchwork")

# Load required libraries
library(mice)
library(dplyr)
library(ggplot2)
library(reshape2)
library(patchwork)

# Load the dataset
df <- read.csv("C:/Users/moham/OneDrive/Desktop/Rassignment/INNHotelsGroup.csv")

# --- EDA ---

# Combining 2 columns to get a single column
df <- df %>%
  mutate(Total_Nights = no_of_week_nights + no_of_weekend_nights)

# Remove unwanted columns
df <- df %>% 
  select(-booking_id, no_of_week_nights, no_of_weekend_nights, -arrival_date, -rebooked)

# Repalcing 'Not Cancelled' to 'Confirmed' for better clarity
df$booking_status <- ifelse(df$booking_status == "Not Canceled", "Confirmed", df$booking_status)

# Rename columns with meaningless names
df <- df %>% 
  rename(
  Advance_Booking = lead_time,
  Market_Type = market_segment_type,
  Special_Request = no_of_special_requests,
  Room_Price_Avg = avg_price_per_room,
  Adults = no_of_adults,
  Parking_Required = required_car_parking_space,
  Status = booking_status
)

# --- Visualization ---

# Visualize distributions
p1 <- ggplot(df, aes(x = Market_Type)) + 
  geom_bar(fill = "blue", color = "black") + 
  theme_minimal() + 
  ggtitle("Distribution of Market Type") +
  xlab("Market Type") +
  ylab("Frequency")

p2 <- ggplot(df, aes(x=Advance_Booking)) + 
  geom_histogram(binwidth=10, fill="blue", color="black") + 
  theme_minimal() + 
  ggtitle("Distribution of Advance_Booking") +
  xlab("Advance_Booking (Days)") +
  ylab("Frequency")

p3 <- ggplot(df, aes(x=Special_Request)) + 
  geom_bar(fill="blue", color="black") + 
  theme_minimal() + 
  ggtitle("Distribution of Number of Special Requests") +
  xlab("Number Of Special Requests") +
  ylab("Count")

p4 <- ggplot(df, aes(x=Total_Nights)) + 
  geom_bar(fill="blue", color="black") + 
  theme_minimal() + 
  ggtitle("Distribution of Number Of Nights") +
  xlab("Number Of Nights") +
  ylab("Count")

# Patchwork layout
(p1 | p2) / (p3 | p4)

# --- Missing Value Imputation ---

# Missing values
missing_values <- sapply(df, function(x) sum(is.na(x)))
missing_columns <- names(missing_values[missing_values > 0])
missing_values_per_column <- missing_values[missing_columns]
print("-------------------------")
print("Missing Values Per Column:")
print(missing_values_per_column)

# Visualize missing values
missing_data <- colSums(is.na(df))
par(mar=c(9, 4, 4, 2))
barplot(missing_data, 
        col=c('navyblue', 'red'), 
        names.arg=names(df), 
        cex.axis=0.75, 
        cex.names=0.7, 
        las=2, 
        ylab="Missing Data",
        main="Pattern Of Missing Data")

# Impute missing values using mice
imputed_data <- mice(df, m = 10, method = 'pmm', seed = 500, printFlag = FALSE)

# Extract the complete datasets
completed_data <- complete(imputed_data, "long", include = TRUE)

# Fit regression models on each imputed dataset
lm_models <- with(imputed_data, lm(Room_Price_Avg ~ Advance_Booking + Special_Request + Total_Nights))

# Pool the results using Rubin's rules
pooled_results <- pool(lm_models)

# Print combined results
print("-------------------------")
print("Combined Results From Pooled Regression Models:")
summary(pooled_results)

# Extracting the complete data set 
# Here we have used the 1 imputation for further analysis
complete_data <- complete(imputed_data, 1)
print("-------------------------")
print("Complete Dataset For Further Analysis:")
print(head(complete_data))


# --- Hypothesis Testing ---

# Perform correlation tests between Room_Price_Avg and other variables
test1 <- cor.test(complete_data$Room_Price_Avg, complete_data$Advance_Booking)
test2 <- cor.test(complete_data$Room_Price_Avg, complete_data$Special_Request)
test3 <- cor.test(complete_data$Room_Price_Avg, complete_data$Total_Nights)

# Frame hypotheses
hypothesis_results <- data.frame(
  Test = c("Room_Price_Avg vs Advance_Booking", 
           "Room_Price_Avg vs Special_Request", 
           "Room_Price_Avg vs Total_Nights"),
  P_Value = c(test1$p.value, test2$p.value, test3$p.value),
  Decision = ifelse(c(test1$p.value, test2$p.value, test3$p.value) < 0.05, 
                    "Reject Null Hypothesis", "Fail to Reject Null Hypothesis")
)
print(hypothesis_results)

# --- Correlation Heatmap ---

# Calculate the correlation matrix
correlation_matrix <- cor(complete_data[, c("Room_Price_Avg", "Advance_Booking", "Special_Request", "Total_Nights")], use = "complete.obs")

# Melt the correlation matrix into a long format
melted_correlation_matrix <- melt(correlation_matrix)

# Create the heatmap with correlation values and no x-axis labels
heatmap_plot <- ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.text.y = element_text(size = 12),  # Adjust y-axis label size
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        plot.title = element_text(hjust = 0.5)) +  # Center the plot title
  coord_fixed() +
  labs(title = "Correlation Heatmap") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4)  # Add correlation values
print(heatmap_plot)

# --- Hierarchical Clustering ---

# Use the first imputed dataset for clustering
clustering_data <- complete_data %>%
  select(Advance_Booking, Special_Request, Room_Price_Avg, Adults, Total_Nights) %>%
  scale()  # Normalize numerical data

# Compute distance matrix
distance_matrix <- dist(clustering_data, method = "euclidean")

# Perform hierarchical clustering
hc <- hclust(distance_matrix, method = "ward.D")

# Select relevant variables for clustering
clustering_data <- complete_data %>%
  select(Advance_Booking, Special_Request, Room_Price_Avg, Adults, Total_Nights) %>%
  scale()  # Normalize numerical data

# Compute the within-cluster sum of squares (WCSS) for different values of k
wcss <- sapply(1:10, function(k){
  kmeans(clustering_data, centers = k, nstart = 25)$tot.withinss
})

# Create elbow plot
elbow_plot <- ggplot(data.frame(k = 1:10, wcss = wcss), aes(x = k, y = wcss)) +
  geom_point(size = 3, color = "blue") +
  geom_line(color = "red") +
  labs(title = "Elbow Method for Optimal Clusters", x = "Number of Clusters (k)", y = "Within-Cluster Sum of Squares") +
  theme_minimal()
print(elbow_plot)

# Cut tree into k clusters
k <- 3
clusters <- cutree(hc, k)

# Add cluster labels to original data set
complete_data$cluster <- as.factor(clusters)

# Analyze clusters
print(summary(complete_data$cluster))

# Visualize clusters
ggplot(complete_data, aes(x = Advance_Booking, y = Room_Price_Avg, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Clusters based on Hierarchical Clustering", x = "Advance_Booking (days)", y = "Average Price Per Room")

# Visualize Price Distribution Across Clusters
ggplot(complete_data, aes(x = cluster, y = Room_Price_Avg, fill = cluster)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               position = position_nudge(y = 10), color = "black") +  # Add median values as text
  labs(title = "Room Prices Across Customer Segments", x = "Cluster", y = "Average Price Per Room")