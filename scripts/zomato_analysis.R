# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)

# 1. Load the dataset
getwd()
setwd("C:/Users/asiya/Desktop/DS RW")
df <- read.csv("Zomato-data-.csv", stringsAsFactors = FALSE)
head(df)
str(df)

# 2. Handle the 'rate' column (equivalent to handleRate function)
# Splits "4.1/5" into "4.1" and converts to numeric
df$rate <- str_remove(df$rate, "/5")
df$rate <- as.numeric(df$rate)
summary(df$rate)
# Check missing values
colSums(is.na(df))

# 3. Online Order Countplot
ggplot(df, aes(x = online_order)) +
  geom_bar(fill = "purple") +
  labs(x = "Online Order", y = "Count") +
  theme_minimal()

# 4. Ratings Comparison - Online vs Offline Orders
ggplot(df, aes(x = online_order, y = rate)) +
  geom_boxplot(fill = "cyan") +
  labs(x = "Online Order", y = "Rating") +
  theme_minimal()

# 5. Countplot for Restaurant Type
ggplot(df, aes(x = listed_in_type)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Restaurant Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. Line plot for Votes by Type
votes_by_type <- df %>%
  group_by(listed_in_type) %>%
  summarise(total_votes = sum(votes, na.rm = TRUE))

ggplot(votes_by_type, aes(x = listed_in_type, y = total_votes, group = 1)) +
  geom_line(color = "green") +
  geom_point() +
  labs(x = "Restaurant Type", y = "Total Votes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Restaurant with Max Votes
max_votes <- max(df$votes, na.rm = TRUE)
top_restaurant <- df[df$votes == max_votes, "name"]
top_restaurant

# 8. Ratings Distribution (Histogram)
ggplot(df, aes(x = rate)) +
  geom_histogram(binwidth = 0.5, fill = "orange", color = "black") +
  labs(title = "Ratings Distribution", x = "Rating", y = "Frequency") +
  theme_minimal()

# 9. Approx Cost for Two (Countplot/Bar)
ggplot(df, aes(x = as.factor(approx_cost_for_two_people))) +
  geom_bar(fill = "darkgreen") +
  labs(x = "Cost for Two (₹)", y = "Count") +
  theme_minimal()

# 10. Heatmap (Pivot Table equivalent)
# Find the relationship between order mode (online_order) and restaurant type.
heatmap_data <- df %>%
  group_by(listed_in_type, online_order) %>%
  summarise(count = n())

heatmap_cast <- dcast(
  heatmap_data,
  listed_in_type ~ online_order,
  value.var = "count",
  fill = 0
)

heatmap_melt <- melt(
  heatmap_cast,
  id.vars = "listed_in_type"
)

colnames(heatmap_melt) <- c("Type", "OnlineOrder", "Count")

ggplot(heatmap_melt, aes(x = OnlineOrder, y = Type, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Online Order vs Restaurant Type") +
  theme_minimal()

# Linear Regression Model
model <- lm(rate ~ votes + approx_cost_for_two_people, data = df)

# Generate predictions
df$predicted_rate <- predict(model, df)

# Generate predictions
df$predicted_rate <- predict(model, df)
ggplot(df, aes(x = rate, y = predicted_rate)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Actual vs Predicted Restaurant Ratings",
    x = "Actual Rating",
    y = "Predicted Rating"
  ) +
  theme_minimal()


