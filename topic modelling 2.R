library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)

file_path <- "C:/Users/mtakporokah/OneDrive - University of Plymouth/dissertation/open access guide/cleaned_openaccess.xlsx"
data <- read_excel(file_path)

# View the structure of the dataset
str(data)

# Specify the columns related to compliance issues
compliance_columns <- c(
  'OA Compliance Issue - Embargo exceeds maximum',
  'OA Compliance Issue - Item not live',
  'OA Compliance Issue - Missed deposit deadline',
  'OA Compliance Issue - Missing effective date',
  'OA Compliance Issue - Missing effective date - could not determine deposit deadline',
  'OA Compliance Issue - Missing publication date - could not determine embargo period',
  'OA Compliance Issue - No file of compliant version',
  'OA Compliance Issue - Overridden to be non-compliant'
)

# Create the 'compliant' column
# If any compliance issue column is TRUE, then compliant = 0, otherwise 1
data$compliant <- ifelse(rowSums(data[compliance_columns] == TRUE) > 0, 0, 1)

# Check the distribution of the new 'compliant' column
table(data$compliant)

# Summary statistics after adding the new column
summary(data)

# Check for missing values, excluding the expected columns
non_exception_columns <- setdiff(names(data), c("OA Policy Exception - Other", "OA Policy Exception - Tech1", compliance_columns))
missing_values <- colSums(is.na(data[, non_exception_columns]))

# Display missing values count for non-exception columns
print(missing_values)

# Distribution of categorical variable: Faculty (Primary Group)
ggplot(data, aes(x = `Primary Group`)) + 
  geom_bar(fill = "skyblue") + 
  theme_minimal() + 
  labs(title = "Distribution of Primary Group (Faculty)", x = "Primary Group", y = "Count")

# Distribution of numerical variable: Deposit Period (calculated from dates if needed)
ggplot(data, aes(x = `First Deposit Date`)) + 
  geom_histogram(binwidth = 30, fill = "lightgreen", color = "black") + 
  theme_minimal() + 
  labs(title = "Distribution of First Deposit Date", x = "First Deposit Date", y = "Frequency")

# Distribution of numerical variable: Number of Files
ggplot(data, aes(x = `Files in repository`)) + 
  geom_histogram(binwidth = 1, fill = "orange", color = "black") + 
  theme_minimal() + 
  labs(title = "Distribution of Number of Files", x = "Files in repository", y = "Frequency")

# Distribution of the new 'compliant' column
ggplot(data, aes(x = as.factor(compliant))) + 
  geom_bar(fill = "purple") + 
  theme_minimal() + 
  labs(title = "Distribution of Compliance Status", x = "Compliance (1 = Compliant, 0 = Non-Compliant)", y = "Count")

# Preprocess the data
# Handling categorical variables (Primary Group)
data$`Primary Group` <- as.factor(data$`Primary Group`)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$compliant, p = .8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- data[trainIndex,]
testData  <- data[-trainIndex,]

# Output the size of training and testing sets
cat("Training set size:", nrow(trainData), "\n")
cat("Testing set size:", nrow(testData), "\n")


####################################
library(caret)
library(randomForest)
library(xgboost)
library(dplyr)

# Prepare the data for modeling
model_data <- data %>%
  select(compliant, `Primary Group`, `First Deposit Date`, `Files in repository`, title) %>%
  mutate(
    `Primary Group` = as.factor(`Primary Group`),
    `First Deposit Date` = as.numeric(as.Date(`First Deposit Date`, format="%Y-%m-%d"))
  )

# Handle any missing values by removing rows with NA
model_data <- na.omit(model_data)

# Clean column names to remove spaces and special characters
colnames(model_data) <- make.names(colnames(model_data))
#################################
# Load necessary libraries
library(tm)
library(cluster)
library(ggplot2)

# Prepare the title data
titles <- model_data$title

# Create a text corpus using VCorpus
corpus <- VCorpus(VectorSource(titles))

# Apply transformations on the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Create a Document-Term Matrix with TF-IDF weighting
dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))

tfidf_matrix<- as.matrix(dtm)

# Perform K-means clustering
set.seed(123)
# Experiment with different values of k (number of clusters)
k_values <- 2:6
wss <- sapply(k_values, function(k) {
  kmeans(tfidf_matrix, centers = k, nstart = 20)$tot.withinss
})

# Plot the Elbow Method to determine the optimal number of clusters
plot(k_values, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters (k)", ylab = "Total within-cluster sum of squares")
# Load necessary libraries
library(Rtsne)
library(umap)


# Remove duplicate rows from the TF-IDF matrix
tfidf_matrix_unique <- tfidf_matrix[!duplicated(tfidf_matrix), ]

# Re-run K-means clustering on the unique data
set.seed(123)
# Increase the number of clusters
set.seed(123)
k <- 5  # Try with 5 clusters
kmeans_result <- kmeans(tfidf_matrix_unique, centers = k, nstart = 20)

# Re-run t-SNE with the new cluster assignments
tsne_result <- Rtsne(tfidf_matrix_unique, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)

# Create a data frame for visualization
tsne_data <- data.frame(tsne_result$Y, cluster = kmeans_result$cluster)

# Visualize the t-SNE results
ggplot(tsne_data, aes(x = X1, y = X2, color = as.factor(cluster))) +
  geom_point() +
  labs(title = paste("t-SNE Clustering of Titles with", k, "Clusters"), x = "Dimension 1", y = "Dimension 2")
###############################################
# Load necessary libraries for word embeddings
library(text2vec)
library(cluster)

# Example of using pre-trained word embeddings (e.g., GloVe)
# Assuming you have a GloVe model loaded as 'glove_model'
# Transform titles to word embeddings (taking the mean vector of words in each title)

# Convert titles to a list of words
titles_words <- strsplit(titles, " ")

# Compute the mean embedding for each title
title_embeddings <- sapply(titles_words, function(words) {
  word_vectors <- glove_model[words, , drop = FALSE]
  colMeans(word_vectors, na.rm = TRUE)
})

# Transpose to match the format for clustering
title_embeddings <- t(title_embeddings)

# Run K-means clustering on the title embeddings
set.seed(123)
kmeans_embeddings <- kmeans(title_embeddings, centers = 5, nstart = 20)

# Visualize the clustering using t-SNE
tsne_embeddings <- Rtsne(title_embeddings, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)
tsne_data <- data.frame(tsne_embeddings$Y, cluster = kmeans_embeddings$cluster)

# Plot the results
ggplot(tsne_data, aes(x = X1, y = X2, color = as.factor(cluster))) +
  geom_point() +
  labs(title = "t-SNE Clustering of Titles with Word Embeddings", x = "Dimension 1", y = "Dimension 2")

