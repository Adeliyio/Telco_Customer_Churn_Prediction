#Data Source https://www.kaggle.com/datasets/blastchar/telco-customer-churn
# Load necessary libraries
# Load necessary libraries
library(dplyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(pROC)
library(rpart)

# Read the CSV file into a variable named "pd"
pd <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')

# Check the structure of the dataset
str(pd)

# Check for missing values in each column
sapply(pd, function(x) sum(is.na(x)))

# Handle missing values
pd <- na.omit(pd)

# Recode categorical variables
cols_recode <- c(10:15)
for (col in cols_recode) {
  pd[[col]] <- as.factor(ifelse(pd[[col]] == "No internet service", "No", pd[[col]]))
}

# Recode "MultipleLines" variable
pd$MultipleLines <- as.factor(ifelse(pd$MultipleLines == "No phone service", "No", pd[[col]]))

# Group "tenure" into categories
group_tenure <- function(tenure) {
  if (tenure >= 0 & tenure <= 12) {
    return('0-12 Month')
  } else if (tenure > 12 & tenure <= 24) {
    return('12-24 Month')
  } else if (tenure > 24 & tenure <= 48) {
    return('24-48 Month')
  } else if (tenure > 48 & tenure <= 60) {
    return('48-60 Month')
  } else if (tenure > 60) {
    return('> 60 Month')
  }
}
pd$tenure_group <- as.factor(sapply(pd$tenure, group_tenure))

# Recode "SeniorCitizen" variable
pd$SeniorCitizen <- as.factor(ifelse(pd$SeniorCitizen == "0", "No", "Yes"))

# Remove unnecessary columns
pd$customerID <- NULL
pd$tenure <- NULL
pd$TotalCharges <- NULL
names(pd)

# Calculate correlation matrix for numerical variables
numeric_var <- sapply(pd, is.numeric)
corr_matrix <- cor(pd[, numeric_var, drop = FALSE])  # Include 'drop = FALSE' to preserve matrix-like structure
corrplot(corr_matrix, main = "\n\nCorrelation Plot for Numerical Variables", method = "number")


# Exploratory Data Analysis - Plots Categorical
par(mfrow = c(2, 2))
plot_1 <- ggplot(pd, aes(x = gender)) + 
  ggtitle("Gender") + 
  xlab("Gender") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_2 <- ggplot(pd, aes(x = SeniorCitizen)) + 
  ggtitle("Senior Citizen") + 
  xlab("Senior Citizen") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_3 <- ggplot(pd, aes(x = Partner)) + 
  ggtitle("Partner") + 
  xlab("Partner") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_4 <- ggplot(pd, aes(x = Dependents)) + 
  ggtitle("Dependents") + 
  xlab("Dependents") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

grid.arrange(plot_1, plot_2, plot_3, plot_4, ncol = 2)

par(mfrow = c(2, 2))
plot_5 <- ggplot(pd, aes(x = PhoneService)) + 
  ggtitle("Phone Service") + 
  xlab("Phone Service") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_6 <- ggplot(pd, aes(x = MultipleLines)) + 
  ggtitle("Multiple Lines") + 
  xlab("Multiple Lines") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_7 <- ggplot(pd, aes(x = InternetService)) + 
  ggtitle("Internet Service") + 
  xlab("Internet Service") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

# Create a colorful bar plot for "Online Security"
plot_8 <- ggplot(pd, aes(x = OnlineSecurity)) + 
  ggtitle("Online Security") + 
  xlab("Online Security") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()


grid.arrange(plot_5, plot_6, plot_7, plot_8, ncol = 2)

par(mfrow = c(2, 2))
plot_9 <- ggplot(pd, aes(x = OnlineBackup)) +
  ggtitle("Online Backup") +
  xlab("Online Backup") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_10 <- ggplot(pd, aes(x = DeviceProtection)) +
  ggtitle("Device Protection") +
  xlab("Device Protection") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5,color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_11 <- ggplot(pd, aes(x = TechSupport)) +
  ggtitle("Tech Support") +
  xlab("Tech Support") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_12 <- ggplot(pd, aes(x = StreamingTV)) +
  ggtitle("Streaming TV") +
  xlab("Streaming TV") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

grid.arrange(plot_9, plot_10, plot_11, plot_12, ncol = 2)

par(mfrow = c(2, 2))
plot_13 <- ggplot(pd, aes(x = StreamingMovies)) +
  ggtitle("Streaming Movies") +
  xlab("Streaming Movies") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_14 <- ggplot(pd, aes(x = Contract)) +
  ggtitle("Contract") +
  xlab("Contract") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_15 <- ggplot(pd, aes(x = PaperlessBilling)) +
  ggtitle("Paperless Billing") +
  xlab("Paperless Billing") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_16 <- ggplot(pd, aes(x = PaymentMethod)) +
  ggtitle("Payment Method") +
  xlab("Payment Method") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

plot_17 <- ggplot(pd, aes(x = tenure_group)) +
  ggtitle("Tenure Group") +
  xlab("Tenure Group") +
  geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5, color="white",fill = "#d45087") +
  ylab("Percentage") +
  coord_flip() +
  theme_minimal()

grid.arrange(plot_13, plot_14, plot_15, plot_16, plot_17, ncol = 2)



# Split the data into training and testing sets
CDP_data <- createDataPartition(pd$Churn, p = 0.7, list = FALSE)
set.seed(2017)
training <- pd[CDP_data, ]
testing <- pd[-CDP_data, ]

# Check the dimensions of the training and testing datasets
dim(training)
dim(testing)
# Convert "Churn" variable to factor with levels 0 and 1
training$Churn <- as.factor(ifelse(training$Churn == "No", 0, 1))
testing$Churn <- as.factor(ifelse(testing$Churn == "No", 0, 1))

# Fit the logistic regression model
LogModel <- glm(Churn ~ ., family = binomial(link = "logit"), data = training)

# Perform chi-square test on the logistic regression model
anova(LogModel, test = "Chisq")

# Convert Churn values in the testing set to numeric (0 and 1)
testing$Churn <- as.numeric(as.character(testing$Churn))

# Predict the churn using the logistic regression model
fitted.results <- predict(LogModel, newdata = testing, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)

# Calculate misclassification error
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy:', 1 - misClasificError))

# Create confusion matrix for logistic regression
print("Confusion Matrix for Logistic Regression")
table(testing$Churn, fitted.results)

# Fit decision tree model
tree_model <- rpart(Churn ~ ., data = training, method = "class")

# Predict using decision tree model
tree_pred <- predict(tree_model, newdata = testing, type = "class")

# Create confusion matrix for decision tree
tree_confusion_matrix <- table(testing$Churn, tree_pred)
print("Confusion Matrix for Decision Tree")
print(tree_confusion_matrix)

# Fit random forest model
rf_model <- randomForest(Churn ~ ., data = training)

# Tune random forest model
mtry_values <- seq(2, 10, by = 2)  # Specify range of mtry values
oob_errors <- rep(NA, length(mtry_values))  # Initialize vector to store OOB errors

for (i in seq_along(mtry_values)) {
  rf_model_temp <- randomForest(Churn ~ ., data = training, mtry = mtry_values[i])
  oob_errors[i] <- rf_model_temp$err.rate[nrow(rf_model_temp$err.rate), "OOB"]
}

# Find the optimal mtry value with the lowest OOB error rate
optimal_mtry <- mtry_values[which.min(oob_errors)]

# Fit random forest model with optimal mtry
rf_model <- randomForest(Churn ~ ., data = training, mtry = optimal_mtry)

# Predict using random forest model
rf_pred <- predict(rf_model, newdata = testing)

# Create confusion matrix for random forest
rf_confusion_matrix <- table(testing$Churn, rf_pred)
print("Confusion Matrix for Random Forest")
print(rf_confusion_matrix)

# Calculate random forest error rate
rf_error_rate <- 1 - sum(diag(rf_confusion_matrix)) / sum(rf_confusion_matrix)
print(paste("Random Forest Error Rate:", rf_error_rate))

# Calculate evaluation metrics for random forest
rf_accuracy <- sum(diag(rf_confusion_matrix)) / sum(rf_confusion_matrix)
rf_precision <- rf_confusion_matrix[2, 2] / sum(rf_pred == "Yes")
rf_recall <- rf_confusion_matrix[2, 2] / sum(testing$Churn == 1)
rf_f1 <- 2 * rf_precision * rf_recall / (rf_precision + rf_recall)

print(paste("Random Forest Accuracy:", rf_accuracy))
print(paste("Random Forest Precision:", rf_precision))
print(paste("Random Forest Recall:", rf_recall))
print(paste("Random Forest F1 Score:", rf_f1))

# Plot ROC curve and calculate AUC for random forest
roc_obj <- roc(testing$Churn, as.numeric(rf_pred))
print(paste("Random Forest AUC:", round(auc(roc_obj), 2)))
plot(roc_obj, main = "ROC Curve for Random Forest")

# Calculate precision, recall, and F1 score using manual calculations
tp <- sum(rf_pred == 1 & testing$Churn == 1)
fp <- sum(rf_pred == 1 & testing$Churn == 0)
fn <- sum(rf_pred == 0 & testing$Churn == 1)

precision_alt <- tp / (tp + fp)
recall_alt <- tp / (tp + fn)
f1_alt <- 2 * precision_alt * recall_alt / (precision_alt + recall_alt)

print(paste("Alternate Precision:", precision_alt))
print(paste("Alternate Recall:", recall_alt))
print(paste("Alternate F1 Score:", f1_alt))