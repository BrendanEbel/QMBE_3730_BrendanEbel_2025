
getwd()
setwd("/Users/brendanebel/Documents/ABA")

library(ggplot2)
library(caret)
library(randomForest)
library(lattice)
library(tidyverse)
library(dplyr)
library(pROC)

# Load dataset
data <- read.csv('loan_default_data_set.csv')

## Do EDA and report you findings.
View(data)
summary(data)
str(data)
#There are 20,000 observations and 21 variables. 
sum(is.na(data))
#There are 3,517 missing observations
colSums(is.na(data))
#Two columns have missing data rep_income and pct_card_over_50_uti.
#I am going to be replacing the missing values with the mean of the column
data <- data %>% mutate(pct_card_over_50_uti = replace_na(pct_card_over_50_uti, mean(pct_card_over_50_uti, na.rm = TRUE)))
data <- data %>% mutate(rep_income = replace_na(rep_income, mean(rep_income, na.rm = TRUE)))

duplicated_rows <- data[duplicated(data), ]
print(duplicated_rows)
#There are no duplicated rows

ggplot(data, aes(x = tot_balance, y = tot_amount_currently_past_due)) +
  geom_point(color = "blue", alpha = 0.6) +  
  labs(title = "Total Balance vs. Total Amount Currently Past Due",
       x = "Total Balance",
       y = "Total Amount Currently Past Due") +
  theme_minimal()
#Here I have plotted a scatter plot of total balance and total amount due. What I can see here is that under $50,000 balance and over $175,000 balance there is no money past due. Between $75,000 and $125,000 that is where you can see the most money past due. 

table(data$rep_education)
prop.table(table(data$rep_education)) * 100
#60.68% Have a college degree, 12% have a graduate, 26.57% have a high school degree and 0.71 have other.

table(data$Def_ind)
prop.table(table(data$Def_ind)) * 100
#No the they are not balanced ther is a 90% - 10% imbalance.
data$Def_ind <- (data$Def_ind - min(data$Def_ind)) / 
  (max(data$Def_ind) - min(data$Def_ind))

#To fix this I could over sample the lower one or undersample the higher one. I could also fo log transformantion, normalization, or standardization.

hist(data$rep_income, 
     main = "Histogram of rep_income", 
     xlab = "rep_income", 
     col = "blue", 
     breaks = 30)
#Looking at this graph rep_income is aproximatly normale there is no skew. 

ggplot(data, aes(x = rep_education, y = Def_ind, fill = Def_ind)) +
  geom_bar(stat = "identity") +
  labs(title = "Default Rate by Education Level",
       x = "Education Level",
       y = "Default Rate") +
  theme_minimal()
#This graph stands out with how much more of a default rate college has than any other education level. College has the most followed by high school, graduate, and other.

# Split dataset
set.seed(42)
trainIndex <- createDataPartition(data$Def_ind, p=0.8, list=FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

model <- glm(Def_ind ~ ., family = binomial, data = data)


knn_model <- train(Def_ind ~ ., data = train, method = "knn", tuneLength = 5)

# Make predictions on the test set
pred_knn <- predict(knn_model, test)

# Ensure both predicted and actual values are factors with the same levels
pred_knn <- factor(pred_knn, levels = c(0, 1))
test$Def_ind <- factor(test$Def_ind, levels = c(0, 1))

conf_matrix <- confusionMatrix(as.factor(pred_knn), as.factor(test$Def_ind))

print(conf_matrix)

accuracy <- conf_matrix$overall['Accuracy']
cat("Accuracy of KNN model:", accuracy, "\n")
#The accuracy of this model is 0.94.

precision <- conf_matrix$byClass['Precision']
cat("Precision:", precision, "\n")
#Precision is 0.94
recall <- conf_matrix$byClass['Recall']
cat("Recall:", recall, "\n")
#Recall is 1

knn_prob <- predict(knn_model, test, type = "prob")

prob_pos_class <- knn_prob[, 2]

roc_curve <- roc(test$Def_ind, prob_pos_class)

plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

importance <- summary(model)$coefficients
importance <- as.data.frame(importance)
importance <- importance[order(abs(importance$Estimate), decreasing = TRUE),]
importance

# Train and evaluate Decision Tree
# Convert the outcome variable to a factor with 2 levels
train$Def_ind <- factor(train$Def_ind, levels = c(0, 1))  

dt_model <- train(Def_ind ~ ., data=train, method='rpart') # Fit Decision Tree Model
pred_dt <- predict(dt_model, test)
conf_matrix_dt <- confusionMatrix(pred_dt, test$Def_ind)
print(conf_matrix_dt)
## Proceed with evaluation and interpretation of both models.
accuracy <- conf_matrix_dt$overall['Accuracy']
cat("Accuracy:", accuracy, "\n")
#0.91
precision <- conf_matrix_dt$byClass['Precision']
cat("Precision:", precision, "\n")
#0.91
recall <- conf_matrix_dt$byClass['Recall']
cat("Recall:", recall, "\n")
#0.99

#The knn did better it had better accuracy, precision, and recall that the desicion tree.
