
# Script Name: Assignment 2
# 
#   Purpose: Homework
#   Author(s): Brendan Ebel
#   Date Created: 
#   
#   Notes:
#   
#
#
#-----------------------------------------------------------------------
# Working Directory 
getwd()
setwd("/Users/brendanebel/Documents/ABA")

#-----------------------------------------------------------------------
#Libraries and Packages
library(caTools)
library(tidyverse)
library(caret)
library(e1071)
library(lattice)
#-----------------------------------------------------------------------
#Load Data
admit <- read.csv("admit.csv")
view(admit)
#-----------------------------------------------------------------------
#Data Analysis

table(admit$admit)
prop.table(table(admit$admit)) * 100
# Admit/don't admit are not balanced 68.25 dont admit while 31.75 do admit.


hist(admit$gre, main="Distribution of GRE Scores", xlab="GRE Score", col="red", border="black")
#The graph is skewed to the left. Most of the scores are between 450-700 with the max scores at 800 and the lowest at 200.


set.seed(1234)
split <- sample.split(admit, SplitRatio = 0.8)

# Create training and testing sets
train_data <- subset(admit, split == TRUE)
test_data <- subset(admit, split == FALSE)


model <- glm(admit ~ ., data = train_data, family = binomial)
summary(model)

probabilities <- predict(model, test_data, type = "response")
predictions <- ifelse(probabilities > 0.5, 1, 0)


conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(test_data$admit))
print(conf_matrix)
#The model is good at detecting non-admitted students. It correctly predicts 62% of all cases. The model is High false negative. It has a moderate precision for identifying non-admitted students. It has a very low Kappa score at (0.0799). The P-Value is 0.03496 which means it is significantly imbalanced. 


accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Accuracy:", round(accuracy, 4)))
#The accuracy is 62%

precision <- conf_matrix$byClass["Precision"]
print(paste("Precision:", round(precision, 4)))
#Precision is 0.6709

recall <- conf_matrix$byClass["Recall"]
print(paste("Recall:", round(recall, 4)))
#The recall is 0.8154


importance <- abs(coef(model))[-1]  
importance <- sort(importance, decreasing = TRUE)
print(importance)
#GPA(0.917) is the most important predictor followed by rank(0.78). GRE falls of alot at 0.00182.

