library(tidyverse)
library(readxl)
library(ggplot2)

getwd()
setwd("/Users/brendanebel/Documents/ABA")

wages <- read_excel("wages.xlsx")

view(wages)

#Question 1
ggplot(wages, aes(x = Age, y = Wage)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, color = 'red') +
  geom_smooth(method = 'lm', formula = y ~ poly(x , 2), se = FALSE, color = 'blue') +
  labs(title = "wage v. age", x = "age", y = "wage") +
  theme_minimal()

linear_model_wage <- lm(Wage ~ Age, data = wages)
summary(linear_model_wage)

# Linear Model
linear_model <- lm(Wage ~ Age + Educ, data = wages)
summary(linear_model)
# C
# Quadratic Model
quadratic_model <- lm(Wage ~ poly(Age, 2) + Educ, data = wages)
summary(quadratic_model)

summary(linear_model)
summary(quadratic_model)

# D
predict(linear_model, newdata = data.frame(Age = c(30, 50, 70), Educ = 16))
predict(quadratic_model, newdata = data.frame(Age = c(30, 50, 70), Educ = 16))
# E
coefs <- coef(quadratic_model)
optimal_age <- -coefs["poly(Age, 2)1"] / (2 * coefs["poly(Age, 2)2"])
optimal_age
# Question 2
rental <- read_excel("AnnArbor.xlsx")
# A
ggplot(rental, aes(x = Beds, y = Rent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = 'brown') +
  labs(title = "Rent vs. Bedrooms", x = "Number of Bedrooms", y = "Rent") +
  theme_minimal()

ggplot(rental, aes(x = Baths, y = Rent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = 'brown') +
  labs(title = "Rent vs. Bathrooms", x = "Number of Bathrooms", y = "Rent") +
  theme_minimal()

ggplot(rental, aes(x = Sqft, y = Rent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = 'brown') +
  labs(title = "Rent vs. Square Footage", x = "Square Footage", y = "Rent") +
  theme_minimal()

log_model <- lm(Rent ~ Beds + Baths + log(Sqft), data = rental)
summary(log_model)
# B
new_data <- data.frame(Beds = 3, Baths = 2, Sqft = 1600)
predicted_rent <- predict(log_model, new_data)
predicted_rent


