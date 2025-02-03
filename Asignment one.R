getwd()
setwd("/Users/brendanebel/Documents/ABA")

netflix <- read.csv("netflix_titles.csv")
view(netflix)
summary(netflix)
#There are 11 columns and 8,807 rows in the netflix data set.
#The column names are show_id, type, title, director, cast, country, date_added, release_year, rating, duration, listed_in, description.
str(netflix)
#All of them are characters except for release_year
sum(is.na(netflix))
#No there is no missing data
#For this code you can use bar charts as they are commonly known to fit categorical data, you could also try and create a pie chart. 