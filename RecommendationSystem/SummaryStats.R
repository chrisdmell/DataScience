library(readr)
library(dplyr)
library(ggplot2)
library(data.table)

setwd("C:\\Users\\user\\Documents\\DataScience\\RecommendationSystem")

ratings = fread("ratings.csv")
tags    = fread("tags.csv")
movies  = fread("movies.csv")

nrow(ratings)
nrow(tags)
nrow(movies)

user.summary = ratings %>%
  group_by(userId) %>% 
  summarise(RatingCount = n(), MeanRating = mean(rating), RatingSpread = max(rating) - min(rating))
summary(select(user.summary, -userId))

ggplot(user.summary) + aes(x= RatingCount) +geom_histogram(binwidth = 50)


