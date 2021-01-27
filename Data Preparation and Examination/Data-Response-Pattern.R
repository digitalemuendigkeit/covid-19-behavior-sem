library(tidyverse)
library(car)
library(psych)
library(matrixStats)

# Examine data for skewness and kurtosis
# Survey 1
data1 <- read_rds("Data/S1-data.RDS")

# Psychological questions
data1psych <- data.frame(data1 %>% select(starts_with("BF")))
data1skpsych <- data.frame(Item = colnames(data1psych), Skewness = skew(data1psych), Kurtosis = kurtosi(data1psych))
