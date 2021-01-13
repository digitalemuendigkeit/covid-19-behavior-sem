library(tidyverse)

# Import Soft Launch Data
S1sl <- data.frame(read_csv(file = "Data/S1-20210113.csv"))[-(1:8),]
S1sl

# Find Age Distribution

# Age group 1: 18-39, Target percentage 33 %
(S1sl %>% count(SD1 < 40))[2,2]/nrow(S1sl)
#40 %

# Age group 2: 40-59, target percentage 38 %
(S1sl %>% count(SD1 > 39 & SD1 < 60))[2,2]/nrow(S1sl)
# 42 %

# Age group 3: 60+, target percentage 27 %
(S1sl %>% count(SD1 > 59))[2,2]/nrow(S1sl)
# 18 %

# Age distribution is alright

# Find Gender Distribution:

# Female: Target percentage max 51 %
(S1sl %>% count(SD2 == 1))[2,2]/nrow(S1sl)
# 60 %

# Male: Target percentage max 49 %
(S1sl %>% count(SD2 == 2))[2,2]/nrow(S1sl)
# 38 %

# Find condition/randomization, target percentage 50 %
(S1sl %>% count(is.na(CCSKN)))[2,2]/(nrow(S1sl) - 3)
# 55 % CO, 45 % CC

# Break-off rate
(S1sl %>% count(is.na(S2)))[2,2]/nrow(S1sl)
# 3 break-offs, 6 % break-off rate

# Where do the break-offs happen?
S1sl[is.na(S1sl$S2),]
# ALl break-offs are due to age being too low

# Mean response time
S1sl[!is.na(S1sl$S2),]$Duration..in.seconds. %>% as.numeric() %>% mean()
# 624 seconds, i.e., 10:24 min
