library(tidyverse)

data1 <- read_rds("Data/S1-data-nd.RDS")

# Data should be only missing (=7) for the colleagues questions, i.e., CCIN3, CCNDN3, COIN3, CODN3
# Data should only be replaced if amount < 15%
nrow(data1 %>% filter(CCIN3 == 7))/nrow(data1 %>% filter(!is.na(CCIN3)))
# 20.4 % -> drop CCIN3
nrow(data1 %>% filter(CCDN3 == 7))/nrow(data1 %>% filter(!is.na(CCIN3)))
# 19.5 % -> drop CCDN3 (strange difference)
nrow(data1 %>% filter(COIN3 == 7))/nrow(data1 %>% filter(!is.na(COIN3)))
# 30.0 % -> drop CCIN3
nrow(data1 %>% filter(CODN3 == 7))/nrow(data1 %>% filter(!is.na(COIN3)))
# 30.0 % -> drop CCDN3

datanomissing <- data1 %>% select(-c("CCIN3", "CCDN3", "COIN3", "CODN3"))
write_csv2(datanomissing, "Data/S1-Data-nm.csv")
write_rds(datanomissing, "Data/S1-Data-nm.RDS")
