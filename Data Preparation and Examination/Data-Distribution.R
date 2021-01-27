library(tidyverse)
library(psych)

# Examine data for skewness and kurtosis
# Survey 1
data1 <- read_rds("Data/S1-data-qualcl.RDS")

# Psychological questions
data1psych <- as.matrix(data1 %>% select(starts_with("BF")))
data1skpsych <- data.frame(Item = colnames(data1psych), Skewness = skew(data1psych), Kurtosis = kurtosi(data1psych))
data1skcritpsych <- data1skpsych %>% filter(Skewness >= 1 | Skewness <= -1 | Kurtosis >= 1 | Kurtosis <= -1)
# BFC3 exhibits skewness and kurtosis beyond the threshold of -1...1, with kurtosis > 2
# exclude none

# Climate Crisis questions
data1cc <- as.matrix(data1 %>% select(starts_with("CC") & !c("CCS1", "CCS2", "CCS3", "CCS4")) %>% filter(!is.na(CCSKN)))
data1skcc <- data.frame(Item = colnames(data1cc), Skewness = skew(data1cc), Kurtosis = kurtosi(data1cc))
data1skcritcc <- data1skcc %>% filter(Skewness >= 1 | Skewness <= -1 | Kurtosis >= 1 | Kurtosis <= -1)
# CCKN4 and CCTB4 exhibit problematical skewness
# CCBI2, CCKN1 CCKN6 and CCIN3 exhibit problematical kurtosis
# CCPN3 exhibits both problematical skewness and kurtosis
# none with an absolute value > 2
# exclude none

# COVID-19 questions
data1co <- as.matrix(data1 %>% select(starts_with("CO") & !paste0("COS", 1:7)) %>% filter(!is.na(COSKN)))
data1skco <- data.frame(Item = colnames(data1co), Skewness = skew(data1co), Kurtosis = kurtosi(data1co))
data1skcritco <- data1skco %>% filter(Skewness >= 1 | Skewness <= -1 | Kurtosis >= 1 | Kurtosis <= -1)
# Most CO items exhibit skewness or kurtosis
data1skvcritco <- data1skco %>% filter(Skewness >= 5 | Skewness <= -5 | Kurtosis >= 5 | Kurtosis <= -5)
# exclude items with skewness | kurtosis > 5: COKN1, COKN2_1, COKN6, CORB3

data1normal <-data1 %>% select(!c("COKN1", "COKN2_1", "COKN6", "CORB3"))
write_csv2(data1normal, "Data/S1-data-nd.csv")
write_rds(data1normal, "Data/S1-data-nd.RDS")
