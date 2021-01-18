library(tidyverse)

# Import data
RD <- read_csv("Data/S1-Raw-Data-20210118.csv")

# Crop Data
# First 2 rows are descriptors
# Next 6 rows are tests
# Crop rows (1:8)
# From Descriptive Cols keep only Duration, RecordedDate, ResponseId, gid
# Crop cols c(1:5,7,10:18)

RDCrop <- RD[-(1:8),-c(1:5,7,10:18)]

# Only completed answers
# S2 is answered

RDComp <- RDCrop[!is.na(RDCrop$S2),]
nrow(RDComp)
# 955 answers

# Straight lining and diagonal lining is best found on response data using text

# Find speeders

# Define upper range
URtime <- quantile(as.numeric(RDComp$`Duration (in seconds)`), probs = c(.75), names = FALSE) +
  1.5 * IQR(as.numeric(RDComp$`Duration (in seconds)`))
# 1497.75 s

#Define lower range
LRtime <- quantile(as.numeric(RDComp$`Duration (in seconds)`), probs = c(.25), names = FALSE) -
  1.5 * IQR(as.numeric(RDComp$`Duration (in seconds)`))
# -104.25 s, invalid

# Find (upper)outliers for percentage
URolperc <- nrow(RDComp[as.numeric(RDComp$`Duration (in seconds)`) > URtime,])/nrow(RDComp)
# 8.3 percent

# Find trim mean
durtm <- mean(as.numeric(RDComp$`Duration (in seconds)`), trim = URolperc)
# trim mean is 712 s

# Speeders

# Twice as fast
RDComp[as.numeric(RDComp$`Duration (in seconds)`) < durtm/2,]
# 67 responses

# Three times as fast
RDComp[as.numeric(RDComp$`Duration (in seconds)`) < durtm/3,]
# 8 responses

RDprel <- RDComp[as.numeric(RDComp$`Duration (in seconds)`) > durtm/3,]
write_csv(RDprel, "Data/S1-Data-preliminary.csv")
