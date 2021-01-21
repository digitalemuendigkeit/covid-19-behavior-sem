library(tidyverse)
library(careless)
library(hcictools)

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
nrow(RDComp %>% filter(S2 == 2))
# 955 answers
# 907 want to be recontacted, 48 don't

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

# Find straight liners and diagonal liners
# Reverse reverse-coded data
# 6:1 Psychological c(BFE1, BFC1, BFA2, BFC2, BFO2, BFN3)
# 1:3 CC Knowledge c(CCKN1, CCKN3, CCKN5, CCKN6)
# 6:1 CC c(CCDI2, CCDI4, CCDI9, CCTB2, CCRB7, CCRB8, CCRB9, CCRB12)
# 1:3 CO Knowledge c(COKN4, COKN5, COKN6)
# 6:1 CO c(CODI2, CODI4, CODI9, COTB3, CORB7, CORB8, CORB9, CORB12)
revcode <-  function(x){recode(x,
                               "1" = 6,
                               "2" = 5,
                               "3" = 4,
                               "4" = 3,
                               "5" = 2,
                               "6" = 1,
                               .default = NaN)}
revkncode <-  function(x){recode(x,
                               "1" = 3,
                               "2" = 2,
                               "3" = 1,
                               .default = NaN)}
RDCompR <- RDComp %>%
  mutate_at(c("BFE1", "BFC1", "BFA2", "BFC2", "BFO2", "BFN3",
                                  "CCDI2", "CCDI4", "CCDI9", "CCTB2", "CCRB7", "CCRB8", "CCRB9", "CCRB12",
                                  "CODI2", "CODI4", "CODI9", "COTB3", "CORB7", "CORB8", "CORB9", "CORB12"),
                                list(revcode)) %>%
  mutate_at(c("CCKN1", "CCKN3", "CCKN5", "CCKN6", "COKN4", "COKN5", "COKN6"),
            list(revkncode))
RDCompR

#Overall analysis with careless

RDCompRC <- careless_indices(RDCompR, speeder_analysis = 356, id_column = "gid", likert_vector = c(25:142))
overallcc <- RDCompR[!is.na(RDCompR$CCSKN),-(97:142)]
coverallcc <- careless_indices(dat = overallcc, speeder_analysis = 356, likert_vector = c(25:52,59:96), id_column = "gid")
ccrem <- coverallcc %>% filter(coverallcc$longstr > 10 | coverallcc$avgstr > 5 | coverallcc$psychsyn <= 0 | coverallcc$mahadflag == TRUE)
ccoverallco <- RDCompR[!is.na(RDCompR$COSKN),-(52:96)]
coverallco <- careless_indices(dat = overallco, speeder_analysis = 356, likert_vector = c(25:52,60:97), id_column = "gid")
corem <- coverallco %>% filter(longstr > 10 | avgstr > 5 | psychsyn <= 0 | psychant >= 0 | mahadflag == TRUE)

# Make question blocks to examine
# Psych - some reversed, straightlining most critical
blockpsych <- RDCompR[,c(145, 1, 25:43)]
cbp <- blockpsych %>% cbind('longstr' = longstring(blockpsych), 'irv' = irv(blockpsych[,3:21]))
cpbrem <- cbp[cbp$gid == "DELV100341",]

# Behavioral Intention - none reversed, pole/diagonal most critical
blockbi <- RDCompR[,c(145, 1, 44:51)]
cbi <- blockbi %>% cbind('longstr' = longstring(blockbi), 'irv' = irv(blockbi[,-(1:2)]))
cbirem <- cbi %>% filter(longstr == 8 | irv >= 2.587746)

# CC Knowledge - some reversed
blockcckn <- RDCompR[!is.na(RDCompR$CCSKN),c(145, 1, 53:58)]
ccckn <- blockcckn %>% cbind('longstr' = longstring(blockcckn), 'irv' = irv(blockcckn[,-(1:2)]))
cccknrem <- ccckn %>% filter(longstr == 6)

# CC Distrust - some reversed, straight-lining most critical
blockccdi <- RDCompR[!is.na(RDCompR$CCSKN),c(145, 1, 59:67)]
cccdi <- blockccdi %>% cbind('longstr' = longstring(blockccdi), 'irv' = irv(blockccdi[,-(1:2)]))
cccdirem <- cccdi %>% filter(longstr == 9)

# CC Threat Beliefs - one reversed, straight-lining most critical
blockcctb <- RDCompR[!is.na(RDCompR$CCSKN),c(145, 1, 68:73)]
ccctb <- blockcctb %>% cbind('longstr' = longstring(blockcctb), 'irv' = irv(blockcctb[,-(1:2)]))
ccctbrem <- ccctb %>% filter(longstr == 6)

# CC Response Beliefs - four reversed, straight-lining most critical
blockccrb <- RDCompR[!is.na(RDCompR$CCSKN),c(145, 1, 74:85)]
cccrb <- blockccrb %>% cbind('longstr' = longstring(blockccrb), 'irv' = irv(blockccrb[,-(1:2)]))
cccrbrem <- cccrb %>% filter(longstr == 12)

# CC Personal Moral Norm - none reversed, diagonal-lining most critical
blockccpn <- RDCompR[!is.na(RDCompR$CCSKN),c(145, 1, 86:88)]
cccpn <- blockccpn %>% cbind('longstr' = longstring(blockccpn), 'irv' = irv(blockccpn[,-(1:2)]))
cccpnrem <- cccpn %>% filter(irv == 2.8867513)

## CC Subjective Norm - none reversed, diagonal-lining more critical
blockccsn <- RDCompR[!is.na(RDCompR$CCSKN), c(145, 1, 89:96)]
cccsn <- blockccsn %>% cbind('longstr' = longstring(blockccsn), 'irv' = irv(blockccsn[,-(1:2)]))
cccsnrem <- cccsn %>% filter(longstr == 8 | irv >= 2.7774603)

# CO Knowledge - some reversed
blockcokn <- RDCompR[!is.na(RDCompR$COSKN),c(145, 1, 98:104)]
ccokn <- blockcokn %>% cbind('longstr' = longstring(blockcokn), 'irv' = irv(blockcokn[,-(1:2)]))
ccoknrem <- ccokn %>% filter(longstr == 7)

# CO Distrust - some reversed, straight-lining most critical
blockcodi <- RDCompR[!is.na(RDCompR$COSKN),c(145, 1, 105:113)]
ccodi <- blockcodi %>% cbind('longstr' = longstring(blockcodi), 'irv' = irv(blockcodi[,-(1:2)]))
ccodirem <- ccodi %>% filter(longstr == 9 | irv == 2.635231)

# CO Threat Beliefs - one reversed, straight-lining most critical
blockcotb <- RDCompR[!is.na(RDCompR$COSKN),c(145, 1, 114:119)]
ccotb <- blockcotb %>% cbind('longstr' = longstring(blockcotb), 'irv' = irv(blockcotb[,-(1:2)]))
ccotbrem <- ccotb %>% filter(longstr == 6 | irv >= 2.581989)

# CO Response Beliefs - four reversed, straight-lining most critical
blockcorb <- RDCompR[!is.na(RDCompR$COSKN),c(145, 1, 120:131)]
ccorb <- blockcorb %>% cbind('longstr' = longstring(blockcorb), 'irv' = irv(blockcorb[,-(1:2)]))
ccorbrem <- ccorb %>% filter(longstr == 12 | irv >= 2.574643)

# CO Personal Moral Norm - none reversed, diagonal-lining most critical
blockcopn <- RDCompR[!is.na(RDCompR$COSKN),c(145, 1, 132:134)]
ccopn <- blockcopn %>% cbind('longstr' = longstring(blockcopn), 'irv' = irv(blockcopn[,-(1:2)]))
ccopnrem <- ccopn %>% filter(irv == 2.8867513)

# CO Subjective Norm - none reversed, diagonal-lining more critical
blockcosn <- RDCompR[!is.na(RDCompR$COSKN), c(145, 1, 135:142)]
ccosn <- blockcosn %>% cbind('longstr' = longstring(blockcosn), 'irv' = irv(blockcosn[,-(1:2)]))
ccosnrem <- ccosn %>% filter(irv == 0| irv >= 2.7774603)

# Make combination table
# rempattern <- data.frame("gid" = cpbrem$gid, "psych" = if_else(cpbrem$irv == 0, "straight", "polar"))
rempattern <- data.frame("gid" = cpbrem$gid, "psych.irv" = cpbrem$irv) %>%
  full_join(cbirem %>% transmute("gid" = gid, "BI.irv" = irv)) %>%
  full_join(cccknrem %>% transmute("gid" = gid, "CCKN.irv" = irv)) %>%
  full_join(cccdirem %>% transmute("gid" = gid, "CCDI.irv" = irv)) %>%
  full_join(ccctbrem %>% transmute("gid" = gid, "CCTB.irv" = irv)) %>%
  full_join(cccrbrem %>% transmute("gid" = gid, "CCRB.irv" = irv)) %>%
  full_join(cccpnrem %>% transmute("gid" = gid, "CCPN.irv" = irv)) %>%
  full_join(cccsnrem %>% transmute("gid" = gid, "CCSN.irv" = irv)) %>%
  full_join(ccoknrem %>% transmute("gid" = gid, "COKN.irv" = irv)) %>%
  full_join(ccodirem %>% transmute("gid" = gid, "CODI.irv" = irv)) %>%
  full_join(ccotbrem %>% transmute("gid" = gid, "COTB.irv" = irv)) %>%
  full_join(ccorbrem %>% transmute("gid" = gid, "CORB.irv" = irv)) %>%
  full_join(ccopnrem %>% transmute("gid" = gid, "COPN.irv" = irv)) %>%
  full_join(ccosnrem %>% transmute("gid" = gid, "COSN.irv" = irv))
rempattern$straight.blocks <- rowSums(rempattern[,2:15] == 0, na.rm = TRUE)
rempattern$polar.blocks <- rowSums(rempattern[,2:15] > 0, na.rm = TRUE)
rempattern$pattern.blocks <- rowSums(rempattern[,16:17], na.rm = TRUE)
rempattern
nrow(rempattern %>% filter(pattern.blocks > 2))

# 23 responses with more than two "pattern response" blocks

# Join data and assess
datacareless <- RDComp %>% left_join(rempattern[,c(1,16:18)])

# Excluded data
# 67 speeders, 48 don't want to be recontacted, 23 pattern responders (satisficer?) = max. 138 should be filtered, at least 817 should remain
dataexcluded <- datacareless %>% filter(as.numeric(`Duration (in seconds)`) < durtm/2 | (pattern.blocks > 2 & !is.na(pattern.blocks)) | S2 == 1)
view(dataexcluded[,c(1, 144:148)])
nrow(dataexcluded)
# 125 sets excluded

# Included data
dataincluded <- datacareless %>% filter(as.numeric(`Duration (in seconds)`) >= durtm/2 & (pattern.blocks <= 2 |is.na(pattern.blocks)) & S2 == 2)
nrow(dataincluded)
