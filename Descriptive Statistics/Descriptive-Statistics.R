library(tidyverse)
library(matrixStats)
library(psych)

# Data used is nonnormal, missing values are not treated
datafull <- read_rds("Data/data-qualcl.RDS")
# Replace 7 for CCDN3 and CCIN3 with NA
datafull$CCDN3[datafull$CCDN3 == 7] <- NA
datafull$CCIN3[datafull$CCIN3 == 7] <- NA

# Psychological Variables
datapsych <-


#Descriptive statistics psychological
describe(datamat)
describeBy(datamat ~ BFE)
psychcode <- data.frame(Item = c("BFE1",
                                 "BFE2",
                                 "BFA1",
                                 "BFA2",
                                 "BFC1",
                                 "BFC2",
                                 "BFN1",
                                 "BFN2",
                                 "BFO1",
                                 "BFO2",
                                 "IC1",
                                 "IC2",
                                 "EC1",
                                 "EC2"),
                        Variable = c(rep("Extraversion", 2),
                                     rep("Agreeableness", 2),
                                     rep("Conscientiousness", 2),
                                     rep("Neuroticism", 2),
                                     rep("Openness", 2),
                                     rep("Internal.Control.Conviction", 2),
                                     rep("External.Control.Conviction", 2)))

psychdesc <- data.frame(Variable = character(),
                        Means = double (),
                        SD = double ())

for (i in 1:(nrow(psychcode)/2)){
  for (j in 1:2){
    psychdesc <- psychdesc %>%
      rbind(data.frame(
        Variable = psychcode[i,2],
        Means = datamatm[{{psychcode[i,1]}}],
        SD = datamatsd[{{psychcode[i,1]}}]))
  }}
psychdesc


#descriptive statistics Climate Change
#Take means of all variables
makeol <- function(x,y){
  rowMeans(subset(datawosk, select = c(paste0(x,y))), na.rm = TRUE)
}
# Make new df with means
varspsych <- data.frame(transmute(datawosk,
                                  Response.ID.1 = Response.ID.1,
                                  "Extraversion" = makepsych("BFE", 1:2),
                                  "Agreeableness" = makepsych("BFA", 1:2),
                                  "Conscientiousness" = makepsych("BFC", 1:2),
                                  "Neuroticism" = makepsych("BFN", 1:2),
                                  "Openness" = makepsych("BFO",1:2),
                                  "Internal.Control.Conviction" = makepsych("EC", 1:2),
                                  "External.Control.Conviction" = makepsych("EC", 1:2)))
varspsych
describe(olvarscc)
ccdesc <- data.frame(describe(olvarscc)) %>%
  transmute(Variable = rownames(data.frame(describe(olvarscc))),
            n = n,
            Means = mean,
            SD = sd) %>%
  filter(Variable != "Response.ID.1*")
order(ccdesc)
ccdesc

makepsych <- function(x,y){
  rowMeans(subset(datawosk, select = c(paste0(x,y))), na.rm = TRUE)
}
olvarscc <- data.frame(transmute(datawosk,
                                 Response.ID.1 = Response.ID.1,
                                 "Avoidant Maladaptism" = makeol("CCAM", 1:3),
                                 "Behavioral Intention" = makeol("CCBI", 1:3),
                                 "Knowledge" = makeol("CCKN", 1:7),
                                 "Perceived Moral Obligation" = makeol("CCPM", 1:3),
                                 "Perceived Self-Efficacy" = makeol("CCRB", c(1, 4, 7, 10)),
                                 "Perceived Response Efficacy" = makeol("CCRB", c(2, 5, 8, 11)),
                                 "Perceived Response Costs" = makeol("CCRB", c(3, 6, 9, 12)),
                                 "Subjective Norm Friends" = makeol("CCSN", 1:2),
                                 "Subjective Norm Family" = makeol("CCSN", 3:4),
                                 "Subjective Norm Colleagues" = makeol("CCSN", 5:6),
                                 "Perceived Susceptibility" = makeol("CCTB", 1:3),
                                 "Perceived Severity" = makeol("CCTB", 4:6),
                                 "Trusting Beliefs Benevolence" = makeol("CCTR", 1:3),
                                 "Trusting Beliefs Competence" = makeol("CCTR", 4:6),
                                 "Trusting Beliefs Integrity" = makeol("CCTR", 7:9),
                                 "Distrusting Beliefs Benevolence" = makeol("CCDI", 1:3),
                                 "Distrusting Beliefs Competence" = makeol("CCDI", 4:6),
                                 "Distrusting Beliefs Integrity" = makeol("CCDI", 7:9),
                                 "Behavior Change" = makeol("CCBC", 1:3),
                                 "Past Behavioral Intention" = makeol("CCPBI", 1:3),
                                 "Past Behavior Change" = makeol("CCPBC", 1:3),))
olvarscc

