library(tidyverse)
library(psych)

#Import Data
# Data used is nonnormal, missing values are not treated
datafull <- read_rds("Data/data-qualcl.RDS")[,-c(1:5,143:145)]
# Replace 7 for CCDN3 and CCIN3 with NA
datafull$CCDN3[datafull$CCDN3 == 7] <- NA
datafull$CCIN3[datafull$CCIN3 == 7] <- NA

#Make scales from personality data
dataper <- data.frame(Variable = c(rep("Extraversion",3),
                                   rep("Agreeableness",3),
                                   rep("Conscientiousness",3),
                                   rep("Neuroticism",3),
                                   rep("Openness.to.Experience",3),
                                   rep("Internal.Control.Conviction",2),
                                   rep("External.Control.Conviction",2)),
                     Item = c(paste0("BFE", 1:3),
                              paste0("BFA", 1:3),
                              paste0("BFC", 1:3),
                              paste0("BFN", 1:3),
                               paste0("BFO", 1:3),
                               paste0("IC", 1:2),
                               paste0("EC", 1:2))
                                   )
dataperun <- data.frame(Variable = unique(dataper$Variable))
dataperunalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(dataperun)){
  x = dataperun[i,1]
  dataperunalpha <- append(dataperunalpha,
                          ((unlist(psych::alpha(datafull[unlist((filter(dataper, Variable == {{x}}))["Item"])], check.keys = FALSE)[1]))[3]))
  print(psych::alpha(datafull[unlist((filter(dataper, Variable == {{x}}))["Item"])], check.keys = FALSE))
}
dataperun <- dataperun %>% cbind(Cs.Alpha = dataperunalpha)
dataperalpha <- dataper %>% left_join(dataperun)
# Cronbach's alpha is insufficient for all constructs
# BFE: No alpha >0.7 attainable through item deletion, choose item with highest item-total correlation (r.drop): BFE2
# BFA: No alpha > 0.7 attainable through item deletion, choose item with highest item-total correlation: BFA1
# BFC: see above,choose BFC2
# BFN: Drop BFN3
# BFO: Choose BFO3
# IC: Choose IC2 (content)
# EC: Choose EC1 (content)
write_csv2(mmper1alpha, "Evaluation_Data/Statistical_Analysis/csalpha-per.csv")

psych::alpha(datafull %>% select(starts_with("BFE")), keys = TRUE)


#Define variables for correlation analysis
datavars1 <- datawosk[,1]
datavars1

#Climate Change
mmcc <- (readRDS("model-cc-1.rds"))$measurement_model
mmcc1a <- data.frame(Construct = (unlist(mmcc))[c(T, F, F)],
                     Indicator = (unlist(mmcc))[c(F, T, F)]) %>%
  filter(str_detect(Indicator, "^CC")) %>%
  cbind(Abbreviation = c(rep("CCAM",3),
                         rep("CCBI",3),
                         rep("CCKN",7),
                         rep("CCPM",3),
                         rep("CCPSE",4),
                         rep("CCPRE",4),
                         rep("CCPRC",4),
                         rep("CCSNFR",2),
                         rep("CCSNFA",2),
                         rep("CCSNCO",2),
                         rep("CCPSUS",3),
                         rep("CCPSEV",3),
                         rep("CCTRB",3),
                         rep("CCTRC",3),
                         rep("CCTRI",3),
                         rep("CCDIB",3),
                         rep("CCDIC",3),
                         rep("CCDII",3)))
mmcc1 <- mmcc1a %>% union(data.frame(Construct = c(rep("Past Behavioral Intention",3),
                                                   rep("Past Behavior",3),
                                                   rep("Behavior",3)),
                                     Indicator = c(paste0("CCPBI", 1:3),
                                                   paste0("CCPBC", 1:3),
                                                   paste0("CCBC", 1:3)),
                                     Abbreviation = c(rep("CCPBI", 3),
                                                      rep("CCPB", 3),
                                                      rep("CCB", 3))))
mmcc1
mmcc1un <- data.frame(Abbreviation = unique(mmcc1$Abbreviation))
mmcc1un
mmcc1unalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(mmcc1un)){
  x = mmcc1un[i,1]
  mmcc1unalpha <- append(mmcc1unalpha,
                         ((unlist(psych::alpha(datawosk[unlist((filter(mmcc1, Abbreviation == {{x}}))["Indicator"])], check.keys = TRUE)[1]))[3]))
  print(psych::alpha(datawosk[unlist((filter(mmcc1, Abbreviation == {{x}}))["Indicator"])], check.keys = TRUE))
}
mmcc1unal <- mmcc1un %>% cbind(Cs.Alpha = mmcc1unalpha)
mmcc1unal
mmcc1alpha <- mmcc1 %>% left_join(mmcc1unal)
mmcc1alpha

# CCAM is bad, drop CCAM1 to get above 0.6
# CCBI is at 0.69 but dropping any item would decrease alpha, retain as is
# CCKN is bad, but doesn't have to be good
# CCPM is satisfactory
# CCPSE: drop CCRB4
# CCPRE: drop CCRB5, CCRB10
# CCPRC: would not get better, retain all
# CCSNFR is bad, retain both in order to not get a single-item measure
# CCSNFA is okay, retain both
# CCSNCO is okay, retain both
# CCPSUS is satisfactory
# CCPSEV as well
# all CCTR, CCDI are good
#for CCPBI, CCPB, CCB alpha would not rise with indicator deletion

mmcc2 <- mmcc1 %>% filter(Indicator != "CCAM1" &
                            Indicator != "CCKN6" &
                            Indicator != "CCRB4" &
                            Indicator != "CCRB5" &
                            Indicator != "CCRB10")
mmcc2
mmcc2un <- data.frame(Abbreviation = unique(mmcc2$Abbreviation))
mmcc2un
mmcc2unalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(mmcc2un)){
  x = mmcc2un[i,1]
  mmcc2unalpha <- append(mmcc2unalpha, ifelse(nrow(filter(mmcc2, Abbreviation == {{x}}))==1,
                                              1,
                                              ((unlist(psych::alpha(datawosk[unlist((filter(mmcc2, Abbreviation == {{x}}))["Indicator"])], check.keys = TRUE)[1]))[2])))
  print(ifelse(nrow(filter(mmcc2, Abbreviation == {{x}}))==1,
               "no-alpha",
               psych::alpha(datawosk[unlist((filter(mmcc2, Abbreviation == {{x}}))["Indicator"])], check.keys = TRUE)))
}
mmcc2unal <- mmcc2un %>% cbind(Cs.Alpha = mmcc2unalpha)
mmcc2unal
mmcc2alpha <- mmcc2 %>% left_join(mmcc2unal)
mmcc2alpha
write_csv2(mmcc2alpha, "Evaluation_Data/Statistical_Analysis/csalpha-cc.csv")

#All important alphas are okay
#Now add data as specified to datavars
datavars2 <- datavars1
for (i in 1:nrow(mmcc2unal)){
  x = mmcc2unal[i,1]
  datavars2 <- datavars2 %>%
    cbind(Bob = rowMeans((datawosk %>% select(unlist((mmcc2alpha %>% filter(Abbreviation == {{x}}))[,2]))), na.rm = TRUE))
}
colnames(datavars2)
colnames(datavars2) <- c("Response.ID.1", unlist(mmcc2unal$Abbreviation))
datavars2

#COVID-19
mmco <- (readRDS("model-co-1.rds"))$measurement_model
mmco1a <- data.frame(Construct = (unlist(mmco))[c(T, F, F)],
                     Indicator = (unlist(mmco))[c(F, T, F)]) %>%
  filter(str_detect(Indicator, "^CO")) %>%
  cbind(Abbreviation = c(rep("COAM",3),
                         rep("COBI",3),
                         rep("COKN",4),
                         rep("COPM",3),
                         rep("COPSE",4),
                         rep("COPRE",4),
                         rep("COPRC",4),
                         rep("COSNFR",2),
                         rep("COSNFA",2),
                         rep("COPSUS",3),
                         rep("COPSEV",3),
                         rep("COTRB",3),
                         rep("COTRC",3),
                         rep("COTRI",3),
                         rep("CODIB",3),
                         rep("CODIC",3),
                         rep("CODII",3)))
mmco1 <- mmco1a %>% union(data.frame(Construct = c(rep("Past Behavioral Intention",3),
                                                   rep("Past Behavior",3),
                                                   rep("Behavior",3)),
                                     Indicator = c(paste0("COPBI", 1:3),
                                                   paste0("COPBC", 1:3),
                                                   paste0("COBC", 1:3)),
                                     Abbreviation = c(rep("COPBI", 3),
                                                      rep("COPB", 3),
                                                      rep("COB", 3))))
mmco1
mmco1un <- data.frame(Abbreviation = unique(mmco1$Abbreviation))
mmco1un
mmco1unalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(mmco1un)){
  x = mmco1un[i,1]
  mmco1unalpha <- append(mmco1unalpha,
                         ((unlist(psych::alpha(datawosk[unlist((filter(mmco1, Abbreviation == {{x}}))["Indicator"])], check.keys = TRUE)[1]))[3]))
  print(psych::alpha(datawosk[unlist((filter(mmco1, Abbreviation == {{x}}))["Indicator"])], check.keys = TRUE))
}
mmco1unal <- mmco1un %>% cbind(Cs.Alpha = mmco1unalpha)
mmco1unal
mmco1alpha <- mmco1 %>% left_join(mmco1unal)
mmco1alpha

#COAM is bad, will not be >0.7 even if one item is dropped
#drop COAM1
#COBI is below 0.7 but would not get better even if one item is dropped -> tolerate
#COKN doesn't have to meet cs alpha
# COPM is good
# COPSE is bad, but would not get better above 0.7 item was dropped
# drop CORB7
# COPRE is good
# COPRC is also good
# COSNFR is bad, keep still
# COSNFA is okay
# COPSUS is just right, could be better if COTB3 was dropped, but keep all
# COPSEV is bad, would not get better when dropping one item
# keep all
# all COTR, CODI are fine
#copbi, cobpmm cobi are all fine enough

mmco2 <- mmco1 %>% filter(Indicator != "COAM1" &
                            Indicator != "CORB4")
mmco2
mmco2un <- data.frame(Abbreviation = unique(mmco2$Abbreviation))
mmco2un
mmco2unalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(mmco2un)){
  x = mmco2un[i,1]
  mmco2unalpha <- append(mmco2unalpha, ifelse(nrow(filter(mmco2, Abbreviation == {{x}}))==1,
                                              1,
                                              ((unlist(psych::alpha(datawosk[unlist((filter(mmco2, Abbreviation == {{x}}))["Indicator"])], check.keys = TRUE)[1]))[2])))
  print(ifelse(nrow(filter(mmco2, Abbreviation == {{x}}))==1,
               "no-alpha",
               psych::alpha(datawosk[unlist((filter(mmco2, Abbreviation == {{x}}))["Indicator"])], check.keys = TRUE)))
}
mmco2unal <- mmco2un %>% cbind(Cs.Alpha = mmco2unalpha)
mmco2unal
mmco2alpha <- mmco2 %>% left_join(mmco2unal)
mmco2alpha
write_csv2(mmco2alpha, "Evaluation_Data/Statistical_Analysis/csalpha-co.csv")

#All important alphas are fine
# Make vars
datavars3 <- datavars2
for (i in 1:nrow(mmco2unal)){
  x = mmco2unal[i,1]
  datavars3 <- datavars3 %>%
    cbind(Bob = rowMeans((datawosk %>% select(unlist((mmco2alpha %>% filter(Abbreviation == {{x}}))[,2]))), na.rm = TRUE))
}
colnames(datavars3)
colnames(datavars3) <- c("Response.ID.1", unlist(mmcc2unal$Abbreviation), unlist(mmco2unal$Abbreviation))
datavars3

#Personality Questions
mmper1 <- data.frame(Construct = c(rep("Extraversion",2),
                                   rep("Agreeableness",2),
                                   rep("Conscientiousness",2),
                                   rep("Neuroticism",2),
                                   rep("Openness.to.Experience",2),
                                   rep("Internal.Control.Conviction",2),
                                   rep("External.Control.Conviction",2)),
                     Indicator = c("BFE1",
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
                                   "EC2"))
mmper1
mmper1un <- data.frame(Construct = unique(mmper1$Construct))
mmper1un
mmper1unalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(mmper1un)){
  x = mmper1un[i,1]
  mmper1unalpha <- append(mmper1unalpha,
                          ((unlist(psych::alpha(datawosk[unlist((filter(mmper1, Construct == {{x}}))["Indicator"])], check.keys = TRUE)[1]))[3]))
  print(psych::alpha(datawosk[unlist((filter(mmper1, Construct == {{x}}))["Indicator"])], check.keys = TRUE))
}
mmper1unal <- mmper1un %>% cbind(Cs.Alpha = mmper1unalpha)
mmper1unal
mmper1alpha <- mmper1 %>% left_join(mmper1unal)
mmper1alpha
write_csv2(mmper1alpha, "Evaluation_Data/Statistical_Analysis/csalpha-per.csv")

allalphas <- mmcc2alpha[,c(3,2,4)] %>%
  union(mmco2alpha[,c(3,2,4)]) %>%
  union(mmper1alpha %>% rename(Abbreviation = Construct)) %>%
  rename(Variable = Abbreviation)
allalphas
write_csv2(allalphas, "Evaluation_Data/Statistical_Analysis/Cronbachs_Alpha_Scales.csv")

#alpha is very low with agreeableness and conscientiousness, but the alternative is making a single-item measure
datavars4 <- datavars3
for (i in 1:nrow(mmper1unal)){
  x = mmper1unal[i,1]
  datavars4 <- datavars4 %>%
    cbind(Bob = rowMeans((datawosk %>% select(unlist((mmper1alpha %>% filter(Construct == {{x}}))[,2]))), na.rm = TRUE))
}
colnames(datavars4)
colnames(datavars4) <- c("Response.ID.1", unlist(mmcc2unal$Abbreviation), unlist(mmco2unal$Abbreviation), unlist(mmper1unal$Construct))
datavars4

##tack on rest of variables for analysis
datavarsdf <- data.frame(datavars4) %>%
  left_join(restdata) %>% mutate(Response.ID.1 = NULL)
datavars <- apply(as.matrix(datavarsdf), 2, as.numeric)
datavars
saveRDS(datavars, "correlation-analysis-data.rds")

#Shortcut to analysis starts here
datavars <- readRDS("correlation-analysis-data.rds")

## Descriptive Statistics
desstatcor1 <- rownames_to_column(data.frame(psych::describe(datavars[,1:48]))) %>% transmute(
  Variable = rowname,
  n = n,
  m = mean,
  SD = sd)
desstatcor1c <- unique(desstatcor1$Variable)
desstatcor2c <- c()
for (i in 1:length(desstatcor1c)){
  x = desstatcor1c[i]
  y = (allalphas %>% filter(Variable == {{x}}))[,3]
  z = paste(y, collapse = ", ")
  desstatcor2c <- append(desstatcor2c, z)}
desstatcor2c

desstatcor2 <- desstatcor1 %>% cbind(Indicators = desstatcor2c) %>% left_join(allalphas[,c(1,3)])
desstatcor3 <- distinct(desstatcor2[,c(1,5,2,6,3,4)])
desstatcor3
write_csv2(desstatcor3, "Evaluation_Data/Statistical_Analysis/descriptive-statistics.csv")


#BI and B
cor.test(datavars[,"CCBI"], datavars[,"CCB"])
cor.test(datavars[,"CCPBI"], datavars[,"CCPB"])
cor.test(datavars[,"CCPB"], datavars[,"CCB"])
cor.test(datavars[,"CCPBI"], datavars[,"CCBI"])
cor.test(datavars[,"COBI"], datavars[,"COB"])
cor.test(datavars[,"COPBI"], datavars[,"COPB"])
cor.test(datavars[,"COPB"], datavars[,"COB"])
cor.test(datavars[,"COPBI"], datavars[,"COBI"])
cor.test(datavars[,"CCB"], datavars[,"COB"])
cor(datavars[,c("CCBI","CCB","CCPBI","CCPB","COBI","COB","COPBI","COPB")], use ="pairwise.complete.obs")

#BI, B and other constructs
cor(datavars[,c("CCBI","CCB","CCPBI","CCPB")], datavars[,unlist(mmcc2un)], use ="pairwise.complete.obs")
cor(datavars[,c("COBI","COB","COPBI","COPB")], datavars[,unlist(mmco2un)], use ="pairwise.complete.obs")

#model constructs
intercorcc <-  data.frame(cor(datavars[,unlist(mmcc2un)], use ="pairwise.complete.obs"))
intercorcc
cor.test(datavars[,"CCPM"], datavars[,"CCPRE"])
partial.r(datavars, x=c("CCPM", "CCPRE"), y="CCBI")
cor.test(datavars[,"CCPM"], datavars[,"CCPSUS"])
cor.test(datavars[,"CCPM"], datavars[,"CCPSEV"])
cor.test(datavars[,"CCPM"], datavars[,"CCSNFA"])

intercorco <- data.frame(cor(datavars[,unlist(mmco2un)], use ="pairwise.complete.obs"))
intercorco
cor.test(datavars[,"COPM"], datavars[,"COPRE"])
cor.test(datavars[,"COPM"], datavars[,"COSNFA"])
cor.test(datavars[,"COPM"], datavars[,"COTRB"])
cor.test(datavars[,"COPM"], datavars[,"COTRI"])
cor.test(datavars[,"COPRC"], datavars[,"CODIC"])


#cor.test(datavars[,"COPSE"], datavars[,"Internal.Control.Conviction"])
#nothing lol
#BI, B (or constructs) and big five and control
cor(datavars[,c("CCBI","CCB","CCPBI","CCPB")], datavars[,unlist(mmper1un)], use ="pairwise.complete.obs")
cor(datavars[,unlist(mmcc2un)], datavars[,unlist(mmper1un)], use ="pairwise.complete.obs")
cor(datavars[,c("COBI","COB","COPBI","COPB")], datavars[,unlist(mmper1un)], use ="pairwise.complete.obs")
cor(datavars[,unlist(mmco2un)], datavars[,unlist(mmper1un)], use ="pairwise.complete.obs")

#Correlation of age and behavior?
cor(datavars[,c("CCBI","CCB","CCPBI","CCPB")], datavars[,"Age"], use ="pairwise.complete.obs")
cor(datavars[,c("COBI","COB","COPBI","COPB")], datavars[,"Age"], use ="pairwise.complete.obs")
#no correlation of age

#Next, t-tests
#sociodemographic variables
#Differences in Gender and Behavioral Intention?
datavars[(datavarsdf$C.Gender %in% 1),"CCBI"]
t.test(datavars[(datavarsdf$C.Gender %in% 1),"CCBI"], datavars[(datavarsdf$C.Gender %in% 2),"CCBI"])
sd(datavars[(datavarsdf$C.Gender %in% 1),"CCBI"])
sd(datavars[(datavarsdf$C.Gender %in% 2),"CCBI"])
#yes!
t.test(datavars[(datavarsdf$C.Gender %in% 1),"COBI"], datavars[(datavarsdf$C.Gender %in% 2),"COBI"])
#also yes
sd(datavars[(datavarsdf$C.Gender %in% 1),"COBI"])
sd(datavars[(datavarsdf$C.Gender %in% 2),"COBI"])
#how about actual behavior
t.test(datavars[(datavarsdf$C.Gender %in% 1),"CCB"], datavars[(datavarsdf$C.Gender %in% 2),"CCB"])
#no
t.test(datavars[(datavarsdf$C.Gender %in% 1),"COB"], datavars[(datavarsdf$C.Gender %in% 2),"COB"])
sd(datavars[(datavarsdf$C.Gender %in% 1),"COB"], na.rm = TRUE)
sd(datavars[(datavarsdf$C.Gender %in% 2),"COB"], na.rm = TRUE)
#tiny bit
#income and CCBI
#high income is 11
t.test(datavars[(datavarsdf$C.Income %in% 11),"CCBI"], datavars[!(datavarsdf$C.Income %in% 11),"CCBI"])
#low income
t.test(datavars[(datavarsdf$C.Income %in% 1),"CCBI"], datavars[!(datavarsdf$C.Income %in% 1),"CCBI"])
#mid income
t.test(datavars[(datavarsdf$C.Income %in% 6),"CCBI"], datavars[!(datavarsdf$C.Income %in% 6),"CCBI"])

#income and CCB
#high income is 11
t.test(datavars[(datavarsdf$C.Income %in% 11),"CCB"], datavars[!(datavarsdf$C.Income %in% 11),"CCB"])
#low income
t.test(datavars[(datavarsdf$C.Income %in% 1),"CCB"], datavars[!(datavarsdf$C.Income %in% 1),"CCB"])
#mid income
t.test(datavars[(datavarsdf$C.Income %in% 6),"CCB"], datavars[!(datavarsdf$C.Income %in% 6),"CCB"])

#income and COBI
#high income is 11
t.test(datavars[(datavarsdf$C.Income %in% 11),"COBI"], datavars[!(datavarsdf$C.Income %in% 11),"COBI"])
#low income
t.test(datavars[(datavarsdf$C.Income %in% 1),"COBI"], datavars[!(datavarsdf$C.Income %in% 1),"COBI"])
#mid income
t.test(datavars[(datavarsdf$C.Income %in% 6),"COBI"], datavars[!(datavarsdf$C.Income %in% 6),"COBI"])

#income and COB
#high income is 11
t.test(datavars[(datavarsdf$C.Income %in% 11),"COB"], datavars[!(datavarsdf$C.Income %in% 11),"COB"])
#low income
t.test(datavars[(datavarsdf$C.Income %in% 1),"COB"], datavars[!(datavarsdf$C.Income %in% 1),"COB"])
#mid income
t.test(datavars[(datavarsdf$C.Income %in% 6),"COB"], datavars[!(datavarsdf$C.Income %in% 6),"COB"])

#education
#doctorate
t.test(datavars[(datavarsdf$C.Education %in% 7),"CCBI"], datavars[!(datavarsdf$C.Education %in% 7),"CCBI"])
t.test(datavars[(datavarsdf$C.Education %in% 7),"CCB"], datavars[!(datavarsdf$C.Education %in% 7),"CCB"])
t.test(datavars[(datavarsdf$C.Education %in% 7),"COBI"], datavars[!(datavarsdf$C.Education %in% 7),"COBI"])
t.test(datavars[(datavarsdf$C.Education %in% 7),"COB"], datavars[!(datavarsdf$C.Education %in% 7),"COB"])
#university degree
t.test(datavars[(datavarsdf$C.Education %in% 6),"CCBI"], datavars[!(datavarsdf$C.Education %in% 6),"CCBI"])
t.test(datavars[(datavarsdf$C.Education %in% 6),"CCB"], datavars[!(datavarsdf$C.Education %in% 6),"CCB"])
t.test(datavars[(datavarsdf$C.Education %in% 6),"COBI"], datavars[!(datavarsdf$C.Education %in% 6),"COBI"])
t.test(datavars[(datavarsdf$C.Education %in% 6),"COB"], datavars[!(datavarsdf$C.Education %in% 6),"COB"])
#abitur
t.test(datavars[(datavarsdf$C.Education %in% 5),"CCBI"], datavars[!(datavarsdf$C.Education %in% 5),"CCBI"])
t.test(datavars[(datavarsdf$C.Education %in% 5),"CCB"], datavars[!(datavarsdf$C.Education %in% 5),"CCB"])
t.test(datavars[(datavarsdf$C.Education %in% 5),"COBI"], datavars[!(datavarsdf$C.Education %in% 5),"COBI"])
t.test(datavars[(datavarsdf$C.Education %in% 5),"COB"], datavars[!(datavarsdf$C.Education %in% 5),"COB"])
#ralschulabschluss
t.test(datavars[(datavarsdf$C.Education %in% 4),"CCBI"], datavars[!(datavarsdf$C.Education %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Education %in% 4),"CCB"], datavars[!(datavarsdf$C.Education %in% 4),"CCB"])
t.test(datavars[(datavarsdf$C.Education %in% 4),"COBI"], datavars[!(datavarsdf$C.Education %in% 4),"COBI"])
t.test(datavars[(datavarsdf$C.Education %in% 4),"COB"], datavars[!(datavarsdf$C.Education %in% 4),"COB"])
#hauptschulabschluss
t.test(datavars[(datavarsdf$C.Education %in% 3),"CCBI"], datavars[!(datavarsdf$C.Education %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Education %in% 3),"CCB"], datavars[!(datavarsdf$C.Education %in% 3),"CCB"])
t.test(datavars[(datavarsdf$C.Education %in% 3),"COBI"], datavars[!(datavarsdf$C.Education %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.Education %in% 3),"COB"], datavars[!(datavarsdf$C.Education %in% 3),"COB"])

#occupation
#fulltime
t.test(datavars[(datavarsdf$C.Occupation %in% 1),"CCBI"], datavars[!(datavarsdf$C.Occupation %in% 1),"CCBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 1),"CCB"], datavars[!(datavarsdf$C.Occupation %in% 1),"CCB"])
t.test(datavars[(datavarsdf$C.Occupation %in% 1),"COBI"], datavars[!(datavarsdf$C.Occupation %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 1),"COB"], datavars[!(datavarsdf$C.Occupation %in% 1),"COB"])
#parttime
t.test(datavars[(datavarsdf$C.Occupation %in% 2),"CCBI"], datavars[!(datavarsdf$C.Occupation %in% 2),"CCBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 2),"CCB"], datavars[!(datavarsdf$C.Occupation %in% 2),"CCB"])
t.test(datavars[(datavarsdf$C.Occupation %in% 2),"COBI"], datavars[!(datavarsdf$C.Occupation %in% 2),"COBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 2),"COB"], datavars[!(datavarsdf$C.Occupation %in% 2),"COB"])
#vocational training
t.test(datavars[(datavarsdf$C.Occupation %in% 3),"CCBI"], datavars[!(datavarsdf$C.Occupation %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 3),"CCB"], datavars[!(datavarsdf$C.Occupation %in% 3),"CCB"])
t.test(datavars[(datavarsdf$C.Occupation %in% 3),"COBI"], datavars[!(datavarsdf$C.Occupation %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 3),"COB"], datavars[!(datavarsdf$C.Occupation %in% 3),"COB"])
#university student
t.test(datavars[(datavarsdf$C.Occupation %in% 4),"CCBI"], datavars[!(datavarsdf$C.Occupation %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 4),"CCB"], datavars[!(datavarsdf$C.Occupation %in% 4),"CCB"])
t.test(datavars[(datavarsdf$C.Occupation %in% 4),"COBI"], datavars[!(datavarsdf$C.Occupation %in% 4),"COBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 4),"COB"], datavars[!(datavarsdf$C.Occupation %in% 4),"COB"])
#other student (none)
#not employed
t.test(datavars[(datavarsdf$C.Occupation %in% 6),"CCBI"], datavars[!(datavarsdf$C.Occupation %in% 6),"CCBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 6),"CCB"], datavars[!(datavarsdf$C.Occupation %in% 6),"CCB"])
t.test(datavars[(datavarsdf$C.Occupation %in% 6),"COBI"], datavars[!(datavarsdf$C.Occupation %in% 6),"COBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 6),"COB"], datavars[!(datavarsdf$C.Occupation %in% 6),"COB"])


#living situation!
#alone
t.test(datavars[(datavarsdf$C.Living %in% 1),"CCBI"], datavars[!(datavarsdf$C.Living %in% 1),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 1),"CCB"], datavars[!(datavarsdf$C.Living %in% 1),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 1),"COBI"], datavars[!(datavarsdf$C.Living %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 1),"COB"], datavars[!(datavarsdf$C.Living %in% 1),"COB"])
#zu zweit partner
t.test(datavars[(datavarsdf$C.Living %in% 2),"CCBI"], datavars[!(datavarsdf$C.Living %in% 2),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 2),"CCB"], datavars[!(datavarsdf$C.Living %in% 2),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 2),"COBI"], datavars[!(datavarsdf$C.Living %in% 2),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 2),"COB"], datavars[!(datavarsdf$C.Living %in% 2),"COB"])
#zu zweit familie
t.test(datavars[(datavarsdf$C.Living %in% 3),"CCBI"], datavars[!(datavarsdf$C.Living %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 3),"CCB"], datavars[!(datavarsdf$C.Living %in% 3),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 3),"COBI"], datavars[!(datavarsdf$C.Living %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 3),"COB"], datavars[!(datavarsdf$C.Living %in% 3),"COB"])
#zu mehr familie
t.test(datavars[(datavarsdf$C.Living %in% 4),"CCBI"], datavars[!(datavarsdf$C.Living %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 4),"CCB"], datavars[!(datavarsdf$C.Living %in% 4),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 4),"COBI"], datavars[!(datavarsdf$C.Living %in% 4),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 4),"COB"], datavars[!(datavarsdf$C.Living %in% 4),"COB"])
#zu zweit wg
t.test(datavars[(datavarsdf$C.Living %in% 5),"CCBI"], datavars[!(datavarsdf$C.Living %in% 5),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 5),"CCB"], datavars[!(datavarsdf$C.Living %in% 5),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 5),"COBI"], datavars[!(datavarsdf$C.Living %in% 5),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 5),"COB"], datavars[!(datavarsdf$C.Living %in% 5),"COB"])
#zu mehreren wg
t.test(datavars[(datavarsdf$C.Living %in% 6),"CCBI"], datavars[!(datavarsdf$C.Living %in% 6),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 6),"CCB"], datavars[!(datavarsdf$C.Living %in% 6),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 6),"COBI"], datavars[!(datavarsdf$C.Living %in% 6),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 6),"COB"], datavars[!(datavarsdf$C.Living %in% 6),"COB"])


#COVID-Questions!
#own risk, 3 is yes, 1 is no
t.test(datavars[(datavarsdf$C.CO.Own.Risk %in% 3),"COBI"], datavars[!(datavarsdf$C.CO.Own.Risk %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Risk %in% 3),"COB"], datavars[!(datavarsdfC.CO.Own.Risk %in% 3),"COB"])
t.test(datavars[(datavarsdf$C.CO.Own.Risk %in% 1),"COBI"], datavars[!(datavarsdf$C.CO.Own.Risk %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Risk %in% 1),"COB"], datavars[!(datavarsdfC.CO.Own.Risk %in% 1),"COB"])
#own infektion, 3 is yes, 1 is no
t.test(datavars[(datavarsdf$C.CO.Own.Infektion %in% 3),"COBI"], datavars[!(datavarsdf$C.CO.Own.Infektion %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Infektion %in% 3),"COB"], datavars[!(datavarsdfC.CO.Own.Infektion %in% 3),"COB"])
t.test(datavars[(datavarsdf$C.CO.Own.Infektion %in% 1),"COBI"], datavars[!(datavarsdf$C.CO.Own.Infektion %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Infektion %in% 1),"COB"], datavars[!(datavarsdfC.CO.Own.Infektion %in% 1),"COB"])
#own severity, 3 is yes, 1 is no
t.test(datavars[(datavarsdf$C.CO.Own.Severity %in% 3),"COBI"], datavars[!(datavarsdf$C.CO.Own.Severity %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Severity %in% 3),"COB"], datavars[!(datavarsdfC.CO.Own.Severity %in% 3),"COB"])
t.test(datavars[(datavarsdf$C.CO.Own.Severity %in% 1),"COBI"], datavars[!(datavarsdf$C.CO.Own.Severity %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Severity %in% 1),"COB"], datavars[!(datavarsdfC.CO.Own.Severity %in% 1),"COB"])
#living with someone at risk C.CO.Living.Risk
t.test(datavars[(datavarsdf$C.CO.Living.Risk %in% 3),"COBI"], datavars[!(datavarsdf$C.CO.Living.Risk %in% 3),"COBI"])
sd(datavars[(datavarsdf$C.CO.Living.Risk %in% 3),"COBI"], na.rm=TRUE)
sd(datavars[!(datavarsdf$C.CO.Living.Risk %in% 3),"COBI"], na.rm=TRUE)

t.test(datavars[(datavarsdf$C.CO.Living.Risk %in% 3),"COB"], datavars[!(datavarsdfC.CO.Living.Risk %in% 3),"COB"])
t.test(datavars[(datavarsdf$C.CO.Living.Risk %in% 1),"COBI"], datavars[!(datavarsdf$C.CO.Living.Risk %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Living.Risk %in% 1),"COB"], datavars[!(datavarsdfC.CO.Living.Risk %in% 1),"COB"])
#knowing others with infection - correlation CO.Others.Infection
cor.test(datavars[,"COBI"], datavars[,"CO.Others.Infection"])
t.test(datavars[(datavarsdf$CO.Others.Infection %in% 0),"COBI"], datavars[!(datavarsdf$CO.Others.Infection %in% 0),"COBI"])
t.test(datavars[(datavarsdf$CO.Others.Infection %in% 0),"COB"], datavars[!(datavarsdfCO.Others.Infection %in% 0),"COB"])
t.test(datavars[(datavarsdf$CO.Others.Infection %in% 5),"COBI"], datavars[!(datavarsdf$CO.Others.Infection %in% 5),"COBI"])
t.test(datavars[(datavarsdf$CO.Others.Infection %in% 5),"COB"], datavars[!(datavarsdfCO.Others.Infection %in% 5),"COB"])
#other's severity CO.Others.Severity
cor.test(datavars[,"COBI"], datavars[,"CO.Others.Severity"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 0),"COBI"], datavars[!(datavarsdf$CO.Others.Severity %in% 0),"COBI"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 0),"COB"], datavars[!(datavarsdfCO.Others.Severity %in% 0),"COB"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 1),"COBI"], datavars[!(datavarsdf$CO.Others.Severity %in% 1),"COBI"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 1),"COB"], datavars[!(datavarsdfCO.Others.Severity %in% 1),"COB"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 3),"COBI"], datavars[!(datavarsdf$CO.Others.Severity %in% 3),"COBI"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 3),"COB"], datavars[!(datavarsdfCO.Others.Severity %in% 3),"COB"])
#occupational C.CO.Own.Ocuppation
t.test(datavars[(datavarsdf$C.CO.Own.Ocuppation %in% 3),"COBI"], datavars[!(datavarsdf$C.CO.Own.Ocuppation %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Ocuppation %in% 3),"COB"], datavars[!(datavarsdfC.CO.Own.Ocuppation %in% 3),"COB"])
t.test(datavars[(datavarsdf$C.CO.Own.Ocuppation %in% 1),"COBI"], datavars[!(datavarsdf$C.CO.Own.Ocuppation %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Ocuppation %in% 1),"COB"], datavars[!(datavarsdfC.CO.Own.Ocuppation %in% 1),"COB"])


#Diet
#Vegans
t.test(datavars[(datavarsdf$C.Diet %in% 1),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 1),"CCBI"])
sd(datavars[(datavarsdf$C.Diet %in% 1),"CCBI"])
sd(datavars[!(datavarsdf$C.Diet %in% 1),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 1),"CCB"], datavars[!(datavarsdfC.Diet %in% 1),"CCB"])
#Vegetarians
t.test(datavars[(datavarsdf$C.Diet %in% 2),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 2),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 2),"CCB"], datavars[!(datavarsdfC.Diet %in% 2),"CCB"])
#3
t.test(datavars[(datavarsdf$C.Diet %in% 3),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 3),"CCB"], datavars[!(datavarsdfC.Diet %in% 3),"CCB"])
#middleground
t.test(datavars[(datavarsdf$C.Diet %in% 4),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 4),"CCB"], datavars[!(datavarsdfC.Diet %in% 4),"CCB"])
#5
t.test(datavars[(datavarsdf$C.Diet %in% 5),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 5),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 5),"CCB"], datavars[!(datavarsdfC.Diet %in% 5),"CCB"])
#daily
t.test(datavars[(datavarsdf$C.Diet %in% 6),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 6),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 6),"CCB"], datavars[!(datavarsdfC.Diet %in% 6),"CCB"])

#CARS!
#Differences in Car Own and Behavioral Intention/Behavior?
t.test(datavars[(datavarsdf$C.Car.Own %in% 5),"CCBI"], datavars[!(datavarsdf$C.Car.Own %in% 5),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Own %in% 5),"CCB"], datavars[!(datavarsdf$C.Car.Own %in% 5),"CCB"])
sd(datavars[(datavarsdf$C.Car.Own %in% 5),"CCBI"])
sd(datavars[!(datavarsdf$C.Car.Own %in% 5),"CCBI"])

#ride along 2
t.test(datavars[(datavarsdf$C.Car.Use.Ride.Along %in% 2),"CCBI"], datavars[!(datavarsdf$C.Car.Use.Ride.Along %in% 2),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Use.Ride.Along %in% 2),"CCB"], datavars[!(datavarsdf$C.Car.Use.Ride.Along %in% 2),"CCB"])
#carshare3
t.test(datavars[(datavarsdf$C.Car.Use.Carshare %in% 3),"CCBI"], datavars[!(datavarsdf$C.Car.Use.Carshare %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Use.Carshare %in% 3),"CCB"], datavars[!(datavarsdf$C.Car.Use.Carshare %in% 3),"CCB"])
#access4
t.test(datavars[(datavarsdf$C.Car.Use.Access %in% 4),"CCBI"], datavars[!(datavarsdf$C.Car.Use.Access %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Use.Access %in% 4),"CCB"], datavars[!(datavarsdf$C.Car.Use.Access %in% 4),"CCB"])

#Driving distance: 1:5
#1
t.test(datavars[(datavarsdf$C.Car.Distance %in% 1),"CCBI"], datavars[!(datavarsdf$C.Car.Distance %in% 1),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Distance %in% 1),"CCB"], datavars[!(datavarsdf$C.Car.Distance %in% 1),"CCB"])
#2
t.test(datavars[(datavarsdf$C.Car.Distance %in% 2),"CCBI"], datavars[!(datavarsdf$C.Car.Distance %in% 2),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Distance %in% 2),"CCB"], datavars[!(datavarsdf$C.Car.Distance %in% 2),"CCB"])
#3
t.test(datavars[(datavarsdf$C.Car.Distance %in% 3),"CCBI"], datavars[!(datavarsdf$C.Car.Distance %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Distance %in% 3),"CCB"], datavars[!(datavarsdf$C.Car.Distance %in% 3),"CCB"])
#4
t.test(datavars[(datavarsdf$C.Car.Distance %in% 4),"CCBI"], datavars[!(datavarsdf$C.Car.Distance %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Distance %in% 4),"CCB"], datavars[!(datavarsdf$C.Car.Distance %in% 4),"CCB"])
#5
t.test(datavars[(datavarsdf$C.Car.Distance %in% 5),"CCBI"], datavars[!(datavarsdf$C.Car.Distance %in% 5),"CCBI"])
sd(datavars[(datavarsdf$C.Car.Distance %in% 5),"CCBI"])
sd(datavars[!(datavarsdf$C.Car.Distance %in% 5),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Distance %in% 5),"CCB"], datavars[!(datavarsdf$C.Car.Distance %in% 5),"CCB"])
sd(datavars[(datavarsdf$C.Car.Distance %in% 5),"CCB"], na.rm=TRUE)
sd(datavars[!(datavarsdf$C.Car.Distance %in% 5),"CCB"], na.rm=TRUE)
