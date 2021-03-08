library(tidyverse)
library(psych)

#Import Data
# Data used is nonnormal, missing values are not treated
datafull <- read_rds("Data/data-qualcl.RDS")[,-c(1:5,143:145)]
# Replace 7 for CCDN3, CCIN3, CODN3 and COIN3 with NA
datafull$CCDN3[datafull$CCDN3 == 7] <- NA
datafull$CCIN3[datafull$CCIN3 == 7] <- NA
datafull$CODN3[datafull$CODN3 == 7] <- NA
datafull$COIN3[datafull$COIN3 == 7] <- NA

#Make scales from personality data
anper <- data.frame(Variable = c(rep("Extraversion",3),
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
anperun <- data.frame(Variable = unique(anper$Variable))
anperunalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(anperun)){
  x = anperun[i,1]
  anperunalpha <- append(anperunalpha,
                         ((unlist(psych::alpha(datafull[unlist((filter(anper, Variable == {{x}}))["Item"])], check.keys = FALSE)[1]))[2]))
  print(psych::alpha(datafull[unlist((filter(anper, Variable == {{x}}))["Item"])], check.keys = FALSE))
}
anperun <- anperun %>% cbind(Cs.Alpha = anperunalpha)
anperalpha <- anper %>% left_join(anperun)
# Cronbach's alpha is insufficient for all constructs except Big Five Neuroticism
# BFE: No alpha >0.7 attainable through item deletion, choose item with highest item-total correlation (r.drop): BFE2
# BFA: No alpha > 0.7 attainable through item deletion, choose item with highest item-total correlation: BFA1
# BFC: see above,choose BFC2
# BFO: Choose BFO3
# IC: Choose IC2 (content)
# EC: Choose EC1 (content)
keepper <- c("BFE2", "BFA1", "BFC2", "BFN1", "BFN2", "BFN3", "BFO3", "IC2", "EC1")
codeper <- anper %>% filter(Item %in% keepper)

# Make scales for CC data

#Climate Change
mmcc <- (readRDS("SEM Climate Crisis/Models/model-cc-1.RDS"))$measurement_model
ancc <- data.frame(Variable = (unlist(mmcc))[c(T, F, F)],
                   Item = (unlist(mmcc))[c(F, T, F)]) %>%
  filter(!(Item %in% Variable)) %>%
  filter(!(Item == "CCKN")) %>%
  rbind(data.frame(Variable = c(rep("Descriptive Norm", 2),
                                 rep("Injunctive Norm", 2),
                                 paste0(c("Perceived Self-Efficacy",
                                          "Perceived Response Efficacy",
                                          "Perceived Response Costs",
                                          "Behavioral Intention"),
                                        c(rep(" Diet", 4),
                                          rep(" Heating",4),
                                          rep(" Driving",4),
                                          rep(" General",4),
                                          rep("",4))),
                                 "Subjective Knowledge",
                                paste0("Behavior", c(rep("",4),
                                                     " Diet",
                                                     " Heating",
                                                     " Driving",
                                                     " General"))
  ),
  Item = c(paste0("CC",
                       c(paste0("DN", 3:4),
                         paste0("IN", 3:4),
                         paste0("RB", 1:3),
                         "BI1",
                         paste0("RB", 4:6),
                         "BI2",
                         paste0("RB", 7:9),
                         "BI3",
                         rep(c(paste0("RB",10:12),
                               "BI4"),2),
                         "SKN",
                         rep(paste0("B", 1:4),2))))
  ))
anccun <- data.frame(Variable = unique(ancc$Variable))
anccunalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(anccun)){
  x = anccun[i,1]
  anccunalpha <- append(anccunalpha,
                      ifelse(test = ncol(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])]) > 1,
                         ((unlist(psych::alpha(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])], check.keys = FALSE)[1]))[2]), NA))
   # print(ifelse(test = ncol(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])]) > 1,
   #              psych::alpha(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])], check.keys = FALSE), NA))
}
anccun <- anccun %>% cbind(Cs.Alpha = anccunalpha)
anccalpha <- ancc %>% left_join(anccun)
# Cronbach's alpha is sufficient for
# CC Perceived Self-Efficacy
# CC Perceived Response Efficacy
# CC Distrusting Beliefs Benevolence, Competence, Integrity
# CC Perceived Susceptibility
# CC Perceived Severity
# CC Personal Moral Norm
# CC Descriptive Norm
# CC Injunctive Norm
# CC Behavioral Intention
# CC Behavior
# Cronbach's alpha is insufficient for
# CC Perceived Response Costs
psych::alpha(datafull %>% select(c(paste0("CCRB", c(7:9,12)))))
# Without CCRB9:
psych::alpha(datafull %>% select(c(paste0("CCRB", c(7:8,12)))))
# Use CCRB12
codecc <- ancc %>% filter(!(Item %in% paste0("CCRB", 7:9))) %>%
  mutate(Variable = paste0("Climate Crisis ", Variable))

# Make scales for CO data

#COVID-19
mmco <- (readRDS("SEM COVID-19/Models/model-co-1.RDS"))$measurement_model
anco <- data.frame(Variable = (unlist(mmco))[c(T, F, F)],
                   Item = (unlist(mmco))[c(F, T, F)]) %>%
  filter(!(Item %in% Variable)) %>%
  filter(!(Item == "COKN")) %>%
  rbind(data.frame(Variable = c(rep("Descriptive Norm", 2),
                                rep("Injunctive Norm", 2),
                                paste0(c("Perceived Self-Efficacy",
                                         "Perceived Response Efficacy",
                                         "Perceived Response Costs",
                                         "Behavioral Intention"),
                                       c(rep(" Contact", 4),
                                         rep(" App",4),
                                         rep(" Mask",4),
                                         rep(" General",4),
                                         rep("",4))),
                                "Subjective Knowledge",
                                paste0("Behavior", c(rep("",4),
                                                     " Contact",
                                                     " App",
                                                     " Mask",
                                                     " General"))
  ),
  Item = c(paste0("CO",
                  c(paste0("DN", 3:4),
                    paste0("IN", 3:4),
                    paste0("RB", 1:3),
                    "BI1",
                    paste0("RB", 4:6),
                    "BI2",
                    paste0("RB", 7:9),
                    "BI3",
                    rep(c(paste0("RB",10:12),
                          "BI4"),2),
                    "SKN",
                    rep(paste0("B", 1:4),2))))
  )%>%
    rbind(data.frame(Variable = "Perceived Self-Efficacy", Item = "CORB3"))
  )
ancoun <- data.frame(Variable = unique(anco$Variable))
ancounalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(ancoun)){
  x = ancoun[i,1]
  ancounalpha <- append(ancounalpha,
                        ifelse(test = ncol(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])]) > 1,
                               ((unlist(psych::alpha(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])], check.keys = FALSE)[1]))[2]), NA))
  # print(ifelse(test = ncol(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])]) > 1,
  #              psych::alpha(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])], check.keys = FALSE), NA))
}
ancoun <- ancoun %>% cbind(Cs.Alpha = ancounalpha)
ancoalpha <- anco %>% left_join(ancoun)
# Cronbach's alpha is sufficient for all constructs
codeco <- anco %>% mutate(Variable = paste0("COVID-19 ", Variable))

codefull <- codeco %>% union(codecc) %>% union(codeper)
codefullun <- unique(codefull$Variable)

datacoded <- data.frame(Dummy = character(851L))
for (i in 1:length(codefullun)){
  x = codefullun[i]
  datacoded <- datacoded %>%
    cbind(Bob = rowMeans((datafull %>% select(unlist((codefull %>% filter(Variable == {{x}}))[,2]))), na.rm = TRUE))
}
datacoded <- datacoded[,-1]
colnames(datacoded) <- codefullun

psych::describe(datacoded)

bighappycor <- cor(as.matrix(datacoded), use = "pairwise.complete.obs")
summarise(bighappycor %>% max())

## Descriptive Statistics
desstatcor1 <- rownames_to_column(data.frame(psych::describe(datacoded[,1:48]))) %>% transmute(
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
