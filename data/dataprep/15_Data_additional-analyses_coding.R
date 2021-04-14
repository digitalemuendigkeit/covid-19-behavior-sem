#
# This file conducts coding and tests scale reliability and
#

library(tidyverse)
library(psych)
library(Hmisc)

#Import Data
# Data used is nonnormal, missing values are not treated
datafull <- read_rds("data/anonymized/data-qualcl-inc.RDS")
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
                                 rep("Openness to Experience",3),
                                 rep("Internal Control Conviction",2),
                                 rep("External Control Conviction",2)),
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
anperalpha2 <- anperalpha %>% filter(Item %in% keepper)
anperalpha2[anperalpha2 < 0.7] <- NA

# Make scales for CC data

#Climate Change
# mmcc <- (readRDS("SEM Climate Crisis/Models/model-cc-1.RDS"))$measurement_model
# ancc <- data.frame(Variable = (unlist(mmcc))[c(T, F, F)],
#                    Item = (unlist(mmcc))[c(F, T, F)]) %>%
#   filter(!(Item %in% Variable)) %>%
#   filter(!(Item == "CCKN")) %>%
#   rbind(data.frame(Variable = c(rep("Descriptive Norm", 2),
#                                  rep("Injunctive Norm", 2),
#                                  paste0(c("Perceived Self-Efficacy",
#                                           "Perceived Response Efficacy",
#                                           "Perceived Response Costs",
#                                           "Behavioral Intention"),
#                                         c(rep(" Diet", 4),
#                                           rep(" Heating",4),
#                                           rep(" Driving",4),
#                                           rep(" General",4),
#                                           rep("",4))),
#                                  "Subjective Knowledge",
#                                 paste0("Behavior", c(rep("",4),
#                                                      " Diet",
#                                                      " Heating",
#                                                      " Driving",
#                                                      " General"))
#   ),
#   Item = c(paste0("CC",
#                        c(paste0("DN", 3:4),
#                          paste0("IN", 3:4),
#                          paste0("RB", 1:3),
#                          "BI1",
#                          paste0("RB", 4:6),
#                          "BI2",
#                          paste0("RB", 7:9),
#                          "BI3",
#                          rep(c(paste0("RB",10:12),
#                                "BI4"),2),
#                          "SKN",
#                          rep(paste0("B", 1:4),2))))
#   ))
# ancc[ancc == "Benevolence"] <- "Distrusting Beliefs Benevolence"
# ancc[ancc == "Competence"] <- "Distrusting Beliefs Competence"
# ancc[ancc == "Integrity"] <- "Distrusting Beliefs Integrity"
# anccun <- data.frame(Variable = unique(ancc$Variable))
# anccunalpha <- c()
# # Analyze Cronbach's Alpha
# for (i in 1:nrow(anccun)){
#   x = anccun[i,1]
#   anccunalpha <- append(anccunalpha,
#                       ifelse(test = ncol(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])]) > 1,
#                          ((unlist(psych::alpha(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])], check.keys = FALSE)[1]))[2]), NA))
#    # print(ifelse(test = ncol(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])]) > 1,
#    #              psych::alpha(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])], check.keys = FALSE), NA))
# }
# anccun <- anccun %>% cbind(Cs.Alpha = anccunalpha)
# anccalpha <- ancc %>% left_join(anccun)
# # Cronbach's alpha is sufficient for
# # CC Perceived Self-Efficacy
# # CC Perceived Response Efficacy
# # CC Distrusting Beliefs Benevolence, Competence, Integrity
# # CC Perceived Susceptibility
# # CC Perceived Severity
# # CC Personal Moral Norm
# # CC Descriptive Norm
# # CC Injunctive Norm
# # CC Behavioral Intention
# # CC Behavior
# # Cronbach's alpha is insufficient for
# # CC Perceived Response Costs
# psych::alpha(datafull %>% select(c(paste0("CCRB", c(7:9,12)))))
# # Without CCRB9:
# psych::alpha(datafull %>% select(c(paste0("CCRB", c(7:8,12)))))
# # Use CCRB12
# codecc <- ancc %>% filter(!(Item %in% paste0("CCRB", 7:9))) %>%
#   mutate(Variable = paste0("Climate Crisis ", Variable))

# Make scales for CO data

#COVID-19
mmco <- (readRDS("sem-covid-19/Models/model-co-1.RDS"))$measurement_model
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
anco[anco == "Benevolence"] <- "Distrusting Beliefs Benevolence"
anco[anco == "Competence"] <- "Distrusting Beliefs Competence"
anco[anco == "Integrity"] <- "Distrusting Beliefs Integrity"
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

codefull <- codeco %>%
  # union(codecc) %>%
  union(codeper)
codefullun <- unique(codefull$Variable)

datacoded <- data.frame(Dummy = character(length = nrow(datafull)))
for (i in 1:length(codefullun)){
  x = codefullun[i]
  datacoded <- datacoded %>%
    cbind(Bob = rowMeans((datafull %>% select(unlist((codefull %>% filter(Variable == {{x}}))[,2]))), na.rm = TRUE))
}
datacoded <- datacoded[,-1]
colnames(datacoded) <- codefullun

# Make descriptive statistics
descstat <- data.frame(Variable = rownames(psych::describe(datacoded)), psych::describe(datacoded)) %>%
  left_join(rbind(anperalpha,
#                  (anccalpha %>% mutate(Variable = paste0("Climate Crisis ", Variable))),
                   (ancoalpha %>% mutate(Variable = paste0("COVID-19 ", Variable)))) %>%
              group_by(Variable) %>%
              mutate(Items = paste0(Item, collapse = ", "),
                     Item = NULL) %>%
              distinct(Variable, .keep_all = TRUE)

  )


# order by groups and alphabetical
descstatalph <- descstat[c(order(descstat$Variable)[order(descstat$Variable) %in% grep("Climate Crisis", descstat$Variable)],
                          order(descstat$Variable)[order(descstat$Variable) %in% grep("COVID-19", descstat$Variable)],
                          66:72),]

descstatshort <- descstat %>%
  transmute(
  Variable = Variable,
  Items = Items,
  n = n,
  Cs.Alpha = ifelse(is.na(Cs.Alpha), "-", round(Cs.Alpha, 2)),
  Mean = round(mean,2),
  SD = round(sd,2)
)

decstatcovid <- descstatshort %>%
  filter(!grepl('Climate', Variable)) %>%
  filter(!((grepl('Perceived', Variable) |
            grepl('Intention', Variable)) &
             (grepl('Contact', Variable) |
                grepl('Mask', Variable) |
                grepl('App', Variable) |
                grepl('General', Variable)))) %>%
  mutate(Variable = gsub('COVID-19 ', '', Variable))




# decstatcovidlong <- descstat %>%
#   mutate(Cs.Alpha = ifelse(is.na(Cs.Alpha),
#                            "-",
#                            Cs.Alpha)) %>%
#   select(c(Variable, Items, n, Cs.Alpha))
#   filter(!grepl('Climate', Variable)) %>%
#   mutate(Variable = gsub('COVID-19 ', '', Variable))

write_excel_csv(decstatcovid, "data/anonymized/descriptive-statistics.csv", delim = ";")

# Make only covid descriptive stats
covidpls <- readRDS("sem-covid-19/Models/model-co-3-a-1.RDS")
sumcopls <- summary(covidpls)
covidpls$mmMatrix
anco
codes <- anco %>% filter(Item %in% covidpls$mmMatrix[,2])

# Append Knowledge data
datacodedkn <- datacoded %>%
  cbind(datafull %>% select((contains("KN")&!(contains("SKN")))))
datacodedkn$'Climate Crisis Knowledge Sum Correct' <- rowSums(datacodedkn %>% select(contains("CCKN")) == 3)
datacodedkn$'Climate Crisis Knowledge Sum DK' <- rowSums(datacodedkn %>% select(contains("CCKN")) == 2)
datacodedkn$'Climate Crisis Knowledge Sum Incorrect' <- rowSums(datacodedkn %>% select(contains("CCKN")) == 1)
datacodedkn$'COVID-19 Knowledge Sum Correct' <- rowSums(datacodedkn %>% select(contains("COKN")) == 3)
datacodedkn$'COVID-19 Knowledge Sum DK' <- rowSums(datacodedkn %>% select(contains("COKN")) == 2)
datacodedkn$'COVID-19 Knowledge Sum Incorrect' <- rowSums(datacodedkn %>% select(contains("COKN")) == 1)

descstatcovidlong <- data.frame(Variable = rownames(psych::describe(datacodedkn)),
                                psych::describe(datacodedkn)) %>%
  filter(!grepl('Climate', Variable), !grepl('CCKN', Variable), !grepl('COKN', Variable)) %>%
  mutate(Variable = gsub('COVID-19 ', '', Variable)) %>%
  left_join(rbind(anperalpha2,
                  ancoalpha) %>%
              group_by(Variable) %>%
              mutate(Items = paste0(Item, collapse = ", "),
                     Item = NULL) %>%
              distinct(Variable, .keep_all = TRUE)) %>%
  select(c(Variable, Items, n, Cs.Alpha, mean, sd, median, min, max, skew, kurtosis))
descstatcovidlong[grepl("Knowledge Sum", descstatcovidlong$Variable),] <- descstatcovidlong[grepl("Knowledge Sum", descstatcovidlong$Variable),] %>%
  mutate(Items = c("COKN1, COKN2, COKN2_1, COKN3, COKN4, COKN5, COKN6"))
saveRDS(descstatcovidlong, "data/anonymized/desc-stat-covid.RDS")


# Append demographic and other vars
datarest <- datafull %>% dplyr::transmute(Gender = car::recode(SD2,
                                                           "1 = 'female';
                                                              2 = 'male';
                                                              3 = 'other'",
                                                           as.factor = TRUE),
                                      Age = SD1,
                                      Education = car::recode(SD3,
                                                              "1 = 'still a student';
                                                                 2 = 'dropped out of school';
                                                                 c(3,4) = 'secondary school leaving certificate';
                                                                 5 = 'university entrance qualification';
                                                                 6 = 'university degree';
                                                                 7 = 'doctorate';
                                                                 8 =  'a different level of education'",
                                                              as.factor = TRUE,
                                                              levels = c('still a student',
                                                                         'dropped out of school',
                                                                         'secondary school leaving certificate',
                                                                         'university entrance qualification',
                                                                         'university degree',
                                                                         'doctorate',
                                                                         'a different level of education')),
                                      Occupation = car::recode(SD4,
                                                               "1 = 'employed full-time';
                                                                  2 = 'employed part-time';
                                                                  3 = 'in vocational training';
                                                                  4 = 'student (university)';
                                                                  5 = 'student (school)';
                                                                  6 = 'not in paid employment'",
                                                               as.factor = TRUE,
                                                               levels = c('employed full-time',
                                                                          'employed part-time',
                                                                          'in vocational training',
                                                                          'student (university)',
                                                                          'student (school)',
                                                                          'not in paid employment')),
                                      Income = car::recode(SD5,
                                                           "1 = 'up to 450 EUR';
                                                                2 = 'between 451 and 1000 EUR';
                                                                3 = 'between 1001 and 1500 EUR';
                                                                4 = 'between 1501 and 2000 EUR';
                                                                5 = 'between 2001 and 2500 EUR';
                                                                6 = 'between 2501 and 3000 EUR';
                                                                7 = 'between 3001 and 3500 EUR';
                                                                8 = 'between 3501 and 4000 EUR';
                                                                9 = 'between 4001 and 4500 EUR';
                                                                10 = 'between 4501 and 5000 EUR';
                                                                11 = 'more than 5000 EUR';
                                                                12 = 'not specified';
                                                                else = 'not specified'",
                                                           as.factor = TRUE,
                                                           levels = c('up to 450 EUR',
                                                                      'between 451 and 1000 EUR',
                                                                      'between 1001 and 1500 EUR',
                                                                      'between 1501 and 2000 EUR',
                                                                      'between 2001 and 2500 EUR',
                                                                      'between 2501 and 3000 EUR',
                                                                      'between 3001 and 3500 EUR',
                                                                      'between 3501 and 4000 EUR',
                                                                      'between 4001 and 4500 EUR',
                                                                      'between 4501 and 5000 EUR',
                                                                      'more than 5000 EUR',
                                                                      'not specified')),
                                      Income2 = car::recode(SD5,
                                                            "c(1,2) = 'up to 1000 EUR';
                                                                c(3,4) = 'between 1001 and 2000 EUR';
                                                                c(5,6) = 'between 2001 and 3000 EUR';
                                                                c(7,8) = 'between 3001 and 4000 EUR';
                                                                c(9,10) = 'between 4001 and 5000 EUR';
                                                                11 = 'more than 5000 EUR';
                                                                12 = 'not specified';
                                                                else = 'not specified'",
                                                            as.factor = TRUE,
                                                            levels = c('up to 1000 EUR',
                                                                       'between 1001 and 2000 EUR',
                                                                       'between 2001 and 3000 EUR',
                                                                       'between 3001 and 4000 EUR',
                                                                       'between 4001 and 5000 EUR',
                                                                       'more than 5000 EUR',
                                                                       'not specified')),
                                      'Household Size' = SD6,
                                      'Household Children' = ifelse(is.na(SD7),0,as.numeric(SD7)),
                                      'Own Risk COVID-19' = car::recode(COS1,
                                                                        "2 = 'do not know';
                                                                        1 = 'no';
                                                                        3 = 'yes'",
                                                                        as.factor = TRUE,
                                                                        levels = c('no', 'do not know', 'yes')),
                                      'Own Infection COVID-19' = car::recode(COS2,
                                                                             "2 = 'do not know';
                                                                        1 = 'no';
                                                                        3 = 'yes'",
                                                                             as.factor = TRUE,
                                                                             levels = c('no', 'do not know', 'yes')),
                                      'Own Hospitalisation COVID-19' = car::recode(COS3,
                                                                                   "1 = 'no';
                                                                                    3 = 'yes'",
                                                                                   as.factor = TRUE,
                                                                                   levels = c('no', 'yes')),
                                      'Cohabitate Risk COVID-19' = car::recode(COS4,
                                                                        "2 = 'do not know';
                                                                        1 = 'no';
                                                                        3 = 'yes';
                                                                        NA = 'no'",
                                                                        as.factor = TRUE,
                                                                        levels = c('no', 'do not know', 'yes')),
                                      'Acquaintances Infection COVID-19' = COS5,
                                      'Acquaintances Hospitalisation COVID-19' = ifelse(is.na(COS6), 0, COS6),
                                      'Work From Home COVID-19' = car::recode(COS7,
                                                                              "1 = 'no';
                                                                                    3 = 'yes'",
                                                                              as.factor = TRUE,
                                                                              levels = c('no', 'yes')),
                                      'AD S1 Count COVID-19' = Count210112,
                                      'AD S2 Count COVID-19' = Count210201,
                                      'AD S1 Incidence COVID-19' = Incidence210112,
                                      'AD S2 Incidence COVID-19' = Incidence210201,
                                      'Diet' = car::recode(CCS1,
                                                           "1 = 'vegan';
                                                           2 = 'vegetarian';
                                                           3 = 'eat meat less than once a week';
                                                           4 = 'eat meat once a week';
                                                           5 = 'eat meat several times a week';
                                                           6 = 'eat meat every day'",
                                                           as.factor = TRUE,
                                                           levels = c('vegan',
                                                                     'vegetarian',
                                                                      'eat meat less than once a week',
                                                                      'eat meat once a week',
                                                                      'eat meat several times a week',
                                                                      'eat meat every day')),
                                      'Car Ownership' = car::recode(CCS2,
                                                                    "1 = 'does not own a car';
                                                                    2 = 'does not own a car but can access one';
                                                                    3 = 'owns a car'",
                                                                    as.factor = TRUE,
                                                                    levels = c('does not own a car',
                                                                                'does not own a car but can access one',
                                                                                'owns a car')),
                                      'Distance Driven Private' = car::recode(CCS3,
                                                                              "1 = 'none';
                                                                              2 = 'up to 5,000 km';
                                                                              3 = 'between 5,001 and 10,000 km';
                                                                              4 = 'between 10,001 and 15,000 km';
                                                                              5 = 'between 15,001 and 20,000 km';
                                                                              6 = 'more than 20.000 km';
                                                                              7 = 'do not know'",
                                                                              as.factor = TRUE,
                                                                              levels = c('none',
                                                                                        'up to 5,000 km',
                                                                                        'between 5,001 and 10,000 km',
                                                                                        'between 10,001 and 15,000 km',
                                                                                        'between 15,001 and 20,000 km',
                                                                                        'more than 20.000 km',
                                                                                        'do not know')),
                                      'Distance Driven Business' = car::recode(CCS4,
                                                                               "1 = 'none';
                                                                              2 = 'up to 5,000 km';
                                                                              3 = 'between 5,001 and 10,000 km';
                                                                              4 = 'between 10,001 and 15,000 km';
                                                                              5 = 'between 15,001 and 20,000 km';
                                                                              6 = 'more than 20.000 km';
                                                                              7 = 'do not know'",
                                                                              as.factor = TRUE,
                                                                              levels = c('none',
                                                                                         'up to 5,000 km',
                                                                                         'between 5,001 and 10,000 km',
                                                                                         'between 10,001 and 15,000 km',
                                                                                         'between 15,001 and 20,000 km',
                                                                                         'more than 20.000 km',
                                                                                         'do not know'))
                                      )
datacodedfull <- datacodedkn %>% cbind(datarest)
saveRDS(datacodedfull, "data/anonymized/datacodedfull.RDS")
