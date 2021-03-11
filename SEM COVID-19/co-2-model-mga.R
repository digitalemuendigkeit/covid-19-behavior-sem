library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)
thm <- seminr_theme_create(construct.compositeA.arrow = "backward", construct.compositeA.use_weights = FALSE, plot.adj = FALSE)
seminr_theme_set(thm)

# Load data and crop to relevant section -c(1:43,48:96,143:145)
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CO") & !paste0("COS", 1:7)) %>% filter(!is.na(COSKN))
datacrop2 <- datafull %>% filter(!is.na(COSKN))
data <- as.matrix(datacrop %>% select(!starts_with("COKN")) %>% cbind("COKN" = rowMeans(datacrop %>% select(starts_with("COKN")))))

# Possible groups: Norm is important when PBC/Self-Efficacy is low
# Possibly differences in age, gender, income
# Possibly differences for combination of high tb and low rb (4 groups)
# Possibly difference for low / high knowledge group
# Possibly differences for low / high distrust

#rbandtb
datalrbltb <- as.data.frame(data) %>% filter(across(c(multi_items("CORB", c(1:2,4:12)), multi_items("COTB", 1:6))) < 4)
datalrbhtb <- as.data.frame(data) %>% filter(across(c(multi_items("CORB", c(1:2,4:12)))) < 4) %>%
  filter(across(c(multi_items("COTB", 1:6))) > 3)
# only 4 sets lol
datalrb <- as.data.frame(data) %>% filter(across(c(multi_items("CORB", c(1:2,4:12)))) < 4)
datahrbltb <- as.data.frame(data) %>% filter(across(c(multi_items("CORB", c(1:2,4:12)))) > 3) %>%
  filter(across(c(multi_items("COTB", 1:6))) < 4)
datahrbhtb <- as.data.frame(data) %>% filter(across(c(multi_items("CORB", c(1:2,4:12)), multi_items("COTB", 1:6))) > 3)
#female and male
dataf <- data[datacrop2$SD2 == 1,]
datam <- data[datacrop2$SD2 == 2,]
#age - generations: genz+y (1980+), genx (1960-1979), genbb(<1960)
datagy <- data[datacrop2$SD1 < 41,]
datagx <- data[datacrop2$SD1 > 40 & datacrop2$SD1 < 61,]
datagb <- data[datacrop2$SD1 > 60,]
# income, approximated on the base of household income
# low: up to 1500 EUR
# medium: up to 4000 EUR
# high: above 4000 EUR
datail <- data[datacrop2$SD5 < 4,]
dataim <- data[datacrop2$SD5 > 3 & datacrop2$SD5 < 9,]
dataih <- data[datacrop2$SD5 > 8 & datacrop2$SD5 < 12,]
# knowledge
datakl <- as.data.frame(data) %>% filter(COKN < 2)
datakh <- as.data.frame(data) %>% filter(COKN > 1.75)
# low/high distrust
datadl <- as.data.frame(data) %>% filter(across(c(multi_items("CODI", 1:9))) < 4)
datadh <- as.data.frame(data) %>% filter(across(c(multi_items("CODI", 1:9))) > 3)

nrow(data)

datalist <- list(data,
              datalrbltb,
              datalrb,
              datahrbltb,
              datahrbhtb,
              dataf,
              datam,
              datagy,
              datagx,
              datagb,
              datail,
              dataim,
              dataih,
              datakl,
              datakh,
              datadl,
              datadh)
nameslist <- c("Original",
               "Low RB low TB",
               "Low RB",
               "High RB low TB",
               "High RB high TB",
               "Female",
               "Male",
               "Gen Y + Z",
               "Gen X",
               "Gen BB",
               "Low Income",
               "Medium Income",
               "High Income",
               "Low Knowledge",
               "High Knowledge",
               "Low Distrust",
               "High Distrust")
datalist2 <- list(data,
                 datahrbltb,
                 datahrbhtb,
                 dataf,
                 datam,
                 datagy,
                 datagx,
                 datagb,
                 datail,
                 dataim,
                 dataih,
                 datakl,
                 datakh,
                 datadl,
                 datadh)
nameslist2 <- c("Original",
               "High RB low TB",
               "High RB high TB",
               "Female",
               "Male",
               "Gen Y + Z",
               "Gen X",
               "Gen BB",
               "Low Income",
               "Medium Income",
               "High Income",
               "Low Knowledge",
               "High Knowledge",
               "Low Distrust",
               "High Distrust")

# Describe measurement model
# Results of co 1 mm eval
# To improve internal consistency reliability of Personal Norm, COPN2 should be deleted.
# Because of a lack of convergent validity, the formative Perceived Self-Efficacy construct should be replaced by a single-item reflective Perceived Self-Efficacy construct.
mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CORB10")),
  composite("Perceived Response Efficacy", multi_items("CORB", 4:6), mode_B),
  composite("Perceived Response Costs", multi_items("CORB", 7:9), mode_B),
  higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), weights =  mode_B),
  composite("Benevolence", multi_items("CODI", 1:3)),
  composite("Competence", multi_items("CODI", 4:6)),
  composite("Integrity", multi_items("CODI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
  composite("Knowledge", single_item("COKN")),
  composite("Perceived Susceptibility", multi_items("COTB", 1:3)),
  composite("Perceived Severity", multi_items("COTB", 4:6)),
  higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), weights = mode_B),
  composite("Personal Moral Norm", multi_items("COPN", c(1,3))),
  composite("Descriptive Norm", multi_items("CODN", 1:2), mode_B),
  composite("Injunctive Norm", multi_items("COIN", 1:2), mode_B),
  higher_composite("Subjective Norm", c("Descriptive Norm", "Injunctive Norm"), weights = mode_B),
  composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B)
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
  paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
)

modelall <- summary(readRDS("SEM COVID-19/Models/model-co-2.RDS"))

modellrbltb <- summary(estimate_pls(datalrbltb, mm, sm))
modellrb <- summary(estimate_pls(datalrb, mm, sm))
modelhrbltb <- summary(estimate_pls(datahrbltb, mm, sm))
modelhrbhtb <- summary(estimate_pls(datahrbhtb, mm, sm))
modelf <- summary(estimate_pls(dataf, mm, sm))
modelm <- summary(estimate_pls(datam, mm, sm))
modelgy <- summary(estimate_pls(datagy, mm, sm))
modelgx <- summary(estimate_pls(datagx, mm, sm))
modelgb <- summary(estimate_pls(datagb, mm, sm))
modelil <- summary(estimate_pls(datail, mm, sm))
modelim <- summary(estimate_pls(dataim, mm, sm))
modelih <- summary(estimate_pls(dataih, mm, sm))
modelkl <- summary(estimate_pls(datakl, mm, sm))
modelkh <- summary(estimate_pls(datakh, mm, sm))
modeldl <- summary(estimate_pls(datadl, mm, sm))
modeldh <- summary(estimate_pls(datadh, mm, sm))

mgc <- function(datalist, nameslist) {
  cdf <- data.frame(group = character(),
                    R2RB = double(),
                    R2TB = double(),
                    R2BI = double(),
                    DBRB = double(),
                    DBTB = double(),
                    KNRB = double(),
                    KNTB = double(),
                    RBBI = double(),
                    TBBI = double(),
                    PNBI = double(),
                    SNBI = double())
  for (i in 1:length(datalist)){
    ms <- summary(estimate_pls(datalist[[i]], mm, sm))
    cdf <- cdf %>% rbind(data.frame(
      group =  nameslist[i],
      R2RB = ms$paths[1,1],
      R2TB = ms$paths[1,2],
    R2BI = ms$paths[1,3],
    DBRB = ms$paths[3,1],
    DBTB = ms$paths[3,2],
    KNRB = ms$paths[4,1],
    KNTB = ms$paths[4,2],
    RBBI = ms$paths[5,3],
    TBBI = ms$paths[6,3],
    PNBI = ms$paths[7,3],
    SNBI = ms$paths[8,3]))
  }
  return(cdf)
}

estc <- mgc(datalist, nameslist)
saveRDS(estc, "SEM COVID-19/co-2-mg-comp.RDS")

mgcb <- function(datalist, nameslist) {
  cdf <- data.frame(group = character(),
                    R2RB = double(),
                    R2TB = double(),
                    R2BI = double(),
                    DBRBbm = double(),
                    DBRBt = double(),
                    DBTBbm = double(),
                    DBTBt = double(),
                    KNRBbm = double(),
                    KNRBt = double(),
                    KNTBbm = double(),
                    KNTBt = double(),
                    RBBIbm = double(),
                    RBBIt = double(),
                    TBBIbm = double(),
                    TBBIt = double(),
                    PNBIbm = double(),
                    PNBIt = double(),
                    SNBIbm = double(),
                    SNBIt = double())
  for (i in 1:length(datalist)){
    m <- estimate_pls(datalist[[i]], mm, sm)
    ms <- summary(m)
    msb <- summary(bootstrap_model(m))
    cdf <- cdf %>% rbind(data.frame(
      group =  nameslist[i],
      R2RB = ms$paths[1,1],
      R2TB = ms$paths[1,2],
      R2BI = ms$paths[1,3],
      DBRBbm = msb$bootstrapped_paths[1,2],
      DBRBt = msb$bootstrapped_paths[1,4],
      DBTBbm = msb$bootstrapped_paths[2,2],
      DBTBt = msb$bootstrapped_paths[2,4],
      KNRBbm = msb$bootstrapped_paths[3,2],
      KNRBt = msb$bootstrapped_paths[3,4],
      KNTBbm = msb$bootstrapped_paths[4,2],
      KNTBt = msb$bootstrapped_paths[4,4],
      RBBIbm = msb$bootstrapped_paths[5,2],
      RBBIt = msb$bootstrapped_paths[5,4],
      TBBIbm = msb$bootstrapped_paths[6,2],
      TBBIt = msb$bootstrapped_paths[6,4],
      PNBIbm = msb$bootstrapped_paths[7,2],
      PNBIt = msb$bootstrapped_paths[7,4],
      SNBIbm = msb$bootstrapped_paths[8,2],
      SNBIt = msb$bootstrapped_paths[8,4]))
  }
  return(cdf)
}

bc <- mgcb(datalist2,nameslist2)
saveRDS(bc, "SEM COVID-19/co-2-mg-boot-comp.RDS")
# notable differences



# # model <- estimate_pls(data, mm, sm)
# # saveRDS(model, "SEM COVID-19/Models/model-co-2.RDS")

# # plot(model)
# # bootmodel <- bootstrap_model(model, nboot = 5000)
# # saveRDS(bootmodel, "SEM COVID-19/Models/model-boot-co-2.RDS")
bootmodel <- readRDS("SEM COVID-19/Models/model-boot-co-2.RDS")
sumbo <- summary(bootmodel)
# # bootfsmodel <- bootstrap_model(model$first_stage_model, nboot = 5000)
# # saveRDS(bootfsmodel, "SEM COVID-19/Models/model-fs-boot-co-2.RDS")
#
# # Models for estimation of convergent validity for formative constructs
# # Perceived Response Efficacy
# mmpre <- constructs(
#   composite("Perceived Response Efficacy Formative", multi_items("CORB", 4:6), mode_B),
#   composite("Perceived Response Efficacy Reflective", single_item("CORB11"))
# )
# smpre <- relationships(
#   paths(from = "Perceived Response Efficacy Formative", to = "Perceived Response Efficacy Reflective")
# )
# rapre <- estimate_pls(data, mmpre, smpre)
# saveRDS(rapre, "SEM COVID-19/Models/rapre-co-2.RDS")
#
# # Perceived Response Costs
# mmprc <- constructs(
#   composite("Perceived Response Costs Formative", multi_items("CORB", 7:9), mode_B),
#   composite("Perceived Response Costs Reflective", single_item("CORB12"))
# )
# smprc <- relationships(
#   paths(from = "Perceived Response Costs Formative", to = "Perceived Response Costs Reflective")
# )
# raprc <- estimate_pls(data, mmprc, smprc)
# saveRDS(raprc, "SEM COVID-19/Models/raprc-co-2.RDS")
#
# # Descriptive Norm
# mmdn <- constructs(
#   composite("Descriptive Norm Formative", multi_items("CODN", 1:2), mode_B),
#   composite("Descriptive Norm Reflective", single_item("CODN4"))
# )
# smdn <- relationships(
#   paths(from = "Descriptive Norm Formative", to = "Descriptive Norm Reflective")
# )
# radn <- estimate_pls(data, mmdn, smdn)
# saveRDS(radn, "SEM COVID-19/Models/radn-co-2.RDS")
#
# # Injunctive Norm
# mmin <- constructs(
#   composite("Injunctive Norm Formative", multi_items("COIN", 1:2), mode_B),
#   composite("Injunctive Norm Reflective", single_item("COIN4"))
# )
# smin <- relationships(
#   paths(from = "Injunctive Norm Formative", to = "Injunctive Norm Reflective")
# )
# rain <- estimate_pls(data, mmin, smin)
# saveRDS(rain, "SEM COVID-19/Models/rain-co-2.RDS")
#
# # Behavioral Intention
# mmbi <- constructs(
#   composite("Behavioral Intention Formative", multi_items("COBI", 1:3), mode_B),
#   composite("Behavioral Intention Reflective", single_item("COBI4"))
# )
# smbi <- relationships(
#   paths(from = "Behavioral Intention Formative", to = "Behavioral Intention Reflective")
# )
# rabi <- estimate_pls(data, mmbi, smbi)
# saveRDS(rabi, "SEM COVID-19/Models/rabi-co-2.RDS")
#
# # Make proxymodel
# proxymm <- constructs(
#   composite("Response Beliefs", multi_items("CORB", c(4:10)), weights = mode_B),
#   composite("Distrusting Beliefs", multi_items("CODI", 1:9), weights = mode_B),
#   composite("Knowledge", single_item("COKN")),
#   composite("Perceived Susceptibility", multi_items("COTB", 1:3)),
#   composite("Perceived Severity", multi_items("COTB", 4:6)),
#   composite("Threat Beliefs", multi_items("COTB", 1:6), weights = mode_B),
#   composite("Personal Moral Norm", multi_items("COPN", c(1,3))),
#   composite("Subjective Norm", c(multi_items("CODN", 1:2), multi_items("COIN", 1:2)), weights = mode_B),
#   composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B)
# )
# proxymodel <- estimate_pls(as.data.frame(data), proxymm, sm)
# saveRDS(proxymodel, "SEM COVID-19/Models/model-proxy-co-2.RDS")
