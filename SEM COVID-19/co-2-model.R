library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)
thm <- seminr_theme_create(construct.compositeA.arrow = "backward", construct.compositeA.use_weights = FALSE, plot.adj = FALSE)
seminr_theme_set(thm)

# Load data and crop to relevant section -c(1:43,48:96,143:145)
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CO") & !paste0("COS", 1:7)) %>% filter(!is.na(COSKN))
data <- as.matrix(datacrop %>% select(!starts_with("COKN")) %>% cbind("COKN" = rowMeans(datacrop %>% select(starts_with("COKN")))))

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


# model <- estimate_pls(data, mm, sm)
# saveRDS(model, "SEM COVID-19/Models/model-co-2.RDS")
# model <- readRDS("SEM COVID-19/Models/model-co-2.RDS")
# plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM COVID-19/Models/model-boot-co-2.RDS")
# bootmodel <- readRDS("SEM COVID-19/Models/model-boot-co-2.RDS")
# bootfsmodel <- bootstrap_model(model$first_stage_model, nboot = 5000)
saveRDS(bootfsmodel, "SEM COVID-19/Models/model-fs-boot-co-2.RDS")

# Models for estimation of convergent validity for formative constructs
# Perceived Response Efficacy
mmpre <- constructs(
  composite("Perceived Response Efficacy Formative", multi_items("CORB", 4:6), mode_B),
  composite("Perceived Response Efficacy Reflective", single_item("CORB11"))
)
smpre <- relationships(
  paths(from = "Perceived Response Efficacy Formative", to = "Perceived Response Efficacy Reflective")
)
rapre <- estimate_pls(data, mmpre, smpre)
saveRDS(rapre, "SEM COVID-19/Models/rapre-co-2.RDS")

# Perceived Response Costs
mmprc <- constructs(
  composite("Perceived Response Costs Formative", multi_items("CORB", 7:9), mode_B),
  composite("Perceived Response Costs Reflective", single_item("CORB12"))
)
smprc <- relationships(
  paths(from = "Perceived Response Costs Formative", to = "Perceived Response Costs Reflective")
)
raprc <- estimate_pls(data, mmprc, smprc)
saveRDS(raprc, "SEM COVID-19/Models/raprc-co-2.RDS")

# Descriptive Norm
mmdn <- constructs(
  composite("Descriptive Norm Formative", multi_items("CODN", 1:2), mode_B),
  composite("Descriptive Norm Reflective", single_item("CODN4"))
)
smdn <- relationships(
  paths(from = "Descriptive Norm Formative", to = "Descriptive Norm Reflective")
)
radn <- estimate_pls(data, mmdn, smdn)
saveRDS(radn, "SEM COVID-19/Models/radn-co-2.RDS")

# Injunctive Norm
mmin <- constructs(
  composite("Injunctive Norm Formative", multi_items("COIN", 1:2), mode_B),
  composite("Injunctive Norm Reflective", single_item("COIN4"))
)
smin <- relationships(
  paths(from = "Injunctive Norm Formative", to = "Injunctive Norm Reflective")
)
rain <- estimate_pls(data, mmin, smin)
saveRDS(rain, "SEM COVID-19/Models/rain-co-2.RDS")

# Behavioral Intention
mmbi <- constructs(
  composite("Behavioral Intention Formative", multi_items("COBI", 1:3), mode_B),
  composite("Behavioral Intention Reflective", single_item("COBI4"))
)
smbi <- relationships(
  paths(from = "Behavioral Intention Formative", to = "Behavioral Intention Reflective")
)
rabi <- estimate_pls(data, mmbi, smbi)
saveRDS(rabi, "SEM COVID-19/Models/rabi-co-2.RDS")

# Make proxymodel
proxymm <- constructs(
  composite("Response Beliefs", multi_items("CORB", c(4:10)), weights = mode_B),
  composite("Distrusting Beliefs", multi_items("CODI", 1:9), weights = mode_B),
  composite("Knowledge", single_item("COKN")),
  composite("Perceived Susceptibility", multi_items("COTB", 1:3)),
  composite("Perceived Severity", multi_items("COTB", 4:6)),
  composite("Threat Beliefs", multi_items("COTB", 1:6), weights = mode_B),
  composite("Personal Moral Norm", multi_items("COPN", c(1,3))),
  composite("Subjective Norm", c(multi_items("CODN", 1:2), multi_items("COIN", 1:2)), weights = mode_B),
  composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B)
)
proxymodel <- estimate_pls(as.data.frame(data), proxymm, sm)
saveRDS(proxymodel, "SEM COVID-19/Models/model-proxy-co-2.RDS")
