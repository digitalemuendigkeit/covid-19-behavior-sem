library(tidyverse)
library(seminr)

# Data Loading ----
# Load data and crop to relevant section -c(1:43,48:96,143:145)
datafull <- read_rds(here::here("Data",
                                "open",
                                "S1-data-nm.RDS"))

# Load copula
PRE_star <- readRDS( here::here("SEM COVID-19",
                              "Models",
                              "copula-pre-model-co-3-b-2.RDS"))
# append to data
data <- datafull %>%
  select(starts_with("CO")) %>%
  select(-paste0("COS", 1:7)) %>%
  filter(!is.na(COSKN)) %>% # other condition
  mutate(COKN = rowMeans(across(starts_with("COKN")))) %>%
  cbind(PRE_star)



# Model Description ----

# Describe measurement model
# Inclusion of Perceived Response Efficacy copula

mm <- constructs(
  composite("Perceived Response Efficacy", multi_items("CORB", 4:6), mode_B),
  composite("Perceived Response Costs", multi_items("CORB", 7:9), mode_B),
  composite("Benevolence", multi_items("CODI", 1:3)),
  composite("Competence", multi_items("CODI", 4:6)),
  composite("Integrity", multi_items("CODI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
  composite("Knowledge", single_item("COKN")),
  composite("Personal Moral Norm", multi_items("COPN", c(1,3))),
  composite("Descriptive Norm", multi_items("CODN", 1:2), mode_B),
  composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B),
  composite("Copula Perceived Response Efficacy", single_item("PRE_star"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs"), to = c("Perceived Response Efficacy", "Perceived Response Costs")),
  paths(from = c("Knowledge"), to = c("Perceived Response Efficacy")),
  paths(from = c("Perceived Response Efficacy", "Perceived Response Costs", "Personal Moral Norm", "Descriptive Norm", "Copula Perceived Response Efficacy"), to = "Behavioral Intention")
)

# Model Estimation ----

model <- estimate_pls(data, mm, sm)
saveRDS(model, "SEM COVID-19/Models/model-co-3-b-2-cop.RDS")
bootmodel <- bootstrap_model(model, nboot = 5000)
saveRDS(bootmodel, "SEM COVID-19/Models/model-boot-co-3-b-2-cop.RDS")
bootfsmodel <- bootstrap_model(model$first_stage_model, nboot = 5000)
saveRDS(bootfsmodel, "SEM COVID-19/Models/model-fs-boot-co-3-b-2-cop.RDS")

# Proxy Model ----

# Make proxy model for PLSpredict
constructs(
  composite("Perceived Response Efficacy", multi_items("CORB", 4:6), mode_B),
  composite("Perceived Response Costs", multi_items("CORB", 7:9), mode_B),
  composite("Distrusting Beliefs", multi_items("CODI", 1:9), weights = mode_B),
  composite("Knowledge", single_item("COKN")),
  composite("Personal Moral Norm", multi_items("COPN", c(1,3))),
  composite("Descriptive Norm", multi_items("CODN", 1:2), mode_B),
  composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B),
  composite("Copula Perceived Response Efficacy", single_item("PRE_star"))
)

proxymodel <- estimate_pls(as.data.frame(data), proxymm, sm)
saveRDS(proxymodel, "SEM COVID-19/Models/model-proxy-co-3-b-2-cop.RDS")

# Convergent Validity ----

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
saveRDS(rapre, "SEM COVID-19/Models/rapre-co-3-b-2-cop.RDS")

# Perceived Response Costs
mmprc <- constructs(
  composite("Perceived Response Costs Formative", multi_items("CORB", 7:9), mode_B),
  composite("Perceived Response Costs Reflective", single_item("CORB12"))
)
smprc <- relationships(
  paths(from = "Perceived Response Costs Formative", to = "Perceived Response Costs Reflective")
)
raprc <- estimate_pls(data, mmprc, smprc)
saveRDS(raprc, "SEM COVID-19/Models/raprc-co-3-b-2-cop.RDS")

# Descriptive Norm
mmdn <- constructs(
  composite("Descriptive Norm Formative", multi_items("CODN", 1:2), mode_B),
  composite("Descriptive Norm Reflective", single_item("CODN4"))
)
smdn <- relationships(
  paths(from = "Descriptive Norm Formative", to = "Descriptive Norm Reflective")
)
radn <- estimate_pls(data, mmdn, smdn)
saveRDS(radn, "SEM COVID-19/Models/radn-co-3-b-2-cop.RDS")

# Behavioral Intention
mmbi <- constructs(
  composite("Behavioral Intention Formative", multi_items("COBI", 1:3), mode_B),
  composite("Behavioral Intention Reflective", single_item("COBI4"))
)
smbi <- relationships(
  paths(from = "Behavioral Intention Formative", to = "Behavioral Intention Reflective")
)
rabi <- estimate_pls(data, mmbi, smbi)
saveRDS(rabi, "SEM COVID-19/Models/rabi-co-3-b-2-cop.RDS")
