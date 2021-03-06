library(tidyverse)
library(seminr)

# Data Loading ----
data <- read_rds(here::here("data",
                                 "open",
                                 "model-data.RDS")) %>%
  mutate(COKN = rowMeans(across(starts_with("COKN"))))

# Model Description ----

# Describe measurement model
mm <- constructs(
  composite("Perceived Self-Efficacy", multi_items("CORB", 1:2), mode_B),
  composite("Perceived Response Efficacy", multi_items("CORB", 4:6), mode_B),
  composite("Perceived Response Costs", multi_items("CORB", 7:9), mode_B),
  higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), weights = mode_B),
  composite("Benevolence", multi_items("CODI", 1:3)),
  composite("Competence", multi_items("CODI", 4:6)),
  composite("Integrity", multi_items("CODI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
  composite("Knowledge", single_item("COKN")),
  composite("Perceived Susceptibility", multi_items("COTB", 1:3)),
  composite("Perceived Severity", multi_items("COTB", 4:6)),
  higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), weights = mode_B),
  composite("Personal Moral Norm", multi_items("COPN", 1:3)),
  composite("Descriptive Norm", multi_items("CODN", 1:2), mode_B),
  composite("Injunctive Norm", multi_items("COIN", 1:2), mode_B),
  higher_composite("Subjective Norm", c("Descriptive Norm", "Injunctive Norm"), weights = mode_B),
  composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B)
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
  paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
)


# Model Estimation ----

model <- estimate_pls(data, mm, sm)

saveRDS(model, "sem-covid-19/models/model-co-1.RDS")
bootmodel <- bootstrap_model(model, nboot = 5000)
saveRDS(bootmodel, "sem-covid-19/models/model-boot-co-1.RDS")
bootfsmodel <- bootstrap_model(model$first_stage_model, nboot = 5000)
saveRDS(bootfsmodel, "sem-covid-19/models/model-fs-boot-co-1.RDS")




# Proxy Model ----
# Make proxy model for PLSpredict
proxymm <- constructs(
  composite("Response Beliefs", multi_items("CORB", c(1:2, 4:9)), weights = mode_B),
  composite("Distrusting Beliefs", multi_items("CODI", 1:9), weights = mode_B),
  composite("Knowledge", single_item("COKN")),
  composite("Perceived Susceptibility", multi_items("COTB", 1:3)),
  composite("Perceived Severity", multi_items("COTB", 4:6)),
  composite("Threat Beliefs", multi_items("COTB", 1:6), weights = mode_B),
  composite("Personal Moral Norm", multi_items("COPN", 1:3)),
  composite("Subjective Norm", c(multi_items("CODN", 1:2), multi_items("COIN", 1:2)), weights = mode_B),
  composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B)
)
proxymodel <- estimate_pls(as.data.frame(data), proxymm, sm)
saveRDS(proxymodel, "sem-covid-19/models/model-proxy-co-1.RDS")




# Convergent Validity ----

# Models for estimation of convergent validity for formative constructs
# Perceived Self-Efficacy
mmpse <- constructs(
  composite("Perceived Self-Efficacy Formative", multi_items("CORB", 1:2), mode_B),
  composite("Perceived Self-Efficacy Reflective", single_item("CORB10"))
)
smpse <- relationships(
  paths(from = "Perceived Self-Efficacy Formative", to = "Perceived Self-Efficacy Reflective")
)
rapse <- estimate_pls(data, mmpse, smpse)
saveRDS(rapse, "sem-covid-19/models/rapse-co-1.RDS")

# Perceived Response Efficacy
mmpre <- constructs(
  composite("Perceived Response Efficacy Formative", multi_items("CORB", 4:6), mode_B),
  composite("Perceived Response Efficacy Reflective", single_item("CORB11"))
)
smpre <- relationships(
  paths(from = "Perceived Response Efficacy Formative", to = "Perceived Response Efficacy Reflective")
)
rapre <- estimate_pls(data, mmpre, smpre)
saveRDS(rapre, "sem-covid-19/models/rapre-co-1.RDS")

# Perceived Response Costs
mmprc <- constructs(
  composite("Perceived Response Costs Formative", multi_items("CORB", 7:9), mode_B),
  composite("Perceived Response Costs Reflective", single_item("CORB12"))
)
smprc <- relationships(
  paths(from = "Perceived Response Costs Formative", to = "Perceived Response Costs Reflective")
)
raprc <- estimate_pls(data, mmprc, smprc)
saveRDS(raprc, "sem-covid-19/models/raprc-co-1.RDS")

# Descriptive Norm
mmdn <- constructs(
  composite("Descriptive Norm Formative", multi_items("CODN", 1:2), mode_B),
  composite("Descriptive Norm Reflective", single_item("CODN4"))
)
smdn <- relationships(
  paths(from = "Descriptive Norm Formative", to = "Descriptive Norm Reflective")
)
radn <- estimate_pls(data, mmdn, smdn)
saveRDS(radn, "sem-covid-19/models/radn-co-1.RDS")

# Injunctive Norm
mmin <- constructs(
  composite("Injunctive Norm Formative", multi_items("COIN", 1:2), mode_B),
  composite("Injunctive Norm Reflective", single_item("COIN4"))
)
smin <- relationships(
  paths(from = "Injunctive Norm Formative", to = "Injunctive Norm Reflective")
)
rain <- estimate_pls(data, mmin, smin)
saveRDS(rain, "sem-covid-19/models/rain-co-1.RDS")

# Behavioral Intention
mmbi <- constructs(
  composite("Behavioral Intention Formative", multi_items("COBI", 1:3), mode_B),
  composite("Behavioral Intention Reflective", single_item("COBI4"))
)
smbi <- relationships(
  paths(from = "Behavioral Intention Formative", to = "Behavioral Intention Reflective")
)
rabi <- estimate_pls(data, mmbi, smbi)
saveRDS(rabi, "sem-covid-19/models/rabi-co-1.RDS")
