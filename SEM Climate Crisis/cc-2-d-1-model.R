library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)

# Load data
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))


# In the reflective measurement model, CCPN2 should be tentatively deleted to rectify internal consistency.
# If this leads to problems with discriminant validity reliability, it should be taken back in.
# Other than that, the reflective measurement model can be retained as-is.
# Because there are incorrigible problems with convergent validity for the Response Belief lower-order constructs and Behavioral Intention, four distinct behavior-specific models should be estimated next.
# The formative higher-order constructs can be retained as-is.
# Behavior: General, i.e. CCBI4, CCRB10, CCRB11, CCRB12
# Describe measurement model
mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB10")),
  composite("Perceived Response Efficacy", single_item("CCRB11")),
  composite("Perceived Response Costs", single_item("CCRB12")),
  higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), weights = mode_B),
  composite("Benevolence", multi_items("CCDI", 1:3)),
  composite("Competence", multi_items("CCDI", 4:6)),
  composite("Integrity", multi_items("CCDI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
  composite("Knowledge", single_item("CCKN")),
  composite("Perceived Susceptibility", multi_items("CCTB", 1:3)),
  composite("Perceived Severity", multi_items("CCTB", 4:6)),
  higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), weights = mode_B),
  composite("Personal Moral Norm", multi_items("CCPN",c(1,3))),
  composite("Descriptive Norm", multi_items("CCDN", 1:2), mode_B),
  composite("Injunctive Norm", multi_items("CCIN", 1:2), mode_B),
  higher_composite("Subjective Norm", c("Descriptive Norm", "Injunctive Norm"), weights = mode_B),
  composite("Behavioral Intention", single_item("CCBI1"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
  paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
)


# model <- estimate_pls(data, mm, sm)
# saveRDS(model, "SEM Climate Crisis/Models/model-cc-2-d-1.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-2-d-1.RDS")
plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-2-d-1.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-2-d-1.RDS")
plot(bootmodel)
bootfsmodel <-  bootstrap_model(model$first_stage_model, nboot = 5000)
saveRDS(bootfsmodel, "SEM Climate Crisis/Models/model-fs-boot-cc-2-d-1.RDS")

# Models for estimation of convergent validity for formative constructs
# Descriptive Norm
mmdn <- constructs(
  composite("Descriptive Norm Formative", multi_items("CCDN", 1:2), mode_B),
  composite("Descriptive Norm Reflective", single_item("CCDN4"))
)
smdn <- relationships(
  paths(from = "Descriptive Norm Formative", to = "Descriptive Norm Reflective")
)
radn <- estimate_pls(data, mmdn, smdn)
saveRDS(radn, "SEM Climate Crisis/Models/radn-cc-2-d-1.RDS")

# Injunctive Norm
mmin <- constructs(
  composite("Injunctive Norm Formative", multi_items("CCIN", 1:2), mode_B),
  composite("Injunctive Norm Reflective", single_item("CCIN4"))
)
smin <- relationships(
  paths(from = "Injunctive Norm Formative", to = "Injunctive Norm Reflective")
)
rain <- estimate_pls(data, mmin, smin)
saveRDS(rain, "SEM Climate Crisis/Models/rain-cc-2-d-1.RDS")

#Make proxy model for plspredict
proxymm <- constructs(
  composite("Response Beliefs", multi_items("CCRB", 10:12), weights = mode_B),
  composite("Distrusting Beliefs", multi_items("CCDI", 1:9), weights = mode_B),
  composite("Knowledge", single_item("CCKN")),
  composite("Threat Beliefs", multi_items("CCTB", 1:6), weights = mode_B),
  composite("Personal Moral Norm", multi_items("CCPN",c(1,3))),
  composite("Subjective Norm", c("CCDN1", "CCDN2", "CCIN1", "CCIN2"), weights = mode_B),
  composite("Behavioral Intention", single_item("CCBI4"))
)
proxysm <- relationships(
  paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
  paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
)
proxymodel <- estimate_pls(data = as.data.frame(data), proxymm, proxysm)
saveRDS(proxymodel, "SEM Climate Crisis/Models/model-proxy-cc-2-d-1.RDS")
