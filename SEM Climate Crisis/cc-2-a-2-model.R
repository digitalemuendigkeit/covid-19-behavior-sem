library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)

# Load data
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))


# In the reflective measurement model, CCPN2 should be tentatively deleted to rectify internal consistency.
# The paths from Threat Beliefs, Personal Moral Norm, and Subjective Norm to Behavioral Intention are insignificant and the effect sizes are negligible.
# Therefore, those paths can be removed from the model.
# Behavior: Diet, i.e. CCBI1, CCRB1, CCRB4, CCRB7
# Describe measurement model
mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB1")),
  composite("Perceived Response Efficacy", single_item("CCRB4")),
  higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy"), weights = mode_B),
  composite("Benevolence", multi_items("CCDI", 1:3)),
  composite("Competence", multi_items("CCDI", 4:6)),
  composite("Integrity", multi_items("CCDI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
  composite("Knowledge", single_item("CCKN")),
  composite("Behavioral Intention", single_item("CCBI1"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs")),
  paths(from = c("Response Beliefs"), to = "Behavioral Intention")
)


model <- estimate_pls(data, mm, sm)
saveRDS(model, "SEM Climate Crisis/Models/model-cc-2-a-2.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-2-a-2.RDS")
plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-2-a-2.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-2-a-2.RDS")
plot(bootmodel)
# bootfsmodel <-  bootstrap_model(model$first_stage_model, nboot = 5000)
# saveRDS(bootfsmodel, "SEM Climate Crisis/Models/model-fs-boot-cc-2-a-2.RDS")

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
saveRDS(radn, "SEM Climate Crisis/Models/radn-cc-2-a-2.RDS")

# Injunctive Norm
mmin <- constructs(
  composite("Injunctive Norm Formative", multi_items("CCIN", 1:2), mode_B),
  composite("Injunctive Norm Reflective", single_item("CCIN4"))
)
smin <- relationships(
  paths(from = "Injunctive Norm Formative", to = "Injunctive Norm Reflective")
)
rain <- estimate_pls(data, mmin, smin)
saveRDS(rain, "SEM Climate Crisis/Models/rain-cc-2-a-2.RDS")

#Make proxy model for plspredict
proxymm <- constructs(
  composite("Response Beliefs", multi_items("CCRB", c(1,4)), weights = mode_B),
  composite("Distrusting Beliefs", multi_items("CCDI", 1:9), weights = mode_B),
  composite("Knowledge", single_item("CCKN")),
  composite("Behavioral Intention", single_item("CCBI1"))
)
proxymodel <- estimate_pls(data = as.data.frame(data), proxymm, sm)
saveRDS(proxymodel, "SEM Climate Crisis/Models/model-proxy-cc-2-a-2.RDS")
