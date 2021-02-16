library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)

# Load data
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))


# The paths from Personal Moral Norm, and Subjective Norm to Behavioral Intention are insignificant and the effect sizes are negligible.
# Therefore, those paths and the normative constructs can be removed from the model.
# The path from Knowledge on Response Beliefs is also insignificant and shows no effect, and can therefore be removed.
# Behavior: Driving, i.e. CCBI3, CCRB3, CCRB6, CCRB9
# Describe measurement model
mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB3")),
  composite("Perceived Response Efficacy", single_item("CCRB6")),
  composite("Perceived Response Costs", single_item("CCRB9")),
  higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), weights = mode_B),
  composite("Benevolence", multi_items("CCDI", 1:3)),
  composite("Competence", multi_items("CCDI", 4:6)),
  composite("Integrity", multi_items("CCDI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
  composite("Knowledge", single_item("CCKN")),
  composite("Perceived Susceptibility", multi_items("CCTB", 1:3)),
  composite("Perceived Severity", multi_items("CCTB", 4:6)),
  higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), weights = mode_B),
  composite("Behavioral Intention", single_item("CCBI3"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs"), to = c("Response Beliefs", "Threat Beliefs")),
  paths(from = c("Knowledge"), to = c("Threat Beliefs")),
  paths(from = c("Response Beliefs", "Threat Beliefs"), to = "Behavioral Intention")
)


# model <- estimate_pls(data, mm, sm)
# saveRDS(model, "SEM Climate Crisis/Models/model-cc-2-c-2.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-2-c-2.RDS")
plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-2-c-2.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-2-c-2.RDS")
plot(bootmodel)
# bootfsmodel <-  bootstrap_model(model$first_stage_model, nboot = 5000)
# saveRDS(bootfsmodel, "SEM Climate Crisis/Models/model-fs-boot-cc-2-c-2.RDS")


#Make proxy model for plspredict
proxymm <- constructs(
  composite("Response Beliefs", multi_items("CCRB", c(3,6,9)), weights = mode_B),
  composite("Distrusting Beliefs", multi_items("CCDI", 1:9), weights = mode_B),
  composite("Knowledge", single_item("CCKN")),
  composite("Threat Beliefs", multi_items("CCTB", 1:6), weights = mode_B),
  composite("Behavioral Intention", single_item("CCBI3"))
)

proxymodel <- estimate_pls(data = as.data.frame(data), proxymm, sm)
saveRDS(proxymodel, "SEM Climate Crisis/Models/model-proxy-cc-2-c-2.RDS")
