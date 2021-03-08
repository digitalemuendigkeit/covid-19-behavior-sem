library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)

# Load data
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))


# Remove Integrity, split up Response Beliefs HOC.
# Behavior: Driving, i.e. CCBI3, CCRB3, CCRB6, CCRB9
# Describe measurement model
mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB3")),
  composite("Perceived Response Efficacy", single_item("CCRB6")),
  composite("Perceived Response Costs", single_item("CCRB9")),
  composite("Benevolence", multi_items("CCDI", 1:3)),
  composite("Competence", multi_items("CCDI", 4:6)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence"), weights = mode_B),
  composite("Behavioral Intention", single_item("CCBI3"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs"), to = c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs")),
  paths(from = c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), to = "Behavioral Intention")
)


# model <- estimate_pls(data, mm, sm)
# saveRDS(model, "SEM Climate Crisis/Models/model-cc-2-c-3-b-1.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-2-c-3-b-1.RDS")
plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-2-c-3-b-1.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-2-c-3-b-1.RDS")
plot(bootmodel)
# bootfsmodel <-  bootstrap_model(model$first_stage_model, nboot = 5000)
# saveRDS(bootfsmodel, "SEM Climate Crisis/Models/model-fs-boot-cc-2-c-3-b-1.RDS")


#Make proxy model for plspredict
proxymm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB3")),
  composite("Perceived Response Efficacy", single_item("CCRB6")),
  composite("Perceived Response Costs", single_item("CCRB9")),
  composite("Distrusting Beliefs", multi_items("CCDI", 1:6), weights = mode_B),
  composite("Behavioral Intention", single_item("CCBI3"))
)

proxymodel <- estimate_pls(data = as.data.frame(data), proxymm, sm)
saveRDS(proxymodel, "SEM Climate Crisis/Models/model-proxy-cc-2-c-3-b-1.RDS")
