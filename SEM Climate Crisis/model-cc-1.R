library(tidyverse)
library(seminr)

# Load data and crop to relevant section -c(1:47,97:145)
databoth <- read_csv("Data/S1-Data-preliminary.csv")[,-c(1:47,97:145)] %>% as.data.frame()
data <- databoth[!is.na(databoth$CCSKN),] %>% as.matrix()
data
# # mean of knowledge
# datakn <- data %>% transmute(CCKN = mean(c(CCKN1, CCKN2, CCKN2_1, CCKN3, CCKN4, CCKN5, CCKN6)))

# Describe measurement model
# CCKN1 is placeholder
mm <- constructs(
  composite("Perceived Self-Efficacy", multi_items("CCRB", 1:3), mode_B),
  composite("Perceived Response Efficacy", multi_items("CCRB", 4:6), mode_B),
  composite("Perceived Response Costs", multi_items("CCRB", 7:9), mode_B),
  higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), mode_B),
  composite("Benevolence", multi_items("CCDI", 1:3)),
  composite("Competence", multi_items("CCDI", 4:6)),
  composite("Integrity", multi_items("CCDI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), mode_B),
  composite("Knowledge", single_item("CCKN1")),
  composite("Perceived Susceptibility", multi_items("CCTB", 1:3)),
  composite("Perceived Severity", multi_items("CCTB", 4:6)),
  higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), mode_B),
  composite("Personal Moral Norm", multi_items("CCPN", 1:3)),
  composite("Descriptive Norm", multi_items("CCDN", 1:3), mode_B),
  composite("Injunctive Norm", multi_items("CCIN", 1:3), mode_B),
  higher_composite("Subjective Norm", c("Descriptive Norm", "Injunctive Norm"), mode_B),
  composite("Behavioral Intention", multi_items("CCBI", 1:3), mode_B)
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
  paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
)


model <- estimate_pls(data, mm, sm)
saveRDS(model, "SEM Climate Crisis/model-cc-1.RDS")
model <- readRDS("SEM Climate Crisis/model-cc-1.RDS")
summo <- summary(model)
sumfs <- summary(model$first_stage_model)
bootmodel <- bootstrap_model(model, nboot = 5000)
saveRDS(bootmodel, "SEM Climate Crisis/model-boot-cc-1.RDS")
summary(bootmodel)
