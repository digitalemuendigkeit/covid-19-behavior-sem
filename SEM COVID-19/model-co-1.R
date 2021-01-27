library(tidyverse)
remotes::install_github("digitalemuendigkeit/seminr", ref = "model-viz", force = TRUE)
library(seminr)

# Load data and crop to relevant section -c(1:43,48:96,143:145)
databoth <- read_csv("Data/S1-Data-preliminary.csv")[,-c(1:43,48:96,143:145)] %>% as.data.frame()
data <- databoth[!is.na(databoth$COSKN),] %>% as.matrix()
data
# # mean of knowledge
# datakn <- data %>% transmute(COKN = mean(c(COKN1, COKN2, COKN2_1, COKN3, COKN4, COKN5, COKN6)))

# Describe measurement model
# COKN1 is placeholder
mm <- constructs(
  composite("Perceived Self-Efficacy", multi_items("CORB", 1:3), mode_B),
  composite("Perceived Response Efficacy", multi_items("CORB", 4:6), mode_B),
  composite("Perceived Response Costs", multi_items("CORB", 7:9), mode_B),
  higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), mode_B),
  composite("Benevolence", multi_items("CODI", 1:3)),
  composite("Competence", multi_items("CODI", 4:6)),
  composite("Integrity", multi_items("CODI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), mode_B),
  composite("Knowledge", single_item("COKN1")),
  composite("Perceived Susceptibility", multi_items("COTB", 1:3)),
  composite("Perceived Severity", multi_items("COTB", 4:6)),
  higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), mode_B),
  composite("Personal Moral Norm", multi_items("COPN", 1:3)),
  composite("Descriptive Norm", multi_items("CODN", 1:3), mode_B),
  composite("Injunctive Norm", multi_items("COIN", 1:3), mode_B),
  higher_composite("Subjective Norm", c("Descriptive Norm", "Injunctive Norm"), mode_B),
  composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B)
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
  paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
)


model <- estimate_pls(data, mm, sm)
saveRDS(model, "SEM COVID-19/model-co-1.RDS")
model <- readRDS("SEM COVID-19/model-co-1.RDS")
summo <- summary(model)
sumfs <- summary(model$first_stage_model)
bootmodel <- bootstrap_model(model, nboot = 5000)
saveRDS(bootmodel, "SEM COVID-19/model-boot-co-1.RDS")
summary(bootmodel)

