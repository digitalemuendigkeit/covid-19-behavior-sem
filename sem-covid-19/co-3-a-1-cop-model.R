library(tidyverse)
library(seminr)

# Data Loading ----
data <- read_rds(here::here("data",
                            "open",
                            "model-data.RDS")) %>%
  mutate(COKN = rowMeans(across(starts_with("COKN"))))

# Load copula
RB_star <- readRDS(here::here("sem-covid-19",
                              "models",
                              "copula-rb-model-co-3-a-1.RDS"))
# append to data
data <- data %>%
  cbind(RB_star)

# Model Description ----

# Describe measurement model
# Inclusion of Response Beliefs copula

mm <- constructs(
  composite("Perceived Response Efficacy", multi_items("CORB", 4:6), mode_B),
  composite("Perceived Response Costs", multi_items("CORB", 7:9), mode_B),
  higher_composite("Response Beliefs", c("Perceived Response Efficacy", "Perceived Response Costs"), weights =  mode_B),
  composite("Benevolence", multi_items("CODI", 1:3)),
  composite("Competence", multi_items("CODI", 4:6)),
  composite("Integrity", multi_items("CODI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
  composite("Knowledge", single_item("COKN")),
  composite("Personal Moral Norm", multi_items("COPN", c(1,3))),
  composite("Descriptive Norm", multi_items("CODN", 1:2), mode_B),
  composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B),
  composite("Copula Response Beliefs", single_item("RB_star"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs")),
  paths(from = c("Response Beliefs", "Personal Moral Norm", "Descriptive Norm", "Copula Response Beliefs"), to = "Behavioral Intention")
)

# Model Estimation ----

model <- estimate_pls(data, mm, sm)
saveRDS(model, "sem-covid-19/models/model-co-3-a-1-cop.RDS")
bootmodel <- bootstrap_model(model, nboot = 5000)
saveRDS(bootmodel, "sem-covid-19/models/model-boot-co-3-a-1-cop.RDS")
bootfsmodel <- bootstrap_model(model$first_stage_model, nboot = 5000)
saveRDS(bootfsmodel, "sem-covid-19/models/model-fs-boot-co-3-a-1-cop.RDS")

# Proxy Model ----

# Make proxy model for PLSpredict
proxymm <- constructs(
  composite("Response Beliefs", multi_items("CORB", c(4:9)), weights = mode_B),
  composite("Distrusting Beliefs", multi_items("CODI", 1:9), weights = mode_B),
  composite("Knowledge", single_item("COKN")),
  composite("Personal Moral Norm", multi_items("COPN", c(1,3))),
  composite("Descriptive Norm", multi_items("CODN", 1:2), weights = mode_B),
  composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B)
)

proxymodel <- estimate_pls(as.data.frame(data), proxymm, sm)
saveRDS(proxymodel, "sem-covid-19/models/model-proxy-co-3-a-1-cop.RDS")
