library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)

# Load data
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))


# Knowledge, Threat Beliefs and Personal Moral Norm can be removed from the model.
# Concerning lower-order constructs, Perceived Self-Efficacy has no discernible effect on Behavioral Intention, and the path coefficient is insignificant.
# Therefore, Perceived Self-Efficacy can be removed.
# Further, the Distrusting Beliefs lower order-constructs Integrity and Competence have no discernible effect on and no significant path to any of the remaining lower-order construct.
# Therefore, the Distrusting Beliefs higher-order construct can be substituted by a regular Distrusting Beliefs Benevolence construct.

# Behavior: General, i.e. CCBI4, CCRB10, CCRB11, CCRB12
# Describe measurement model
mm <- constructs(
  composite("Perceived Response Efficacy", single_item("CCRB11")),
  composite("Perceived Response Costs", single_item("CCRB12")),
  higher_composite("Response Beliefs", c("Perceived Response Efficacy", "Perceived Response Costs"), weights = mode_B),
  composite("Distrusting Beliefs Benevolence", multi_items("CCDI", 1:3)),
  composite("Descriptive Norm", multi_items("CCDN", 1:2), mode_B),
  composite("Injunctive Norm", multi_items("CCIN", 1:2), mode_B),
  higher_composite("Subjective Norm", c("Descriptive Norm", "Injunctive Norm"), weights = mode_B),
  composite("Behavioral Intention", single_item("CCBI1"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs Benevolence"), to = c("Response Beliefs")),
  paths(from = c("Response Beliefs", "Subjective Norm"), to = "Behavioral Intention")
)


model <- estimate_pls(data, mm, sm)
saveRDS(model, "SEM Climate Crisis/Models/model-cc-2-d-2.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-2-d-2.RDS")
plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-2-d-2.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-2-d-2.RDS")
plot(bootmodel)
# bootfsmodel <-  bootstrap_model(model$first_stage_model, nboot = 5000)
# saveRDS(bootfsmodel, "SEM Climate Crisis/Models/model-fs-boot-cc-2-d-2.RDS")

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
saveRDS(radn, "SEM Climate Crisis/Models/radn-cc-2-d-2.RDS")

# Injunctive Norm
mmin <- constructs(
  composite("Injunctive Norm Formative", multi_items("CCIN", 1:2), mode_B),
  composite("Injunctive Norm Reflective", single_item("CCIN4"))
)
smin <- relationships(
  paths(from = "Injunctive Norm Formative", to = "Injunctive Norm Reflective")
)
rain <- estimate_pls(data, mmin, smin)
saveRDS(rain, "SEM Climate Crisis/Models/rain-cc-2-d-2.RDS")

#Make proxy model for plspredict
proxymm <- constructs(
  composite("Response Beliefs", multi_items("CCRB", 11:12), weights = mode_B),
  composite("Distrusting Beliefs Benevolence", multi_items("CCDI", 1:3), weights = mode_B),
  composite("Subjective Norm", c("CCDN1", "CCDN2", "CCIN1", "CCIN2"), weights = mode_B),
  composite("Behavioral Intention", single_item("CCBI4"))
)

# proxymodel <- estimate_pls(data = as.data.frame(data), proxymm, sm)
# saveRDS(proxymodel, "SEM Climate Crisis/Models/model-proxy-cc-2-d-2.RDS")
