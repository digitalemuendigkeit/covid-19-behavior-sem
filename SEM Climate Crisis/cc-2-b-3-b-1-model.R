library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)

# Load data
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))


# Threat Beliefs and Subjective Norm can be eliminated as constructs, but Injunctive Norm should be retained.
# Concerning lower-order constructs, Distrusting Beliefs Integrity and Competence have no discernible effect and no significant path on any Response Beliefs lower-order constructs and can be eliminated.
# With that, Distrusting Beliefs Benevolence as a regular construct substitutes the Distrusting Beliefs higher-order construct.
# Additionally, there are two possible constellations:
# Or Knowledge could be retained and Response Beliefs split up in different constructs.
# Version Response Beliefs as LOC
# Describe measurement model
mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB2")),
  composite("Perceived Response Efficacy", single_item("CCRB5")),
  composite("Distrusting Beliefs Benevolence", multi_items("CCDI", 1:3)),
  composite("Knowledge", single_item("CCKN")),
  composite("Injunctive Norm", multi_items("CCIN", 1:2), mode_B),
  composite("Personal Moral Norm", multi_items("CCPN",c(1,3))),
  composite("Behavioral Intention", single_item("CCBI2"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs Benevolence", "Knowledge"), to = c("Perceived Self-Efficacy", "Perceived Response Efficacy")),
  paths(from = c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Personal Moral Norm", "Injunctive Norm"), to = "Behavioral Intention")
)


# model <- estimate_pls(data, mm, sm)
# saveRDS(model, "SEM Climate Crisis/Models/model-cc-2-b-3-b-1.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-2-b-3-b-1.RDS")
plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-2-b-3-b-1.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-2-b-3-b-1.RDS")
plot(bootmodel)


# Models for estimation of convergent validity for formative constructs
# Injunctive Norm
mmin <- constructs(
  composite("Injunctive Norm Formative", multi_items("CCIN", 1:2), mode_B),
  composite("Injunctive Norm Reflective", single_item("CCIN4"))
)
smin <- relationships(
  paths(from = "Injunctive Norm Formative", to = "Injunctive Norm Reflective")
)
rain <- estimate_pls(data, mmin, smin)
saveRDS(rain, "SEM Climate Crisis/Models/rain-cc-2-b-3-b-1.RDS")

#Make proxy model for plspredict
proxymodel <- estimate_pls(data = as.data.frame(data), mm, sm)
saveRDS(proxymodel, "SEM Climate Crisis/Models/model-proxy-cc-2-b-3-b-1.RDS")
