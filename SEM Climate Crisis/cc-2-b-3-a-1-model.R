library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)

# Load data
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))


# Threat Beliefs and Descriptive Norm can be eliminated as constructs.
# Concerning lower-order constructs, Distrusting Beliefs Integrity and Competence have no discernible effect and no significant path on any Response Beliefs lower-order constructs and can be eliminated.
# With that, Distrusting Beliefs Benevolence as a regular construct substitutes the Distrusting Beliefs higher-order construct.
# Additionally, there are two possible constellations:
# Knowledge could be removed and Response Beliefs could remain a higher-order construct.
# Describe measurement model
mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB2")),
  composite("Perceived Response Efficacy", single_item("CCRB5")),
  composite("Perceived Response Costs", single_item("CCRB8")),
  higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), weights = mode_B),
  composite("Distrusting Beliefs Benevolence", multi_items("CCDI", 1:3)),
  composite("Personal Moral Norm", multi_items("CCPN",c(1,3))),
  composite("Behavioral Intention", single_item("CCBI2"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs Benevolence"), to = c("Response Beliefs")),
  paths(from = c("Response Beliefs", "Personal Moral Norm"), to = "Behavioral Intention")
)


# model <- estimate_pls(data, mm, sm)
# saveRDS(model, "SEM Climate Crisis/Models/model-cc-2-b-3-a.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-2-b-3-a.RDS")
plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-2-b-3-a.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-2-b-3-a.RDS")
plot(bootmodel)
# bootfsmodel <-  bootstrap_model(model$first_stage_model, nboot = 5000)
saveRDS(bootfsmodel, "SEM Climate Crisis/Models/model-fs-boot-cc-2-b-3-a.RDS")


#Make proxy model for plspredict
proxymm <- constructs(
  composite("Response Beliefs", multi_items("CCRB", c(2,5,8)), weights = mode_B),
  composite("Distrusting Beliefs Benevolence", multi_items("CCDI", 1:3), weights = mode_B),
  composite("Personal Moral Norm", multi_items("CCPN",c(1,3))),
  composite("Behavioral Intention", single_item("CCBI2"))
)
proxymodel <- estimate_pls(data = as.data.frame(data), proxymm, sm)
saveRDS(proxymodel, "SEM Climate Crisis/Models/model-proxy-cc-2-b-3-a.RDS")
