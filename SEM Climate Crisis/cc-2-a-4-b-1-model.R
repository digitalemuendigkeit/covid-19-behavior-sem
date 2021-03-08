library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)

# Load data
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))


# DB > Distrusting Beliefs Integrity
# Because of the lack of effect associated with many of the lower-order constructs, splitting up the Response Beliefs constructs and reducing the model from there might lead to better predictive power.
# Version with full hoc
# Describe measurement model
mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB1")),
  composite("Perceived Response Efficacy", single_item("CCRB4")),
  composite("Distrusting Beliefs Integrity", multi_items("CCDI", 7:9)),
  composite("Knowledge", single_item("CCKN")),
  composite("Behavioral Intention", single_item("CCBI1"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs Integrity", "Knowledge"), to = c("Perceived Self-Efficacy", "Perceived Response Efficacy")),
  paths(from = c("Perceived Self-Efficacy", "Perceived Response Efficacy"), to = "Behavioral Intention")
)


# model <- estimate_pls(data, mm, sm)
# saveRDS(model, "SEM Climate Crisis/Models/model-cc-2-a-4-b-1.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-2-a-4-b-1.RDS")
plot(model)
bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-2-a-4-b-1.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-2-a-4-b-1.RDS")
#saveRDS(bootmodel, "SEM Climate Crisis/Models/model-fs-boot-cc-2-a-4-b-1.RDS")
plot(bootmodel)

proxymodel <- estimate_pls(as.data.frame(data), mm, sm)
saveRDS(proxymodel, "SEM Climate Crisis/Models/model-proxy-cc-2-a-4-b-1.RDS")
