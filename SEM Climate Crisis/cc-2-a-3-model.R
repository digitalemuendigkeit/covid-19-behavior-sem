library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)

# Load data
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))


# In the reflective measurement model, CCPN2 should be tentatively deleted to rectify internal consistency.
# Split up higher-order constructs to possibly improve predictive power from there
# Behavior: Diet, i.e. CCBI1, CCRB1, CCRB4, CCRB7
# Describe measurement model
mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB1")),
  composite("Perceived Response Efficacy", single_item("CCRB4")),
  composite("Distrusting Beliefs Benevolence", multi_items("CCDI", 1:3)),
  composite("Distrusting Beliefs Competence", multi_items("CCDI", 4:6)),
  composite("Distrusting Beliefs Integrity", multi_items("CCDI", 7:9)),
  composite("Knowledge", single_item("CCKN")),
  composite("Behavioral Intention", single_item("CCBI1"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs Benevolence", "Distrusting Beliefs Competence", "Distrusting Beliefs Integrity", "Knowledge"),
        to = c("Perceived Self-Efficacy", "Perceived Response Efficacy")),
  paths(from = c("Perceived Self-Efficacy", "Perceived Response Efficacy"), to = "Behavioral Intention")
)


# model <- estimate_pls(data = as.data.frame(data), mm, sm)
# saveRDS(model, "SEM Climate Crisis/Models/model-cc-2-a-3.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-2-a-3.RDS")
plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-2-a-3.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-2-a-3.RDS")
plot(bootmodel)
