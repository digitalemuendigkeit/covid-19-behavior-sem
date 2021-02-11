library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)
thm <- seminr_theme_create(construct.compositeA.arrow = "backward", construct.compositeA.use_weights = FALSE, plot.adj = FALSE)
seminr_theme_set(thm)

# Load data
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))

# Describe measurement model
# Results sm 3b eval
#As indicated by the summary table, the paths from Knowledge to Response Beliefs as well as the paths from Personal Moral Norm and Subjective Norm to Behavioral Intention are insignificant and can be omitted.
#Further, it would be interesting to see what happens if the Response Beliefs construct is split.

# mm <- constructs(
#   composite("Perceived Self-Efficacy", single_item("CCRB3")),
#   composite("Perceived Response Efficacy", single_item("CCRB6")),
#   composite("Perceived Response Costs", single_item("CCRB9")),
#   higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), mode_B),
#   composite("Benevolence", multi_items("CCDI", 1:3)),
#   composite("Competence", multi_items("CCDI", 4:6)),
#   composite("Integrity", multi_items("CCDI", 7:9)),
#   higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), mode_B),
#   composite("Knowledge", single_item("CCKN")),
#   composite("Perceived Susceptibility", multi_items("CCTB", 1:3)),
#   composite("Perceived Severity", multi_items("CCTB", 4:6)),
#   higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), mode_B),
#   composite("Behavioral Intention", single_item("CCBI3"))
# )
#
# sm <- relationships(
#   paths(from = c("Distrusting Beliefs"), to = c("Response Beliefs", "Threat Beliefs")),
#   paths(from = "Knowledge", to = "Threat Beliefs"),
#   paths(from = c("Response Beliefs", "Threat Beliefs"), to = "Behavioral Intention"))

mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB3")),
  composite("Perceived Response Efficacy", single_item("CCRB6")),
  composite("Perceived Response Costs", single_item("CCRB9")),
  composite("Benevolence", multi_items("CCDI", 1:3)),
  composite("Competence", multi_items("CCDI", 4:6)),
  composite("Integrity", multi_items("CCDI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), mode_B),
  composite("Knowledge", single_item("CCKN")),
  composite("Perceived Susceptibility", multi_items("CCTB", 1:3)),
  composite("Perceived Severity", multi_items("CCTB", 4:6)),
  higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), mode_B),
  composite("Behavioral Intention", single_item("CCBI3"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs"), to = c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs", "Threat Beliefs")),
  paths(from = "Knowledge", to = "Threat Beliefs"),
  paths(from = c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs", "Threat Beliefs"), to = "Behavioral Intention"))


# model <- estimate_pls(data, mm, sm)
# saveRDS(model, "SEM Climate Crisis/Models/model-cc-3b-2b.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-3b-2b.RDS")
plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-3b-2b.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-3b-2b.RDS")
plot(bootmodel)
# bootfsmodel <- bootstrap_model(model$first_stage_model, nboot = 5000)
# saveRDS(bootfsmodel, "SEM Climate Crisis/Models/model-fs-boot-cc-3b-2b.RDS")
