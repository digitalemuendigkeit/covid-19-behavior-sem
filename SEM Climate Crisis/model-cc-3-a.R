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
# Results mm eval 2
# One way to remedy this would be to substitute the problematic formative constructs by the single-item reflective constructs used in the redundancy analysis
mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB10")),
  composite("Perceived Response Efficacy", single_item("CCRB11")),
  composite("Perceived Response Costs", single_item("CCRB12")),
  higher_composite("Response Beliefs", c("Perceived Self-Efficacy", "Perceived Response Efficacy", "Perceived Response Costs"), mode_B),
  composite("Benevolence", multi_items("CCDI", 1:3)),
  composite("Competence", multi_items("CCDI", 4:6)),
  composite("Integrity", multi_items("CCDI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), mode_B),
  composite("Knowledge", single_item("CCKN")),
  composite("Perceived Susceptibility", multi_items("CCTB", 1:3)),
  composite("Perceived Severity", multi_items("CCTB", 4:6)),
  higher_composite("Threat Beliefs", c("Perceived Susceptibility", "Perceived Severity"), mode_B),
  composite("Personal Moral Norm", multi_items("CCPN", 1:3)),
  composite("Descriptive Norm", multi_items("CCDN", 1:2), mode_B),
  composite("Injunctive Norm", multi_items("CCIN", 1:2), mode_B),
  higher_composite("Subjective Norm", c("Descriptive Norm", "Injunctive Norm"), mode_B),
  composite("Behavioral Intention", single_item("CCBI4"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Response Beliefs", "Threat Beliefs")),
  paths(from = c("Response Beliefs", "Threat Beliefs", "Personal Moral Norm", "Subjective Norm"), to = "Behavioral Intention")
)


model <- estimate_pls(data, mm, sm)
#saveRDS(model, "SEM Climate Crisis/Models/model-cc-3-a.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-3-a.RDS")
plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-3-a.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-3-a.RDS")
plot(bootmodel)

# # Models for estimation of convergent validity for formative constructs
# # Perceived Self-Efficacy
# mmpse <- constructs(
#   composite("Perceived Self-Efficacy Formative", multi_items("CCRB", 1:2), mode_B),
#   composite("Perceived Self-Efficacy Reflective", single_item("CCRB10"))
# )
# smpse <- relationships(
#   paths(from = "Perceived Self-Efficacy Formative", to = "Perceived Self-Efficacy Reflective")
# )
# rapse <- estimate_pls(data, mmpse, smpse)
# saveRDS(rapse, "SEM Climate Crisis/Models/rapse-cc-3-a.RDS")
#
# # Perceived Response Efficacy
# mmpre <- constructs(
#   composite("Perceived Response Efficacy Formative", multi_items("CCRB", 4:5), mode_B),
#   composite("Perceived Response Efficacy Reflective", single_item("CCRB11"))
# )
# smpre <- relationships(
#   paths(from = "Perceived Response Efficacy Formative", to = "Perceived Response Efficacy Reflective")
# )
# rapre <- estimate_pls(data, mmpre, smpre)
# saveRDS(rapre, "SEM Climate Crisis/Models/rapre-cc-3-a.RDS")
#
# # Perceived Response Costs
# mmprc <- constructs(
#   composite("Perceived Response Costs Formative", multi_items("CCRB", c(7,9)), mode_B),
#   composite("Perceived Response Costs Reflective", single_item("CCRB12"))
# )
# smprc <- relationships(
#   paths(from = "Perceived Response Costs Formative", to = "Perceived Response Costs Reflective")
# )
# raprc <- estimate_pls(data, mmprc, smprc)
# saveRDS(raprc, "SEM Climate Crisis/Models/raprc-cc-3-a.RDS")

# Descriptive Norm
mmdn <- constructs(
  composite("Descriptive Norm Formative", multi_items("CCDN", 1:2), mode_B),
  composite("Descriptive Norm Reflective", single_item("CCDN4"))
)
smdn <- relationships(
  paths(from = "Descriptive Norm Formative", to = "Descriptive Norm Reflective")
)
radn <- estimate_pls(data, mmdn, smdn)
saveRDS(radn, "SEM Climate Crisis/Models/radn-cc-3-a.RDS")

# Injunctive Norm
mmin <- constructs(
  composite("Injunctive Norm Formative", multi_items("CCIN", 1:2), mode_B),
  composite("Injunctive Norm Reflective", single_item("CCIN4"))
)
smin <- relationships(
  paths(from = "Injunctive Norm Formative", to = "Injunctive Norm Reflective")
)
rain <- estimate_pls(data, mmin, smin)
saveRDS(rain, "SEM Climate Crisis/Models/rain-cc-3-a.RDS")

# # Behavioral Intention
# mmbi <- constructs(
#   composite("Behavioral Intention Formative", multi_items("CCBI", 2:3), mode_B),
#   composite("Behavioral Intention Reflective", single_item("CCBI4"))
# )
# smbi <- relationships(
#   paths(from = "Behavioral Intention Formative", to = "Behavioral Intention Reflective")
# )
# rabi <- estimate_pls(data, mmbi, smbi)
# saveRDS(rabi, "SEM Climate Crisis/Models/rabi-cc-3-a.RDS")
