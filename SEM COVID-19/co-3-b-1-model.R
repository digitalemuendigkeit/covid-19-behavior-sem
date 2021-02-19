library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)
thm <- seminr_theme_create(construct.compositeA.arrow = "backward", construct.compositeA.use_weights = FALSE, plot.adj = FALSE)
seminr_theme_set(thm)

# Load data and crop to relevant section -c(1:43,48:96,143:145)
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CO") & !paste0("COS", 1:7)) %>% filter(!is.na(COSKN))
data <- as.matrix(datacrop %>% select(!starts_with("COKN")) %>% cbind("COKN" = rowMeans(datacrop %>% select(starts_with("COKN")))))

# Describe measurement model
# Results of co 2 mm eval
# The path from Threat Beliefs to Behavioral Intention is insignificant and the effect size is negligible, therefore the path and the Threat Beliefs construct can be removed.
# Therefore, Perceived Self-Efficacy should be removed as a lower-order construct.
# Therefore, Injunctive Norm should be removed as a lower-order construct and Descriptive Norm substitute the Subjective Norm higher-order construct.
# Response Beliefs split ab

mm <- constructs(
  composite("Perceived Response Efficacy", multi_items("CORB", 4:6), mode_B),
  composite("Perceived Response Costs", multi_items("CORB", 7:9), mode_B),
  composite("Benevolence", multi_items("CODI", 1:3)),
  composite("Competence", multi_items("CODI", 4:6)),
  composite("Integrity", multi_items("CODI", 7:9)),
  higher_composite("Distrusting Beliefs", c("Benevolence", "Competence", "Integrity"), weights = mode_B),
  composite("Knowledge", single_item("COKN")),
  composite("Personal Moral Norm", multi_items("COPN", c(1,3))),
  composite("Descriptive Norm", multi_items("CODN", 1:2), mode_B),
  composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B)
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs", "Knowledge"), to = c("Perceived Response Efficacy", "Perceived Response Costs")),
  paths(from = c("Perceived Response Efficacy", "Perceived Response Costs", "Personal Moral Norm", "Descriptive Norm"), to = "Behavioral Intention")
)


# model <- estimate_pls(data, mm, sm)
# saveRDS(model, "SEM COVID-19/Models/model-co-3-b-1.RDS")
# model <- readRDS("SEM COVID-19/Models/model-co-3-b-1.RDS")
# plot(model)
# bootmodel <- bootstrap_model(model, nboot = 5000)
# saveRDS(bootmodel, "SEM COVID-19/Models/model-boot-co-3-b-1.RDS")
bootmodel <- readRDS("SEM COVID-19/Models/model-boot-co-3-b-1.RDS")
# bootfsmodel <- bootstrap_model(model$first_stage_model, nboot = 5000)
# saveRDS(bootfsmodel, "SEM COVID-19/Models/model-fs-boot-co-3-b-1.RDS")

# Models for estimation of convergent validity for formative constructs
# Perceived Response Efficacy
mmpre <- constructs(
  composite("Perceived Response Efficacy Formative", multi_items("CORB", 4:6), mode_B),
  composite("Perceived Response Efficacy Reflective", single_item("CORB11"))
)
smpre <- relationships(
  paths(from = "Perceived Response Efficacy Formative", to = "Perceived Response Efficacy Reflective")
)
rapre <- estimate_pls(data, mmpre, smpre)
saveRDS(rapre, "SEM COVID-19/Models/rapre-co-3-b-1.RDS")

# Perceived Response Costs
mmprc <- constructs(
  composite("Perceived Response Costs Formative", multi_items("CORB", 7:9), mode_B),
  composite("Perceived Response Costs Reflective", single_item("CORB12"))
)
smprc <- relationships(
  paths(from = "Perceived Response Costs Formative", to = "Perceived Response Costs Reflective")
)
raprc <- estimate_pls(data, mmprc, smprc)
saveRDS(raprc, "SEM COVID-19/Models/raprc-co-3-b-1.RDS")

# Descriptive Norm
mmdn <- constructs(
  composite("Descriptive Norm Formative", multi_items("CODN", 1:2), mode_B),
  composite("Descriptive Norm Reflective", single_item("CODN4"))
)
smdn <- relationships(
  paths(from = "Descriptive Norm Formative", to = "Descriptive Norm Reflective")
)
radn <- estimate_pls(data, mmdn, smdn)
saveRDS(radn, "SEM COVID-19/Models/radn-co-3-b-1.RDS")

# Behavioral Intention
mmbi <- constructs(
  composite("Behavioral Intention Formative", multi_items("COBI", 1:3), mode_B),
  composite("Behavioral Intention Reflective", single_item("COBI4"))
)
smbi <- relationships(
  paths(from = "Behavioral Intention Formative", to = "Behavioral Intention Reflective")
)
rabi <- estimate_pls(data, mmbi, smbi)
saveRDS(rabi, "SEM COVID-19/Models/rabi-co-3-b-1.RDS")

# Make proxymodel
proxymm <- constructs(
  composite("Perceived Response Efficacy", multi_items("CORB", 4:6), mode_B),
  composite("Perceived Response Costs", multi_items("CORB", 7:9), mode_B),
  composite("Distrusting Beliefs", multi_items("CODI", 1:9), weights = mode_B),
  composite("Knowledge", single_item("COKN")),
  composite("Personal Moral Norm", multi_items("COPN", c(1,3))),
  composite("Descriptive Norm", multi_items("CODN", 1:2), weights = mode_B),
  composite("Behavioral Intention", multi_items("COBI", 1:3), mode_B)
)

proxymodel <- estimate_pls(as.data.frame(data), proxymm, sm)
saveRDS(proxymodel, "SEM COVID-19/Models/model-proxy-co-3-b-1.RDS")
