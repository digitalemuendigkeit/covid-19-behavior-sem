library(tidyverse)
# library(remotes)
# remotes::install_github("sem-in-r/seminr", ref = "model-viz", force = TRUE)
library(seminr)

# Load data
datafull <- read_rds("Data/S1-data-nm.RDS")
datacrop <- datafull %>% select(starts_with("CC") & !paste0("CCS", 1:4)) %>% filter(!is.na(CCSKN))
data <- as.matrix(datacrop %>% select(!starts_with("CCKN")) %>% cbind("CCKN" = rowMeans(datacrop %>% select(starts_with("CCKN")))))


# Injunctive Norm and Perceived Response Efficacy can be removed.
# All other constructs and paths can be retained.
# Describe measurement model
mm <- constructs(
  composite("Perceived Self-Efficacy", single_item("CCRB2")),
  composite("Distrusting Beliefs Benevolence", multi_items("CCDI", 1:3)),
  composite("Personal Moral Norm", multi_items("CCPN",c(1,3))),
  composite("Behavioral Intention", single_item("CCBI2"))
)

sm <- relationships(
  paths(from = c("Distrusting Beliefs Benevolence"), to = c("Perceived Self-Efficacy")),
  paths(from = c("Perceived Self-Efficacy", "Personal Moral Norm"), to = "Behavioral Intention")
)


# model <- estimate_pls(data, mm, sm)
# saveRDS(model, "SEM Climate Crisis/Models/model-cc-2-b-3-a-2.RDS")
model <- readRDS("SEM Climate Crisis/Models/model-cc-2-b-3-a-2.RDS")
plot(model)
bootmodel <- bootstrap_model(model, nboot = 5000)
saveRDS(bootmodel, "SEM Climate Crisis/Models/model-boot-cc-2-b-3-a-2.RDS")
bootmodel <- read_rds("SEM Climate Crisis/Models/model-boot-cc-2-b-3-a-2.RDS")
plot(bootmodel)

#Make proxy model for plspredict
proxymodel <- estimate_pls(data = as.data.frame(data), mm, sm)
saveRDS(proxymodel, "SEM Climate Crisis/Models/model-proxy-cc-2-b-3-a-2.RDS")
