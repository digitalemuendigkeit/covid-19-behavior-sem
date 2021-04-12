# Generate open model data from anonymized Data
library(osfr)
library(tidyverse)

open_path <- here::here("data", "open")

# Crop ----
anonymized_data <- read_rds(here::here("data",
                                       "anonymized",
                                       "S1-Data-nm.RDS"))
open_model_data <- anonymized_data %>%
  filter(
    !is.na(COSKN)) %>% # other condition
  select(
    starts_with("CO") &
      !starts_with("COS")
  )

write_rds(open_model_data,
          here::here("data",
                     "open",
                     "model-data.RDS"))
write_csv(open_model_data,
          here::here("data",
                     "open",
                     "model-data.csv"))

# Upload final data to OSF ----
osf_id_open <- "bkwau"

# get osf nodes
osf_open_node <- osf_retrieve_node(osf_id_open)


# start upload to osf
# change to RDS and CSV once ready
files_to_upload <- dir(open_path, pattern = "*.csv", recursive = T)

osf_open_node %>%
  osf_upload(path = paste0("data/open/", files_to_upload),
             conflicts = "overwrite",
             recurse = T, progress = T)


files_to_upload <- dir(open_path, pattern = "*.RDS", recursive = T)

osf_open_node %>%
  osf_upload(path = paste0("data/open/", files_to_upload),
             conflicts = "overwrite",
             recurse = T, progress = T)

