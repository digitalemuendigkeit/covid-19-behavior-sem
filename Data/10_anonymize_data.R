# Generate anonymized data from RAW Data
library(osfr)
library(tidyverse)


raw_path <- here::here("Data", "raw")
open_path <- raw_path <- here::here("Data", "open")

# Anonymization and data prep ----





# Upload final data to OSF ----
osf_id_open <- "9xzdr"
osf_id_protected <- "rvkx4" # This component will remain private

# get osf nodes
osf_open_node <- osf_retrieve_node(osf_id_open)
osf_protected_node <- osf_retrieve_node(osf_id_protected)


# start upload to osf

files_to_upload <- dir(open_path, pattern = "*.txt", recursive = T)

osf_open_node %>%
  osf_upload(path = paste0("Data/open/", files_to_upload),
             conflicts = "overwrite",
             recurse = T, progress = T)
