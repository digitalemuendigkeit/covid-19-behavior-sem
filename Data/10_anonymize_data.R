# Generate anonymized data from RAW Data
library(osfr)
library(tidyverse)


raw_path <- here::here("Data", "raw")
open_path <- raw_path <- here::here("Data", "open")

# Anonymization and data prep ----
# Import, cleaning, anonymization
source(here::here("dataprep/", "10_Data-Import-and-Examination.R"))
# PLS prep: Remove variables exhibiting extreme skewness or kurtosis
source(here::here("dataprep/", "20_Data-Distribution.R"))
# PLS prep: Imputate missing data or remove variables
source(here::here("dataprep/", "30_Data-Missing.R"))


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
