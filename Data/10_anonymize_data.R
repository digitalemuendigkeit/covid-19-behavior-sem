# Generate anonymized data from RAW Data
library(osfr)
library(tidyverse)


raw_path <- here::here("data", "raw")
anonymized_path <- here::here("data", "anonymized")

# Anonymization and data prep ----
# Import, cleaning, anonymization
source(here::here("data", "dataprep", "10_Data-Import-and-Examination.R"))
# Import, cleaning, anonymization
source(here::here("data", "dataprep", "15_Data_additional-analyses_coding.R"))
# PLS prep: Remove variables exhibiting extreme skewness or kurtosis
source(here::here("data","dataprep", "20_Data-Distribution.R"))
# PLS prep: Imputate missing data or remove variables
source(here::here("data","dataprep", "30_Data-Missing.R"))


# Upload final data to OSF ----
osf_id_anonymized <- "9xzdr" # Access to this component can be requested
osf_id_protected <- "rvkx4" # This component will remain private

# get osf nodes
osf_anonymized_node <- osf_retrieve_node(osf_id_anonymized)
osf_protected_node <- osf_retrieve_node(osf_id_protected)


# start upload to osf
# change to RDS and CSV once ready
files_to_upload <- dir(anonymized_path, pattern = "*.csv", recursive = T)

osf_anonymized_node %>%
  osf_upload(path = paste0("data/anonymized/", files_to_upload),
             conflicts = "overwrite",
             recurse = T, progress = T)


files_to_upload <- dir(anonymized_path, pattern = "*.RDS", recursive = T)

osf_anonymized_node %>%
  osf_upload(path = paste0("data/anonymized/", files_to_upload),
             conflicts = "overwrite",
             recurse = T, progress = T)

