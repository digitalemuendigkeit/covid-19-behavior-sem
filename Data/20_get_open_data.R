# This file will download the open data from the OSF repository.
#
# WARNING: DO NOT RUN IF YOU HAVE UPDATED DATA!


# Generate anonymized data from RAW Data
library(osfr)
library(tidyverse)

open_path <- raw_path <- here::here("Data", "open")

osf_id <- "9xzdr" # This component is open!

osf_node <- osf_retrieve_node(osf_id)

osf_node %>%
  osf_ls_files() %>%
  osf_download(here::here("Data", "open"),
               recurse = TRUE,
               conflicts = "overwrite",
               progress = TRUE)
