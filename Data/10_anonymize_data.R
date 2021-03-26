# Generate anonymized data from RAW Data
library(osfr)
library(tidyverse)


raw_path <- here::here("Data", "raw")
open_path <- raw_path <- here::here("Data", "open")

# Anonymization and data prep ----





# Upload final data to OSF ----
osf_id_open <- "9xzdr"
osf_id_protected <- "rvkx4" # This component will remain private



osf_open_node <- osf_retrieve_node(osf_id_open)
osf_protected_node <- osf_retrieve_node(osf_id_protected)







osf_open_node %>% osf_upload()
