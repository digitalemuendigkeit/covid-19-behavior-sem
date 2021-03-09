library(tidyverse)
datafull <- read_rds("Data/data-qualcl.RDS")
datachoicetext <- read_csv("Data/S1-data-choicetext.csv") %>%
  tibble() %>%
  select(c("ResponseId", "SD8"))
colnames(datachoicetext) <- c("ResponseId", "SD8.text")
datafulllk <- datafull %>%
  left_join(datachoicetext) %>%
  mutate(SD8.text = SD8.text)
# As I accidentally used the wrong administrative district list, there is a disconnect between the RKI data and my data
# let's harmonize it
# for ease
`%notin%` <- Negate(`%in%`)
# List I used
kreislist <- readRDS("Additional Analysis/kreislist.RDS")[1:401,]
# RKI data
incidence <- read_csv2("Additional Analysis/20210112-Fallzahlen.csv",
                       skip = 4,
                       col_names = c("Landkreis", "LKNR", "Count210112", "Incidence210112")
                       ) %>%
  tibble() %>% left_join(
  tibble(read_csv2("Additional Analysis/20210201-Fallzahlen.csv",
                   skip = 4,
                   col_names = c("Landkreis", "LKNR", "Count210201", "Incidence210201"))
         ),
  by = c("Landkreis", "LKNR")
  )
incidence$Landkreis
rkikreise <- substring(incidence$Landkreis, 4)
#Find the difference
# Both
myinrki <- kreislist$name[kreislist$name %in% rkikreise]
rkiinmy <- incidence$Landkreis[rkikreise %in% kreislist$name]
# difference of three
# because of doubles?
n_occur_rki <- data.frame(table(rkikreise))
suspis <- n_occur_rki[n_occur_rki$Freq > 1,1]
n_occur_my <- data.frame(table(kreislist$name))
n_occur_my[n_occur_my$Freq >1,1]
perps <- suspis[n_occur_rki[n_occur_rki$Freq > 1,1] %notin% n_occur_my[n_occur_my$Freq >1,1]]
# clean them
myinrkic <- myinrki[!myinrki %in% perps]
rkiinmyc <- incidence$Landkreis[rkikreise %in% myinrkic]
# Only in my list
onlymy <- data.frame(MyName = kreislist$name[kreislist$name %notin% rkikreise | kreislist$name %in% perps],
                     MyType = kreislist$type[kreislist$name %notin% rkikreise | kreislist$name %in% perps],
                     MyIndex = which(kreislist$name %notin% rkikreise | kreislist$name %in% perps))
# Only in the rki list
onlyrki <- data.frame(RKIName = incidence$Landkreis[rkikreise %notin% kreislist$name | rkikreise %in% perps],
                      RKIIndex = which(rkikreise %notin% kreislist$name | rkikreise %in% perps))
tester <- onlyrki %>% cbind(onlymy %>% rbind(data.frame(MyName = rep(NA, 11), MyType = rep(NA,11), MyIndex = rep(NA,11))))
harmonizer <- onlymy %>%
  cbind(RKIIndex = c(6,NA,58,50,113,116,142,183,196,316,197,206,212,239,250,269,268,271,270,146,352,315,326,330,1))
harmonized <- onlyrki %>% full_join(harmonizer)

Cypher <- data.frame(MyName = myinrkic, RKIName = rkiinmyc) %>%
  union(harmonized[,c(1,3)])

kreisincidence <- kreislist %>%
  transmute(MyName = name,
            SD8.text = surveyname) %>%
  left_join(Cypher %>% rename(Landkreis = RKIName)) %>%
  left_join(incidence)
#sum up and take means for berlin
kreisincidence[kreisincidence$MyName == "Berlin",5] <- sum(incidence[grepl("Berlin", incidence$Landkreis),3])
kreisincidence[kreisincidence$MyName == "Berlin",6] <- colMeans(incidence[grepl("Berlin", incidence$Landkreis),4])
kreisincidence[kreisincidence$MyName == "Berlin",7] <- sum(incidence[grepl("Berlin", incidence$Landkreis),5])
kreisincidence[kreisincidence$MyName == "Berlin",8] <- colMeans(incidence[grepl("Berlin", incidence$Landkreis),6])

saveRDS(kreisincidence, "Additional Analysis/kreisincidence.RDS")
datafullwin <- datafull %>%
  left_join(datachoicetext) %>%
  left_join(kreisincidence[,-c(1,3:4)], by = "SD8.text")
# for some reason, this leads to duplicated ids
n_occursi <- data.frame(table(datafullwin$ResponseId))
n_occursi[n_occursi$Freq > 1,]
# Remove duplicate ids
datafullwin <- datafullwin[!duplicated(datafullwin$ResponseId),]
saveRDS(datafullwin, "Data/data-qualcl-inc.RDS")

