library(tidyverse)
library(car)
library(data.table)
library(patchwork)

# Data Loading ----
# Load data and crop to relevant section -c(1:43,48:96,143:145)
datafull <- read_rds(here::here("Data",
                                "open",
                                "S1-data-nm.RDS"))
dataco <- datafull %>% filter(!is.na(COSKN))
datacc <- datafull %>% filter(!is.na(CCSKN))


agge <- data.frame(read_csv2("Additional Analysis/age-gender-germany.csv", skip = 8))[-c(87:90),-1]
agge$X2 <- c(0:85)
agge[agge == "-"] <- NA
colnames(agge)[1] <- "Age"
agge <- sapply(agge, as.numeric)
aggegm <- agge[-c(1:18),1:8]
aggegf <- agge[-c(1:18),c(1,9:15)]
aggenm <- agge[-c(1:18),c(1,16:22)]
aggenf <- agge[-c(1:18),c(1,23:29)]
aggefull <- as.data.frame(aggegm) %>% transmute(Age = Age,
                                      German.Male = rowSums(aggegm[,-1], na.rm = TRUE)) %>%
          left_join(as.data.frame(aggegf) %>% transmute(Age = Age,
                                                        German.Female = rowSums(aggegf[,-1], na.rm = TRUE))) %>%
          left_join(as.data.frame(aggenm) %>% transmute(Age = Age,
                                                        Nongerman.Male = rowSums(aggenm[,-1], na.rm = TRUE))) %>%
          left_join(as.data.frame(aggenf) %>% transmute(Age = Age,
                                                        Nongerman.Female = rowSums(aggenf[,-1], na.rm = TRUE))) %>%
  mutate(Male = German.Male + Nongerman.Male,
         Female = German.Female + Nongerman.Female)

agebreaks <- c(18,25,30,35,40,45,50,55,60,65,70,75,80,85,500)
agelabels <- c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")

agudf <- data.frame(Male = double(), Female = double())
for (i in 1:(length(agebreaks)-1)){
  agudf <- agudf %>% union(data.frame(Male = sum(aggefull[(aggefull$Age >= agebreaks[i] & aggefull$Age < agebreaks[i+1]), 6]),
                                      Female = sum(aggefull[(aggefull$Age >= agebreaks[i] & aggefull$Age < agebreaks[i+1]), 7])))
}

aggefullc <- data.frame(Age.Groups = agelabels) %>% cbind(agudf)

# Source: Bildungsstand der Bevölkerung, Ergebnisse 2019
allgschu <- data.frame(Allg.Schul. = c("still a student", "dropped out of school", "Haupt-(Volks-)schulabschluss",
                                       "Abschluss der polytechnischen Oberschule",
                                       "Mittlerer Abschluss",
                                       "university entrance qualification",
                                       "no information"),
                       Count = c(2500, 2855,20202,4583,16635,23656,110))
allgschu2 <- data.frame(Education =  c("still a student",
                                       "dropped out of school",
                                       "secondary school leaving certificate",
                                       "university entrance qualification"),
                        Count = c(2500, 2855, 20202+4583+16635,23656))
berbil <- data.frame(Ber.Abschl. = c("Lehre",
                                     "Fachschulabschluss",
                                     "Fachschulabschluss DDR",
                                     "Bachelor",
                                     "Master",
                                     "Diplom",
                                     "Promotion",
                                     "in Ausbildung",
                                     "ohne"),
                     Count = c(32942, 5931, 640, 1867, 1258, 9090, 862, 6227, 11546))
eduge <- allgschu2[1:3,] %>%
  rbind(data.frame(Education = "university entrance qualification",
                   Count = c(allgschu2[4,2] - sum(berbil[4:7,2])))) %>%
  rbind(data.frame(Education = "university degree",
                   Count = c(sum(berbil[4:6,2])))) %>%
  rbind(data.frame(Education = c("doctorate", "a different level of education"),
                   Count = c(sum(berbil[7,2]),0)))

# Sources: Bevölkerung und Erwerbstätigkeit, 2019
# Destatis Studierende Insgesamt 2019
ebdf <- data.frame(Status = c("Erwerbspersonen", "Erwerbstätige", "davon Auszubildende", "Erwerbslose", "Nichterwerbspersonen", "Studierende"),
                   Count = c(43749, 42379, 1527, 1371,38099,2891.049))
ocge <- data.frame(Occupation = c("employed", "in vocational training", "student (university)", "student (school)", "not in paid employment"),
                   Ger.Count = c(ebdf[2,2]-ebdf[3,2], ebdf[3,2], ebdf[6,2], 0, ebdf[4,2]+ebdf[5,2]-ebdf[6,2]))


# Source: Statistisches Bundesamt, Haushaltsbuch, 2019
nekge <-  data.frame(read_csv2("Additional Analysis/household-income-classes.csv", skip = 7, skip_empty_rows = TRUE, ))[-c(4:6),]
nekge[,1] <- c("Erfasste Haushalte", "Hochgerechnete Haushalte", "Durchschnittliches monatliches Haushaltsnettoeinkommen in EUR")
colnames(nekge)[1] <- "Nettoeinkommensklasse"
nekge <- nekge[,-c(2:4,11)]
iccldf <- data.frame(Nettoeinkommensklasse = c("up to 1299 EUR",
                                             "between 1300 and 1699 EUR",
                                             "between 1700 and 2599 EUR",
                                             "between 2600 and 3599 EUR",
                                             "between 3600 EUR and 4999 EUR",
                                             "more than 5000 EUR"),
                   Count = unlist(nekge[2,-1]))
# Source: Statistisches Bundesamt, Haushaltsbuch, 2019
negge <-  data.frame(read_csv2("Additional Analysis/household-income-size.csv", skip = 7, skip_empty_rows = TRUE, ))[-c(4:6),-c(1:3)]
negge[,1] <- c("Erfasste Haushalte", "Hochgerechnete Haushalte", "Durchschnittliches monatliches Haushaltsnettoeinkommen in EUR")
ichsdf <- data.frame("Household.Size" = c("1 person", "2 persons", "3 persons", "4 persons", "5 persons or more"),
                     "Mean.Household.Net.Income.EUR" = unlist(negge[3,-c(1,7)]))

#Source: EU SILC 2019
neidf <- data.frame(Group = c("Pop.Year.Mean",
                              "Male.Year.Mean",
                              "Female.Year.Mean",
                              "Pop.Year.Median",
                              "Male.Year.Median",
                              "Female.Year.Median"),
                    Value.in.EUR = c(26105, 26816,25410,23515,24039,22979))
neidf <- neidf %>% rbind(neidf %>% mutate(
                         Group = c("Pop.Month.Mean",
                                   "Male.Month.Mean",
                                   "Female.Month.Mean",
                                   "Pop.Month.Median",
                                   "Male.Month.Median",
                                   "Female.Month.Median"),
                         Value.in.EUR = Value.in.EUR/12
                         )
                         )

datavis <- datafull %>% dplyr::transmute(Gender = car::recode(SD2,
                                                              "1 = 'female';
                                                              2 = 'male';
                                                              3 = 'other'",
                                                              as.factor = TRUE),
                                         Age = SD1,
                                         Education = car::recode(SD3,
                                                                 "1 = 'still a student';
                                                                 2 = 'dropped out of school';
                                                                 c(3,4) = 'secondary school leaving certificate';
                                                                 5 = 'university entrance qualification';
                                                                 6 = 'university degree';
                                                                 7 = 'doctorate';
                                                                 8 =  'a different level of education'",
                                                                 as.factor = TRUE,
                                                                 levels = c('still a student',
                                                                            'dropped out of school',
                                                                            'secondary school leaving certificate',
                                                                            'university entrance qualification',
                                                                            'university degree',
                                                                            'doctorate',
                                                                            'a different level of education')),
                                         Occupation = car::recode(SD4,
                                                                  "1 = 'employed full-time';
                                                                  2 = 'employed part-time';
                                                                  3 = 'in vocational training';
                                                                  4 = 'student (university)';
                                                                  5 = 'student (school)';
                                                                  6 = 'not in paid employment'",
                                                                  as.factor = TRUE,
                                                                  levels = c('employed full-time',
                                                                             'employed part-time',
                                                                             'in vocational training',
                                                                             'student (university)',
                                                                             'student (school)',
                                                                             'not in paid employment')),
                                         Income = car::recode(SD5,
                                                              "1 = 'up to 450 EUR';
                                                                2 = 'between 451 and 1000 EUR';
                                                                3 = 'between 1001 and 1500 EUR';
                                                                4 = 'between 1501 and 2000 EUR';
                                                                5 = 'between 2001 and 2500 EUR';
                                                                6 = 'between 2501 and 3000 EUR';
                                                                7 = 'between 3001 and 3500 EUR';
                                                                8 = 'between 3501 and 4000 EUR';
                                                                9 = 'between 4001 and 4500 EUR';
                                                                10 = 'between 4501 and 5000 EUR';
                                                                11 = 'more than 5000 EUR';
                                                                12 = 'not specified';
                                                                else = 'not specified'",
                                                              as.factor = TRUE,
                                                              levels = c('up to 450 EUR',
                                                                         'between 451 and 1000 EUR',
                                                                         'between 1001 and 1500 EUR',
                                                                         'between 1501 and 2000 EUR',
                                                                         'between 2001 and 2500 EUR',
                                                                         'between 2501 and 3000 EUR',
                                                                         'between 3001 and 3500 EUR',
                                                                         'between 3501 and 4000 EUR',
                                                                         'between 4001 and 4500 EUR',
                                                                         'between 4501 and 5000 EUR',
                                                                         'more than 5000 EUR',
                                                                         'not specified')),
                                         Income2 = car::recode(SD5,
                                                               "c(1,2) = 'up to 1000 EUR';
                                                                c(3,4) = 'between 1001 and 2000 EUR';
                                                                c(5,6) = 'between 2001 and 3000 EUR';
                                                                c(7,8) = 'between 3001 and 4000 EUR';
                                                                c(9,10) = 'between 4001 and 5000 EUR';
                                                                11 = 'more than 5000 EUR';
                                                                12 = 'not specified';
                                                                else = 'not specified'",
                                                               as.factor = TRUE,
                                                               levels = c('up to 1000 EUR',
                                                                          'between 1001 and 2000 EUR',
                                                                          'between 2001 and 3000 EUR',
                                                                          'between 3001 and 4000 EUR',
                                                                          'between 4001 and 5000 EUR',
                                                                          'more than 5000 EUR',
                                                                          'not specified')),
                                         Condition = car::recode(CCKN1,
                                                                 "1:3 = 'climate crisis';
                                                                 else = 'COVID-19 pandemic'"),
                                         Household.Size = SD6,
                                         Household.Child = as.numeric(ifelse(is.na(SD7), 0, SD7))
)
datavis[datavis$Household.Child == 11,]
datafull$SD7

setDT(datavis)[, Age.Groups := cut(Age,
                                   breaks = agebreaks,
                                   right = FALSE,
                                   labels = agelabels)]


# #theme_update(text = element_text(size = 12),
#              axis.title.x=element_blank(),
#              axis.ticks.x=element_blank(),
#              axis.title.y=element_text(angle = 90, vjust = 0.5),
#              legend.position = 'right',
#              legend.text = element_text(size = 10),
#              legend.key = element_blank(),
#              legend.title = element_blank(),
#              #legend.spacing.y = unit(0.3, "cm"),
#              plot.title = element_text(hjust = 0.5),
#              panel.background = element_blank(),
#              axis.line.x = element_line(colour = "black"),
#              panel.grid.major.y = element_line(colour = "#BBBBBB"),
#              axis.ticks.y = element_line(colour = "#BBBBBB"),
#              panel.grid.major.x = element_blank())



cbpaltor <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "#BBBBBB")
cbpaltor1 <- sort(cbpaltor)
cbpaltor2 <- rev(cbpaltor)

genderplot <- ggplot(datavis, aes(fill = str_wrap(Gender), x = Condition)) +
  geom_bar() +
  ggtitle("Gender Distribution") +
  #  theme(aspect.ratio = 1) +
  scale_fill_manual(values=cbpaltor,  breaks = c("female", "male", "other"))
genderplot

eduplot <- ggplot(datavis, aes(fill = Education, x = Condition)) +
  geom_bar() +
  ggtitle("Education") +
  scale_fill_manual(values=cbpaltor2)
eduplot

jobplot <- ggplot(datavis, aes(fill = Occupation, x = Condition)) +
  geom_bar() +
  ggtitle("Occupation") +
  scale_fill_manual(values=cbpaltor)
jobplot

ageplot <- ggplot(datavis, aes(fill = Condition, x = Age)) +
  geom_bar(aes(fill = Condition), position="dodge") +
  ggtitle("Age Distribution") +
  scale_fill_manual(values=cbpaltor2) +
  scale_y_continuous(breaks = seq(0,12,5)) +
  scale_x_continuous(breaks = seq(20,80,20))
ageplot

agedf <- aggefullc %>%
  rename(Male.Ger = Male, Female.Ger = Female) %>%
  left_join(datavis[Gender == "female"] %>% group_by(Age.Groups) %>% summarise(Female.Sam = n())) %>%
  left_join(datavis[Gender == "male"] %>% group_by(Age.Groups) %>% summarise(Male.Sam = n()))
agedf[is.na(agedf)] <- 0
agedf <- agedf %>%
  mutate(Pop.Ger = Male.Ger + Female.Ger,
         Pop.Sam = Male.Sam + Female.Sam) %>%
  mutate(Male.Ger.p = Male.Ger/sum(Pop.Ger),
         Female.Ger.p = Female.Ger/sum(Pop.Ger),
         Female.Sam.p = Female.Sam/sum(Pop.Sam),
         Male.Sam.p = Male.Sam/sum(Pop.Sam))
agedfm <- agedf %>% mutate(
  Female.Ger = -Female.Ger,
  Female.Sam = -Female.Sam,
  Female.Ger.p = -Female.Ger.p,
  Female.Sam.p = -Female.Sam.p
)
# Find out the conversion to percent to rename scales
max(agedf$Male.Ger)

genderpal <- c("#009988", "#EE7733")

agelong <- agedfm %>%
  pivot_longer(!Age.Groups, names_to = "Subgroup", values_to = "Count") %>%
  mutate(Age.Groups = factor(Age.Groups, levels = unique(Age.Groups))) %>%
  mutate(ageno = as.numeric(Age.Groups) - 0.5)
agelong <- agelong %>% rbind(agelong %>% filter(Subgroup == "Female.Ger.p" | Subgroup == "Male.Ger.p", Age.Groups == "85+") %>% mutate(ageno = ageno + 1))
agelong[agelong == "Female.Sam.p"] <- "female"
agelong[agelong == "Male.Sam.p"] <- "male"
agelong[agelong == "Female.Ger.p"] <- "female in population"
agelong[agelong == "Male.Ger.p"] <- "male in population"

pyrplot <- ggplot(agelong, aes(x = Age.Groups, y = Count)) +
  geom_bar(data = agelong %>% filter(Subgroup == "female"), aes(fill = Subgroup), stat = "identity") +
  geom_bar(data = agelong %>% filter(Subgroup == "male"), aes(fill = Subgroup), stat = "identity") +
  geom_step(data = agelong %>% filter(Subgroup == "female in population"), aes(x = ageno, linetype = "female and male")) +
  geom_step(data = agelong %>% filter(Subgroup == "male in population"), aes(x = ageno, linetype = "female and male")) +
  coord_flip() +
  scale_fill_manual(name = "", values = genderpal, labels = c("female in sample", "male in sample")) +
  scale_linetype_manual(name = "", values = "solid", labels = "in population") +
  scale_y_continuous(name = "Sample count",
                     breaks = seq(-70/sum(agedf$Pop.Sam), 70/sum(agedf$Pop.Sam), by= 10/sum(agedf$Pop.Sam)),
                     labels = c(70, 60,50,40,30,20,10,0,10,20,30,40,50,60, 70),
                     sec.axis = sec_axis(trans =~.*1,name = "Population count",
                                         breaks = seq(-5000000/sum(agedf$Pop.Ger),5000000/sum(agedf$Pop.Ger),1000000/sum(agedf$Pop.Ger)),
                                         labels = paste0(as.character(c(5:0,1:5)), "m"))) +
  labs(title = "Sample gender and age compared with German population",
       x = "Age groups",
       caption = "Data source: 2019 German census, Statistisches Bundesamt (Destatis)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))
pyrplot

edufull <- eduge %>% left_join(datavis %>% group_by(Education) %>% summarise(Sample.Count = n()))
edufull[is.na(edufull)] <- 0
edufull <- edufull %>%
  mutate(Ger.p = Count/sum(edufull$Count),
         Sam.p = Sample.Count/sum(edufull$Sample.Count))
edulong <- edufull %>% pivot_longer(!Education, names_to = "Subgroup", values_to = "Count") %>%
  mutate(Education = factor(Education, levels = levels(datavis$Education))) %>%
  mutate(eduno = rev(as.numeric(Education) - 0.5))
edulong <- edulong %>% rbind(edulong %>% filter(Subgroup == "Ger.p", Education == "still a student") %>% mutate(eduno = eduno + 1))

eduplot2 <- ggplot(edulong, aes(x = Education, y = Count)) +
  geom_bar(data = edulong %>% filter(Subgroup == "Sam.p"), aes(fill = Subgroup), stat = "identity") +
  geom_step(data = edulong %>% filter(Subgroup == "Ger.p"), aes(x = eduno, linetype = "in population*")) +
  coord_flip() +
  scale_fill_manual(name = "", labels = "in sample",values = "#0077BB") +
  scale_linetype_manual(name = "", values = "solid") +
  scale_y_continuous(name = "Sample count",
                     breaks = seq(0, 500/sum(edufull$Sample.Count), by=100/sum(edufull$Sample.Count)),
                     labels = c(0,100*(1:5)),
                     sec.axis = sec_axis(trans =~.*1,name = "Population count",
                                         breaks = seq(0, 40000/sum(edufull$Count),by=10000/sum(edufull$Count)),
                                         labels = paste0(as.character(c(0,10*(1:4))), "m"))) +
  labs(title = "Sample education compared with German population*",
       x = "Highest level of education",
       caption = "*age 15 and above;
       Data source: 2019 German census, Statistisches Bundesamt (Destatis)") +
  theme_minimal() +
  scale_x_discrete(limits = rev(levels(datavis$Education))) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))
eduplot2

ocsam <- datavis %>% group_by(Occupation) %>% summarise(Sample.Count = n())
ocsam <- ocsam %>% rbind(data.frame(Occupation = "employed", Sample.Count = sum(ocsam[1:2,2])))
ocfull <- ocge %>% left_join(ocsam)
ocfull <- ocfull %>%
  mutate(Ger.p = Ger.Count/sum(ocfull$Ger.Count),
         Sam.p = Sample.Count/sum(ocfull$Sample.Count)) %>%
  mutate(Occupation = rev(c("not in paid employment", "student (school)**", "student (university)", "in vocational training", "employed")))
oclong <- ocfull %>% pivot_longer(!Occupation, names_to = "Subgroup", values_to = "Count") %>%
  mutate(Occupation = factor(Occupation, levels = c("not in paid employment", "student (school)**", "student (university)", "in vocational training", "employed"))) %>%
  mutate(ocno = as.numeric(Occupation) - 0.5)
oclong <- oclong %>% rbind(oclong %>% filter(Subgroup == "Ger.p", Occupation == "employed") %>% mutate(ocno = ocno + 1))

ocplot2 <- ggplot(oclong, aes(x = Occupation, y = Count)) +
  geom_bar(data = oclong %>% filter(Subgroup == "Sam.p"), aes(fill = Subgroup), stat = "identity") +
  geom_step(data = oclong %>% filter(Subgroup == "Ger.p"), aes(x = ocno, linetype = "in population*")) +
  coord_flip() +
  scale_fill_manual(name = "", labels = "in sample",values = "#EE3377") +
  scale_linetype_manual(name = "", values = "solid") +
  scale_y_continuous(name = "Sample count",
                     breaks = seq(0, 600/sum(ocfull$Sample.Count), by=100/sum(ocfull$Sample.Count)),
                     labels = c(0,100*(1:6)),
                     sec.axis = sec_axis(trans =~.*1,name = "Population count",
                                         breaks = seq(0, 50000/sum(ocfull$Ger.Count),by=10000/sum(ocfull$Ger.Count)),
                                         labels = paste0(as.character(c(0,10*(1:5))), "m"))) +
  labs(title = "Sample occupation compared with German population*",
       x = "Occupation",
       caption = "*age 15 and above; **no census data available;
       Data source: 2019 German census and 2019/2020 winter semester, Statistisches Bundesamt (Destatis)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))
ocplot2

icclsample <- datavis %>% group_by(Income) %>% summarise(Sample.Count = n())
icclsample2 <- data.frame(Net.Income.Class = c("up to 1000 EUR",
                                     "between 1001 and 1500 EUR",
                                     "between 1501 and 2500 EUR",
                                     "between 2501 and 3500 EUR",
                                     "between 3501 and 5000 EUR",
                                     "more than 5001 EUR",
                                     "not specified"),
                          Sample.Count = c(icclsample$Sample.Count[1]+icclsample$Sample.Count[2],
                                           icclsample$Sample.Count[3],
                                           icclsample$Sample.Count[4]+icclsample$Sample.Count[5],
                                           icclsample$Sample.Count[6]+icclsample$Sample.Count[7],
                                           icclsample$Sample.Count[8]+icclsample$Sample.Count[9]+icclsample$Sample.Count[10],
                                           icclsample$Sample.Count[11],
                                           icclsample$Sample.Count[12]))
icclfull <- (iccldf %>%
               rename(Net.Income.Class = Nettoeinkommensklasse,
                               Pop.Count = Count)) %>%
  full_join(icclsample2)
icclfull <- icclfull %>%
  mutate(Pop.p = Pop.Count/sum(icclfull$Pop.Count, na.rm = TRUE),
         Sam.p = Sample.Count/sum(icclfull$Sample.Count, na.rm = TRUE))

iccllong <- icclfull %>%
  pivot_longer(!Net.Income.Class, names_to = "Subgroup", values_to = "Count") %>%
  mutate(Net.Income.Class = factor(Net.Income.Class, levels = rev(c("up to 1000 EUR",
                                                                "up to 1299 EUR",
                                                                "between 1001 and 1500 EUR",
                                                                "between 1300 and 1699 EUR",
                                                                "between 1501 and 2500 EUR",
                                                                "between 1700 and 2599 EUR",
                                                                "between 2501 and 3500 EUR",
                                                                "between 2600 and 3599 EUR",
                                                                "between 3501 and 5000 EUR",
                                                                "between 3600 EUR and 4999 EUR",
                                                                "more than 5000 EUR",
                                                                "more than 5001 EUR",
                                                                "not specified")))) #%>%
  # mutate(icno = as.numeric(Net.Income.Class))

icclassplot <- ggplot(iccllong, aes(x = Net.Income.Class, y = Count)) +
  geom_bar(data = iccllong %>% filter(Subgroup == "Sam.p"), aes(fill = Subgroup, colour = Subgroup), stat = "identity") +
  geom_bar(data = iccllong %>% filter(Subgroup == "Pop.p"), aes(fill = Subgroup, colour = Subgroup), stat = "identity") +
  coord_flip() +
  scale_fill_manual(name = "", labels = c("sample participants", "German households"), values = c("#33BBEE", NA)) +
  scale_color_manual(name = "", labels = c("sample participants", "German households"), values = c("#33BBEE","#000000")) +
  scale_y_continuous(name = "Sample count",
                     breaks = seq(0, 300/sum(icclfull$Sample.Count, na.rm = TRUE), by=50/sum(icclfull$Sample.Count, na.rm = TRUE)),
                     labels = c(0,50*(1:6)),
                     sec.axis = sec_axis(trans =~.*1,name = "Household count",
                                         breaks = seq(0, 9000/sum(icclfull$Pop.Count, na.rm = TRUE),by=1000/sum(icclfull$Pop.Count, na.rm = TRUE)),
                                         labels = paste0(as.character(c(0,(1:9))), "m"))) +
  labs(title = "Net monthly household income of participants sample compared with German households",
       x = "Net monthly household income",
       caption = "Data source: 2019, Statistisches Bundesamt (Destatis)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
  # geom_step(data = iccllong %>% filter(Subgroup == "Pop.p"), aes(x = icno, linetype = "in population"))

ichsprox <- datavis %>% select(Income,Household.Size,Household.Child)
levels(ichsprox$Income) <- c(450,
                             (451+1000)/2,
                             (1001+1500)/2,
                             (1501+2000)/2,
                             (2001+2500)/2,
                             (2501+3000)/2,
                             (3001+3500)/2,
                             (3501+4000)/2,
                             (4001+4500)/2,
                             (4500+5000)/2,
                             5001*2,
                             NA)
ichsprox2 <- ichsprox[,1:2] %>% rename(Household.Size = Household.Size)
ichsprox2$Household.Size[ichsprox2$Household.Size >= 5] <- "5 or more"
ichsprox2$Income <- as.numeric(levels(ichsprox2$Income))[ichsprox2$Income]
ichsfull <- ichsdf %>%
  mutate(Household.Size = c("1", "2", "3", "4", "5 or more")) %>%
  left_join(ichsprox2 %>% group_by(Household.Size) %>% summarize(Approximate.MNI.Sam.EUR = mean(Income, na.rm = TRUE)))

ichslong <- ichsfull %>% pivot_longer(!Household.Size, names_to = "Subgroup", values_to = "Mean.Household.Income.EUR") %>%
  mutate(Household.Size = factor(Household.Size)) %>%
  mutate(icno = as.numeric(Household.Size) - 0.5)
ichslong <- ichslong %>% rbind(ichslong %>% filter(Subgroup == "Mean.Household.Net.Income.EUR", Household.Size == "5 or more") %>%
                                 mutate(icno = icno + 1))

ichsizeplot <- ggplot(ichslong, aes(x = Household.Size, y = Mean.Household.Income.EUR)) +
  geom_bar(data = ichslong %>% filter(Subgroup == "Approximate.MNI.Sam.EUR"), aes(fill = Subgroup), stat = "identity") +
  geom_step(data = ichslong %>% filter(Subgroup == "Mean.Household.Net.Income.EUR"), aes(x = icno, linetype = "population")) +
  coord_flip() +
  scale_fill_manual(name = "", labels = "sample (approximated)*",values = "#33BBEE") +
  scale_linetype_manual(name = "", values = "solid") +
  scale_y_continuous(breaks = seq(0, 6000, by = 2000),
                     labels = c(paste0(c(0, 2000, 4000, 6000), " EUR"))) +
  labs(title = "Net monthly household income of sample compared with German population",
       x = "Household size",
       y = "Mean net monthly household income",
       caption = "Mean for sample approximated from income classes;
       Data source: 2019, Statistisches Bundesamt (Destatis)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ichsizeplot

heidf <- ichsprox %>% mutate(Household.Adult = Household.Size - Household.Child)
heidf$Income <- as.numeric(levels(heidf$Income))[heidf$Income]
heidf <- heidf %>% mutate(NEI = ifelse(Household.Size == 1, 12*Income, 12*Income/(1+(Household.Adult-1)/2+Household.Child*0.3)))
median(heidf$NEI, na.rm = TRUE)
mean(heidf$NEI, na.rm = TRUE)

sampleplot <- pyrplot +  eduplot2 + ocplot2 + plot_layout(nrow = 3, guides = 'keep') & theme(legend.justification = "left")
ggsave("Figures/sampledescription.png", width = 15.5, height = 15.5, units = "cm")
pyrplot
ggsave("Figures/sampleagegender.png", width = 15.5, height = 10, units = "cm")
eduplot2
ggsave("Figures/sampleeducation.png", width = 15.5, height = 10, units = "cm")
ocplot2
ggsave("Figures/sampleoccupation.png", width = 15.5, height = 10, units = "cm")
