library(tidyverse)
library(car)
library(data.table)


# Load data and crop to relevant section -c(1:43,48:96,143:145)
datafull <- read_rds("Data/S1-data-nm.RDS")
dataco <- datafull %>% filter(!is.na(COSKN))
datacc <- datafull %>% filter(!is.na(CCSKN))

agge <- data.frame(read_csv2("Descriptive Statistics/age-gender-germany.csv", skip = 8))[-c(87:90),-1]
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
                                                                 else = 'COVID-19 pandemic'")
                                         )
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
  left_join(datavis[Gender == "male"] %>% group_by(Age.Groups) %>% summarise(Male.Sam = n())) %>%
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
  scale_fill_manual(name = "In sample", values = genderpal) +
  scale_linetype_manual(name = "In population", values = "solid") +
  scale_y_continuous(name = "Sample count",
                     breaks = seq(-70/sum(agedf$Pop.Sam), 70/sum(agedf$Pop.Sam), by= 10/sum(agedf$Pop.Sam)),
                     labels = c(70, 60,50,40,30,20,10,0,10,20,30,40,50,60, 70),
                     sec.axis = sec_axis(trans =~.*1,name = "Population count",
                                         breaks = seq(-5000000/sum(agedf$Pop.Ger),5000000/sum(agedf$Pop.Ger),1000000/sum(agedf$Pop.Ger)),
                                         labels = paste0(as.character(c(5:0,1:5)), "m"))) +
  labs(title = "Sample composition compared with German population (2019)",
       x = "Age groups") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
pyrplot

eduplot2 <- ggplot(datavis, aes(fill = Education, x = Education)) +
  geom_bar() +
  ggtitle("Education") +
  scale_fill_manual(values=cbpaltor2) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(datavis$Education)))
eduplot2
