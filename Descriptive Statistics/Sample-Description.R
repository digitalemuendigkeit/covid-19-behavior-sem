library(tidyverse)
library(car)

# Load data and crop to relevant section -c(1:43,48:96,143:145)
datafull <- read_rds("Data/S1-data-nm.RDS")
dataco <- datafull %>% filter(!is.na(COSKN))
datacc <- datafull %>% filter(!is.na(CCSKN))

test <- car::recode(datafull$SD2,
                    "'1' = 'female';
                    '2' = 'male';
                    '3' = 'other'",
                    as.factor = TRUE)


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
head(datavis)
