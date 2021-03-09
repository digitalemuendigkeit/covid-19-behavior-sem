library(tidyverse)
library(psych)
library(Hmisc)

#' correlation_matrix
#' Creates a publication-ready / formatted correlation matrix, using `Hmisc::rcorr` in the backend.
#'
#' @param df dataframe; containing numeric and/or logical columns to calculate correlations for
#' @param type character; specifies the type of correlations to compute; gets passed to `Hmisc::rcorr`; options are `"pearson"` or `"spearman"`; defaults to `"pearson"`
#' @param digits integer/double; number of decimals to show in the correlation matrix; gets passed to `formatC`; defaults to `3`
#' @param decimal.mark character; which decimal.mark to use; gets passed to `formatC`; defaults to `.`
#' @param use character; which part of the correlation matrix to display; options are `"all"`, `"upper"`, `"lower"`; defaults to `"all"`
#' @param show_significance boolean; whether to add `*` to represent the significance levels for the correlations; defaults to `TRUE`
#' @param replace_diagonal boolean; whether to replace the correlations on the diagonal; defaults to `FALSE`
#' @param replacement character; what to replace the diagonal and/or upper/lower triangles with; defaults to `""` (empty string)
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' `correlation_matrix(iris)`
#' `correlation_matrix(mtcars)`
correlation_matrix <- function(df,
                               type = "pearson",
                               digits = 3,
                               decimal.mark = ".",
                               use = "all",
                               show_significance = TRUE,
                               replace_diagonal = FALSE,
                               replacement = ""){

  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)

  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]

  # transform input data frame to matrix
  x <- as.matrix(df)

  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)

  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }

  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")

  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }

  return(Rnew)
}

#Import Data
# Data used is nonnormal, missing values are not treated
datafull <- read_rds("Data/data-qualcl-inc.RDS")[,-c(1:5,143:145)]
# Replace 7 for CCDN3, CCIN3, CODN3 and COIN3 with NA
datafull$CCDN3[datafull$CCDN3 == 7] <- NA
datafull$CCIN3[datafull$CCIN3 == 7] <- NA
datafull$CODN3[datafull$CODN3 == 7] <- NA
datafull$COIN3[datafull$COIN3 == 7] <- NA

#Make scales from personality data
anper <- data.frame(Variable = c(rep("Extraversion",3),
                                 rep("Agreeableness",3),
                                 rep("Conscientiousness",3),
                                 rep("Neuroticism",3),
                                 rep("Openness to Experience",3),
                                 rep("Internal Control Conviction",2),
                                 rep("External Control Conviction",2)),
                    Item = c(paste0("BFE", 1:3),
                             paste0("BFA", 1:3),
                             paste0("BFC", 1:3),
                             paste0("BFN", 1:3),
                             paste0("BFO", 1:3),
                             paste0("IC", 1:2),
                             paste0("EC", 1:2))
)
anperun <- data.frame(Variable = unique(anper$Variable))
anperunalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(anperun)){
  x = anperun[i,1]
  anperunalpha <- append(anperunalpha,
                         ((unlist(psych::alpha(datafull[unlist((filter(anper, Variable == {{x}}))["Item"])], check.keys = FALSE)[1]))[2]))
  print(psych::alpha(datafull[unlist((filter(anper, Variable == {{x}}))["Item"])], check.keys = FALSE))
}
anperun <- anperun %>% cbind(Cs.Alpha = anperunalpha)
anperalpha <- anper %>% left_join(anperun)
# Cronbach's alpha is insufficient for all constructs except Big Five Neuroticism
# BFE: No alpha >0.7 attainable through item deletion, choose item with highest item-total correlation (r.drop): BFE2
# BFA: No alpha > 0.7 attainable through item deletion, choose item with highest item-total correlation: BFA1
# BFC: see above,choose BFC2
# BFO: Choose BFO3
# IC: Choose IC2 (content)
# EC: Choose EC1 (content)
keepper <- c("BFE2", "BFA1", "BFC2", "BFN1", "BFN2", "BFN3", "BFO3", "IC2", "EC1")
codeper <- anper %>% filter(Item %in% keepper)

# Make scales for CC data

#Climate Change
mmcc <- (readRDS("SEM Climate Crisis/Models/model-cc-1.RDS"))$measurement_model
ancc <- data.frame(Variable = (unlist(mmcc))[c(T, F, F)],
                   Item = (unlist(mmcc))[c(F, T, F)]) %>%
  filter(!(Item %in% Variable)) %>%
  filter(!(Item == "CCKN")) %>%
  rbind(data.frame(Variable = c(rep("Descriptive Norm", 2),
                                 rep("Injunctive Norm", 2),
                                 paste0(c("Perceived Self-Efficacy",
                                          "Perceived Response Efficacy",
                                          "Perceived Response Costs",
                                          "Behavioral Intention"),
                                        c(rep(" Diet", 4),
                                          rep(" Heating",4),
                                          rep(" Driving",4),
                                          rep(" General",4),
                                          rep("",4))),
                                 "Subjective Knowledge",
                                paste0("Behavior", c(rep("",4),
                                                     " Diet",
                                                     " Heating",
                                                     " Driving",
                                                     " General"))
  ),
  Item = c(paste0("CC",
                       c(paste0("DN", 3:4),
                         paste0("IN", 3:4),
                         paste0("RB", 1:3),
                         "BI1",
                         paste0("RB", 4:6),
                         "BI2",
                         paste0("RB", 7:9),
                         "BI3",
                         rep(c(paste0("RB",10:12),
                               "BI4"),2),
                         "SKN",
                         rep(paste0("B", 1:4),2))))
  ))
ancc[ancc == "Benevolence"] <- "Distrusting Beliefs Benevolence"
ancc[ancc == "Competence"] <- "Distrusting Beliefs Competence"
ancc[ancc == "Integrity"] <- "Distrusting Beliefs Integrity"
anccun <- data.frame(Variable = unique(ancc$Variable))
anccunalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(anccun)){
  x = anccun[i,1]
  anccunalpha <- append(anccunalpha,
                      ifelse(test = ncol(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])]) > 1,
                         ((unlist(psych::alpha(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])], check.keys = FALSE)[1]))[2]), NA))
   # print(ifelse(test = ncol(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])]) > 1,
   #              psych::alpha(datafull[unlist((filter(ancc, Variable == {{x}}))["Item"])], check.keys = FALSE), NA))
}
anccun <- anccun %>% cbind(Cs.Alpha = anccunalpha)
anccalpha <- ancc %>% left_join(anccun)
# Cronbach's alpha is sufficient for
# CC Perceived Self-Efficacy
# CC Perceived Response Efficacy
# CC Distrusting Beliefs Benevolence, Competence, Integrity
# CC Perceived Susceptibility
# CC Perceived Severity
# CC Personal Moral Norm
# CC Descriptive Norm
# CC Injunctive Norm
# CC Behavioral Intention
# CC Behavior
# Cronbach's alpha is insufficient for
# CC Perceived Response Costs
psych::alpha(datafull %>% select(c(paste0("CCRB", c(7:9,12)))))
# Without CCRB9:
psych::alpha(datafull %>% select(c(paste0("CCRB", c(7:8,12)))))
# Use CCRB12
codecc <- ancc %>% filter(!(Item %in% paste0("CCRB", 7:9))) %>%
  mutate(Variable = paste0("Climate Crisis ", Variable))

# Make scales for CO data

#COVID-19
mmco <- (readRDS("SEM COVID-19/Models/model-co-1.RDS"))$measurement_model
anco <- data.frame(Variable = (unlist(mmco))[c(T, F, F)],
                   Item = (unlist(mmco))[c(F, T, F)]) %>%
  filter(!(Item %in% Variable)) %>%
  filter(!(Item == "COKN")) %>%
  rbind(data.frame(Variable = c(rep("Descriptive Norm", 2),
                                rep("Injunctive Norm", 2),
                                paste0(c("Perceived Self-Efficacy",
                                         "Perceived Response Efficacy",
                                         "Perceived Response Costs",
                                         "Behavioral Intention"),
                                       c(rep(" Contact", 4),
                                         rep(" App",4),
                                         rep(" Mask",4),
                                         rep(" General",4),
                                         rep("",4))),
                                "Subjective Knowledge",
                                paste0("Behavior", c(rep("",4),
                                                     " Contact",
                                                     " App",
                                                     " Mask",
                                                     " General"))
  ),
  Item = c(paste0("CO",
                  c(paste0("DN", 3:4),
                    paste0("IN", 3:4),
                    paste0("RB", 1:3),
                    "BI1",
                    paste0("RB", 4:6),
                    "BI2",
                    paste0("RB", 7:9),
                    "BI3",
                    rep(c(paste0("RB",10:12),
                          "BI4"),2),
                    "SKN",
                    rep(paste0("B", 1:4),2))))
  )%>%
    rbind(data.frame(Variable = "Perceived Self-Efficacy", Item = "CORB3"))
  )
anco[anco == "Benevolence"] <- "Distrusting Beliefs Benevolence"
anco[anco == "Competence"] <- "Distrusting Beliefs Competence"
anco[anco == "Integrity"] <- "Distrusting Beliefs Integrity"
ancoun <- data.frame(Variable = unique(anco$Variable))
ancounalpha <- c()
# Analyze Cronbach's Alpha
for (i in 1:nrow(ancoun)){
  x = ancoun[i,1]
  ancounalpha <- append(ancounalpha,
                        ifelse(test = ncol(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])]) > 1,
                               ((unlist(psych::alpha(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])], check.keys = FALSE)[1]))[2]), NA))
  # print(ifelse(test = ncol(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])]) > 1,
  #              psych::alpha(datafull[unlist((filter(anco, Variable == {{x}}))["Item"])], check.keys = FALSE), NA))
}
ancoun <- ancoun %>% cbind(Cs.Alpha = ancounalpha)
ancoalpha <- anco %>% left_join(ancoun)
# Cronbach's alpha is sufficient for all constructs
codeco <- anco %>% mutate(Variable = paste0("COVID-19 ", Variable))

codefull <- codeco %>% union(codecc) %>% union(codeper)
codefullun <- unique(codefull$Variable)

datacoded <- data.frame(Dummy = character(length = nrow(datafull)))
for (i in 1:length(codefullun)){
  x = codefullun[i]
  datacoded <- datacoded %>%
    cbind(Bob = rowMeans((datafull %>% select(unlist((codefull %>% filter(Variable == {{x}}))[,2]))), na.rm = TRUE))
}
datacoded <- datacoded[,-1]
colnames(datacoded) <- codefullun

# Make descriptive statistics
descstat <- data.frame(Variable = rownames(psych::describe(datacoded)), psych::describe(datacoded)) %>%
  left_join(rbind(anperalpha,
                  (anccalpha%>% mutate(Variable = paste0("Climate Crisis ", Variable))),
                   (ancoalpha %>% mutate(Variable = paste0("COVID-19 ", Variable)))) %>%
              group_by(Variable) %>%
              mutate(Items = paste0(Item, collapse = ", "),
                     Item = NULL) %>%
              distinct(Variable, .keep_all = TRUE)

  )
# order by groups and alphabetical
descstatalph <- descstat[c(order(descstat$Variable)[order(descstat$Variable) %in% grep("Climate Crisis", descstat$Variable)],
                          order(descstat$Variable)[order(descstat$Variable) %in% grep("COVID-19", descstat$Variable)],
                          66:72),]

descstatshort <- descstat %>%
  transmute(
  Variable = Variable,
  Items = Items,
  n = n,
  Cs.Alpha = ifelse(is.na(Cs.Alpha), "-", round(Cs.Alpha, 2)),
  Mean = round(mean,2),
  SD = round(sd,2)
)

# Append Knowledge data
datacodedkn <- datacoded %>%
  cbind(datafull %>% select((contains("KN")&!(contains("SKN")))))
datacodedkn$'Climate Crisis Knowledge Sum Correct' <- rowSums(datacodedkn %>% select(contains("CCKN")) == 3)
datacodedkn$'Climate Crisis Knowledge Sum DK' <- rowSums(datacodedkn %>% select(contains("CCKN")) == 2)
datacodedkn$'Climate Crisis Knowledge Sum Incorrect' <- rowSums(datacodedkn %>% select(contains("CCKN")) == 1)
datacodedkn$'COVID-19 Knowledge Sum Correct' <- rowSums(datacodedkn %>% select(contains("COKN")) == 3)
datacodedkn$'COVID-19 Knowledge Sum DK' <- rowSums(datacodedkn %>% select(contains("COKN")) == 2)
datacodedkn$'COVID-19 Knowledge Sum Incorrect' <- rowSums(datacodedkn %>% select(contains("COKN")) == 1)

# Append demographic and other vars
datarest <- datafull %>% dplyr::transmute(Gender = car::recode(SD2,
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
                                      )
psych::describe(datafull$SD8)
# Are Behavior or Behavioral Intention correlated with count or incidence?
correlation_matrix(datafull %>% select(starts_with("COB") | starts_with("Count") | starts_with("Incidence")),
                   type = "spearman")
summary(aov(datarest$Age ~ datarest$Gender))

# Exploratory correlation analysis
allcor <- correlation_matrix(df = datacodedkn %>% select(!starts_with("CC") & !starts_with("CO")),
                                  type = "spearman",
                                  use = "lower"
)
allcorv <- cor(datacodedkn %>% select(!starts_with("CC") & !starts_with("CO")),
                   method = "spearman", use = "pairwise.complete.obs")
BigLargeCors <- data.frame(V1 = rownames(which(allcorv > 0.7 & allcorv < 0.999, arr.ind = TRUE)),
                           V2 = colnames(allcorv[,which(allcorv > 0.7 & allcorv < 0.999, arr.ind = TRUE)[,2]]),
                           SC = round(allcorv[allcorv > 0.7 & allcorv < 0.999], 3))
# No surprises

# Look at correlations
# COVID-19
# Behavioral Intention and Behavior
cor(datacoded %>% select(starts_with("COVID-19 Behavior")), use = "pairwise.complete.obs", method = "spearman")
cor.test(datacoded[,"COVID-19 Behavior"],datacoded[,"COVID-19 Behavioral Intention"], method = "spearman")
corr.test(datacoded %>% select(starts_with("COVID-19 Behavior")), use = "pairwise.complete.obs", method = "spearman")
# Behavioral Intention and Behavior and all possible predictors
cor(datacodedkn %>% select(!starts_with("COVID-19 Behavior") &
                             !starts_with("Climate Crisis") &
                             !starts_with("CC") &
                             !ends_with("Contact") &
                             !ends_with("App") &
                             !ends_with("Mask") &
                             !ends_with("General")),
    datacodedkn[,c("COVID-19 Behavior", "COVID-19 Behavioral Intention")],
    use = "pairwise.complete.obs", method = "spearman")
corr.test(datacodedkn %>% select(!starts_with("COVID-19 Behavior") &
                             !starts_with("Climate Crisis") &
                             !starts_with("CC") &
                             !ends_with("Contact") &
                             !ends_with("App") &
                             !ends_with("Mask") &
                             !ends_with("General")),
    datacodedkn[,c("COVID-19 Behavior", "COVID-19 Behavioral Intention")],
    use = "pairwise.complete.obs", method = "spearman")

#COVID-19 Corr matrix
covidcor <- correlation_matrix(df = datacodedkn %>% select(starts_with("COVID-19") &
                                            !ends_with("Contact") &
                                            !ends_with("App") &
                                            !ends_with("Mask") &
                                            !ends_with("General")),
                   type = "spearman",
                   use = "lower"
)
#Climate Crisis Corr matrix
climatecor <- correlation_matrix(df = datacodedkn %>% select(starts_with("Climate Crisis") &
                                                 !ends_with("Diet") &
                                                 !ends_with("Driving") &
                                                 !ends_with("Heating") &
                                                 !ends_with("General")),
                   type = "spearman",
                   use = "lower"
)
#Behavior  Psych Corr matrix
psychcor <- correlation_matrix(df = datacodedkn %>% select(contains("Behavior") |
                                        !starts_with("Climate Crisis") &
                                        !starts_with("COVID-19") &
                                        !starts_with("CO") &
                                        !starts_with("CC")),
                   type = "spearman",
                   use = "lower")


#


#Correlation of age and behavior?
cor(datavars[,c("CCBI","CCB","CCPBI","CCPB")], datavars[,"Age"], use ="pairwise.complete.obs")
cor(datavars[,c("COBI","COB","COPBI","COPB")], datavars[,"Age"], use ="pairwise.complete.obs")
#no correlation of age

#Next, t-tests
#sociodemographic variables
#Differences in Gender and Behavioral Intention?
datavars[(datavarsdf$C.Gender %in% 1),"CCBI"]
t.test(datavars[(datavarsdf$C.Gender %in% 1),"CCBI"], datavars[(datavarsdf$C.Gender %in% 2),"CCBI"])
sd(datavars[(datavarsdf$C.Gender %in% 1),"CCBI"])
sd(datavars[(datavarsdf$C.Gender %in% 2),"CCBI"])
#yes!
t.test(datavars[(datavarsdf$C.Gender %in% 1),"COBI"], datavars[(datavarsdf$C.Gender %in% 2),"COBI"])
#also yes
sd(datavars[(datavarsdf$C.Gender %in% 1),"COBI"])
sd(datavars[(datavarsdf$C.Gender %in% 2),"COBI"])
#how about actual behavior
t.test(datavars[(datavarsdf$C.Gender %in% 1),"CCB"], datavars[(datavarsdf$C.Gender %in% 2),"CCB"])
#no
t.test(datavars[(datavarsdf$C.Gender %in% 1),"COB"], datavars[(datavarsdf$C.Gender %in% 2),"COB"])
sd(datavars[(datavarsdf$C.Gender %in% 1),"COB"], na.rm = TRUE)
sd(datavars[(datavarsdf$C.Gender %in% 2),"COB"], na.rm = TRUE)
#tiny bit
#income and CCBI
#high income is 11
t.test(datavars[(datavarsdf$C.Income %in% 11),"CCBI"], datavars[!(datavarsdf$C.Income %in% 11),"CCBI"])
#low income
t.test(datavars[(datavarsdf$C.Income %in% 1),"CCBI"], datavars[!(datavarsdf$C.Income %in% 1),"CCBI"])
#mid income
t.test(datavars[(datavarsdf$C.Income %in% 6),"CCBI"], datavars[!(datavarsdf$C.Income %in% 6),"CCBI"])

#income and CCB
#high income is 11
t.test(datavars[(datavarsdf$C.Income %in% 11),"CCB"], datavars[!(datavarsdf$C.Income %in% 11),"CCB"])
#low income
t.test(datavars[(datavarsdf$C.Income %in% 1),"CCB"], datavars[!(datavarsdf$C.Income %in% 1),"CCB"])
#mid income
t.test(datavars[(datavarsdf$C.Income %in% 6),"CCB"], datavars[!(datavarsdf$C.Income %in% 6),"CCB"])

#income and COBI
#high income is 11
t.test(datavars[(datavarsdf$C.Income %in% 11),"COBI"], datavars[!(datavarsdf$C.Income %in% 11),"COBI"])
#low income
t.test(datavars[(datavarsdf$C.Income %in% 1),"COBI"], datavars[!(datavarsdf$C.Income %in% 1),"COBI"])
#mid income
t.test(datavars[(datavarsdf$C.Income %in% 6),"COBI"], datavars[!(datavarsdf$C.Income %in% 6),"COBI"])

#income and COB
#high income is 11
t.test(datavars[(datavarsdf$C.Income %in% 11),"COB"], datavars[!(datavarsdf$C.Income %in% 11),"COB"])
#low income
t.test(datavars[(datavarsdf$C.Income %in% 1),"COB"], datavars[!(datavarsdf$C.Income %in% 1),"COB"])
#mid income
t.test(datavars[(datavarsdf$C.Income %in% 6),"COB"], datavars[!(datavarsdf$C.Income %in% 6),"COB"])

#education
#doctorate
t.test(datavars[(datavarsdf$C.Education %in% 7),"CCBI"], datavars[!(datavarsdf$C.Education %in% 7),"CCBI"])
t.test(datavars[(datavarsdf$C.Education %in% 7),"CCB"], datavars[!(datavarsdf$C.Education %in% 7),"CCB"])
t.test(datavars[(datavarsdf$C.Education %in% 7),"COBI"], datavars[!(datavarsdf$C.Education %in% 7),"COBI"])
t.test(datavars[(datavarsdf$C.Education %in% 7),"COB"], datavars[!(datavarsdf$C.Education %in% 7),"COB"])
#university degree
t.test(datavars[(datavarsdf$C.Education %in% 6),"CCBI"], datavars[!(datavarsdf$C.Education %in% 6),"CCBI"])
t.test(datavars[(datavarsdf$C.Education %in% 6),"CCB"], datavars[!(datavarsdf$C.Education %in% 6),"CCB"])
t.test(datavars[(datavarsdf$C.Education %in% 6),"COBI"], datavars[!(datavarsdf$C.Education %in% 6),"COBI"])
t.test(datavars[(datavarsdf$C.Education %in% 6),"COB"], datavars[!(datavarsdf$C.Education %in% 6),"COB"])
#abitur
t.test(datavars[(datavarsdf$C.Education %in% 5),"CCBI"], datavars[!(datavarsdf$C.Education %in% 5),"CCBI"])
t.test(datavars[(datavarsdf$C.Education %in% 5),"CCB"], datavars[!(datavarsdf$C.Education %in% 5),"CCB"])
t.test(datavars[(datavarsdf$C.Education %in% 5),"COBI"], datavars[!(datavarsdf$C.Education %in% 5),"COBI"])
t.test(datavars[(datavarsdf$C.Education %in% 5),"COB"], datavars[!(datavarsdf$C.Education %in% 5),"COB"])
#ralschulabschluss
t.test(datavars[(datavarsdf$C.Education %in% 4),"CCBI"], datavars[!(datavarsdf$C.Education %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Education %in% 4),"CCB"], datavars[!(datavarsdf$C.Education %in% 4),"CCB"])
t.test(datavars[(datavarsdf$C.Education %in% 4),"COBI"], datavars[!(datavarsdf$C.Education %in% 4),"COBI"])
t.test(datavars[(datavarsdf$C.Education %in% 4),"COB"], datavars[!(datavarsdf$C.Education %in% 4),"COB"])
#hauptschulabschluss
t.test(datavars[(datavarsdf$C.Education %in% 3),"CCBI"], datavars[!(datavarsdf$C.Education %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Education %in% 3),"CCB"], datavars[!(datavarsdf$C.Education %in% 3),"CCB"])
t.test(datavars[(datavarsdf$C.Education %in% 3),"COBI"], datavars[!(datavarsdf$C.Education %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.Education %in% 3),"COB"], datavars[!(datavarsdf$C.Education %in% 3),"COB"])

#occupation
#fulltime
t.test(datavars[(datavarsdf$C.Occupation %in% 1),"CCBI"], datavars[!(datavarsdf$C.Occupation %in% 1),"CCBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 1),"CCB"], datavars[!(datavarsdf$C.Occupation %in% 1),"CCB"])
t.test(datavars[(datavarsdf$C.Occupation %in% 1),"COBI"], datavars[!(datavarsdf$C.Occupation %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 1),"COB"], datavars[!(datavarsdf$C.Occupation %in% 1),"COB"])
#parttime
t.test(datavars[(datavarsdf$C.Occupation %in% 2),"CCBI"], datavars[!(datavarsdf$C.Occupation %in% 2),"CCBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 2),"CCB"], datavars[!(datavarsdf$C.Occupation %in% 2),"CCB"])
t.test(datavars[(datavarsdf$C.Occupation %in% 2),"COBI"], datavars[!(datavarsdf$C.Occupation %in% 2),"COBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 2),"COB"], datavars[!(datavarsdf$C.Occupation %in% 2),"COB"])
#vocational training
t.test(datavars[(datavarsdf$C.Occupation %in% 3),"CCBI"], datavars[!(datavarsdf$C.Occupation %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 3),"CCB"], datavars[!(datavarsdf$C.Occupation %in% 3),"CCB"])
t.test(datavars[(datavarsdf$C.Occupation %in% 3),"COBI"], datavars[!(datavarsdf$C.Occupation %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 3),"COB"], datavars[!(datavarsdf$C.Occupation %in% 3),"COB"])
#university student
t.test(datavars[(datavarsdf$C.Occupation %in% 4),"CCBI"], datavars[!(datavarsdf$C.Occupation %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 4),"CCB"], datavars[!(datavarsdf$C.Occupation %in% 4),"CCB"])
t.test(datavars[(datavarsdf$C.Occupation %in% 4),"COBI"], datavars[!(datavarsdf$C.Occupation %in% 4),"COBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 4),"COB"], datavars[!(datavarsdf$C.Occupation %in% 4),"COB"])
#other student (none)
#not employed
t.test(datavars[(datavarsdf$C.Occupation %in% 6),"CCBI"], datavars[!(datavarsdf$C.Occupation %in% 6),"CCBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 6),"CCB"], datavars[!(datavarsdf$C.Occupation %in% 6),"CCB"])
t.test(datavars[(datavarsdf$C.Occupation %in% 6),"COBI"], datavars[!(datavarsdf$C.Occupation %in% 6),"COBI"])
t.test(datavars[(datavarsdf$C.Occupation %in% 6),"COB"], datavars[!(datavarsdf$C.Occupation %in% 6),"COB"])


#living situation!
#alone
t.test(datavars[(datavarsdf$C.Living %in% 1),"CCBI"], datavars[!(datavarsdf$C.Living %in% 1),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 1),"CCB"], datavars[!(datavarsdf$C.Living %in% 1),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 1),"COBI"], datavars[!(datavarsdf$C.Living %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 1),"COB"], datavars[!(datavarsdf$C.Living %in% 1),"COB"])
#zu zweit partner
t.test(datavars[(datavarsdf$C.Living %in% 2),"CCBI"], datavars[!(datavarsdf$C.Living %in% 2),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 2),"CCB"], datavars[!(datavarsdf$C.Living %in% 2),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 2),"COBI"], datavars[!(datavarsdf$C.Living %in% 2),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 2),"COB"], datavars[!(datavarsdf$C.Living %in% 2),"COB"])
#zu zweit familie
t.test(datavars[(datavarsdf$C.Living %in% 3),"CCBI"], datavars[!(datavarsdf$C.Living %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 3),"CCB"], datavars[!(datavarsdf$C.Living %in% 3),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 3),"COBI"], datavars[!(datavarsdf$C.Living %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 3),"COB"], datavars[!(datavarsdf$C.Living %in% 3),"COB"])
#zu mehr familie
t.test(datavars[(datavarsdf$C.Living %in% 4),"CCBI"], datavars[!(datavarsdf$C.Living %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 4),"CCB"], datavars[!(datavarsdf$C.Living %in% 4),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 4),"COBI"], datavars[!(datavarsdf$C.Living %in% 4),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 4),"COB"], datavars[!(datavarsdf$C.Living %in% 4),"COB"])
#zu zweit wg
t.test(datavars[(datavarsdf$C.Living %in% 5),"CCBI"], datavars[!(datavarsdf$C.Living %in% 5),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 5),"CCB"], datavars[!(datavarsdf$C.Living %in% 5),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 5),"COBI"], datavars[!(datavarsdf$C.Living %in% 5),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 5),"COB"], datavars[!(datavarsdf$C.Living %in% 5),"COB"])
#zu mehreren wg
t.test(datavars[(datavarsdf$C.Living %in% 6),"CCBI"], datavars[!(datavarsdf$C.Living %in% 6),"CCBI"])
t.test(datavars[(datavarsdf$C.Living %in% 6),"CCB"], datavars[!(datavarsdf$C.Living %in% 6),"CCB"])
t.test(datavars[(datavarsdf$C.Living %in% 6),"COBI"], datavars[!(datavarsdf$C.Living %in% 6),"COBI"])
t.test(datavars[(datavarsdf$C.Living %in% 6),"COB"], datavars[!(datavarsdf$C.Living %in% 6),"COB"])


#COVID-Questions!
#own risk, 3 is yes, 1 is no
t.test(datavars[(datavarsdf$C.CO.Own.Risk %in% 3),"COBI"], datavars[!(datavarsdf$C.CO.Own.Risk %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Risk %in% 3),"COB"], datavars[!(datavarsdfC.CO.Own.Risk %in% 3),"COB"])
t.test(datavars[(datavarsdf$C.CO.Own.Risk %in% 1),"COBI"], datavars[!(datavarsdf$C.CO.Own.Risk %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Risk %in% 1),"COB"], datavars[!(datavarsdfC.CO.Own.Risk %in% 1),"COB"])
#own infektion, 3 is yes, 1 is no
t.test(datavars[(datavarsdf$C.CO.Own.Infektion %in% 3),"COBI"], datavars[!(datavarsdf$C.CO.Own.Infektion %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Infektion %in% 3),"COB"], datavars[!(datavarsdfC.CO.Own.Infektion %in% 3),"COB"])
t.test(datavars[(datavarsdf$C.CO.Own.Infektion %in% 1),"COBI"], datavars[!(datavarsdf$C.CO.Own.Infektion %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Infektion %in% 1),"COB"], datavars[!(datavarsdfC.CO.Own.Infektion %in% 1),"COB"])
#own severity, 3 is yes, 1 is no
t.test(datavars[(datavarsdf$C.CO.Own.Severity %in% 3),"COBI"], datavars[!(datavarsdf$C.CO.Own.Severity %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Severity %in% 3),"COB"], datavars[!(datavarsdfC.CO.Own.Severity %in% 3),"COB"])
t.test(datavars[(datavarsdf$C.CO.Own.Severity %in% 1),"COBI"], datavars[!(datavarsdf$C.CO.Own.Severity %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Severity %in% 1),"COB"], datavars[!(datavarsdfC.CO.Own.Severity %in% 1),"COB"])
#living with someone at risk C.CO.Living.Risk
t.test(datavars[(datavarsdf$C.CO.Living.Risk %in% 3),"COBI"], datavars[!(datavarsdf$C.CO.Living.Risk %in% 3),"COBI"])
sd(datavars[(datavarsdf$C.CO.Living.Risk %in% 3),"COBI"], na.rm=TRUE)
sd(datavars[!(datavarsdf$C.CO.Living.Risk %in% 3),"COBI"], na.rm=TRUE)

t.test(datavars[(datavarsdf$C.CO.Living.Risk %in% 3),"COB"], datavars[!(datavarsdfC.CO.Living.Risk %in% 3),"COB"])
t.test(datavars[(datavarsdf$C.CO.Living.Risk %in% 1),"COBI"], datavars[!(datavarsdf$C.CO.Living.Risk %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Living.Risk %in% 1),"COB"], datavars[!(datavarsdfC.CO.Living.Risk %in% 1),"COB"])
#knowing others with infection - correlation CO.Others.Infection
cor.test(datavars[,"COBI"], datavars[,"CO.Others.Infection"])
t.test(datavars[(datavarsdf$CO.Others.Infection %in% 0),"COBI"], datavars[!(datavarsdf$CO.Others.Infection %in% 0),"COBI"])
t.test(datavars[(datavarsdf$CO.Others.Infection %in% 0),"COB"], datavars[!(datavarsdfCO.Others.Infection %in% 0),"COB"])
t.test(datavars[(datavarsdf$CO.Others.Infection %in% 5),"COBI"], datavars[!(datavarsdf$CO.Others.Infection %in% 5),"COBI"])
t.test(datavars[(datavarsdf$CO.Others.Infection %in% 5),"COB"], datavars[!(datavarsdfCO.Others.Infection %in% 5),"COB"])
#other's severity CO.Others.Severity
cor.test(datavars[,"COBI"], datavars[,"CO.Others.Severity"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 0),"COBI"], datavars[!(datavarsdf$CO.Others.Severity %in% 0),"COBI"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 0),"COB"], datavars[!(datavarsdfCO.Others.Severity %in% 0),"COB"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 1),"COBI"], datavars[!(datavarsdf$CO.Others.Severity %in% 1),"COBI"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 1),"COB"], datavars[!(datavarsdfCO.Others.Severity %in% 1),"COB"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 3),"COBI"], datavars[!(datavarsdf$CO.Others.Severity %in% 3),"COBI"])
t.test(datavars[(datavarsdf$CO.Others.Severity %in% 3),"COB"], datavars[!(datavarsdfCO.Others.Severity %in% 3),"COB"])
#occupational C.CO.Own.Ocuppation
t.test(datavars[(datavarsdf$C.CO.Own.Ocuppation %in% 3),"COBI"], datavars[!(datavarsdf$C.CO.Own.Ocuppation %in% 3),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Ocuppation %in% 3),"COB"], datavars[!(datavarsdfC.CO.Own.Ocuppation %in% 3),"COB"])
t.test(datavars[(datavarsdf$C.CO.Own.Ocuppation %in% 1),"COBI"], datavars[!(datavarsdf$C.CO.Own.Ocuppation %in% 1),"COBI"])
t.test(datavars[(datavarsdf$C.CO.Own.Ocuppation %in% 1),"COB"], datavars[!(datavarsdfC.CO.Own.Ocuppation %in% 1),"COB"])


#Diet
#Vegans
t.test(datavars[(datavarsdf$C.Diet %in% 1),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 1),"CCBI"])
sd(datavars[(datavarsdf$C.Diet %in% 1),"CCBI"])
sd(datavars[!(datavarsdf$C.Diet %in% 1),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 1),"CCB"], datavars[!(datavarsdfC.Diet %in% 1),"CCB"])
#Vegetarians
t.test(datavars[(datavarsdf$C.Diet %in% 2),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 2),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 2),"CCB"], datavars[!(datavarsdfC.Diet %in% 2),"CCB"])
#3
t.test(datavars[(datavarsdf$C.Diet %in% 3),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 3),"CCB"], datavars[!(datavarsdfC.Diet %in% 3),"CCB"])
#middleground
t.test(datavars[(datavarsdf$C.Diet %in% 4),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 4),"CCB"], datavars[!(datavarsdfC.Diet %in% 4),"CCB"])
#5
t.test(datavars[(datavarsdf$C.Diet %in% 5),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 5),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 5),"CCB"], datavars[!(datavarsdfC.Diet %in% 5),"CCB"])
#daily
t.test(datavars[(datavarsdf$C.Diet %in% 6),"CCBI"], datavars[!(datavarsdf$C.Diet %in% 6),"CCBI"])
t.test(datavars[(datavarsdf$C.Diet %in% 6),"CCB"], datavars[!(datavarsdfC.Diet %in% 6),"CCB"])

#CARS!
#Differences in Car Own and Behavioral Intention/Behavior?
t.test(datavars[(datavarsdf$C.Car.Own %in% 5),"CCBI"], datavars[!(datavarsdf$C.Car.Own %in% 5),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Own %in% 5),"CCB"], datavars[!(datavarsdf$C.Car.Own %in% 5),"CCB"])
sd(datavars[(datavarsdf$C.Car.Own %in% 5),"CCBI"])
sd(datavars[!(datavarsdf$C.Car.Own %in% 5),"CCBI"])

#ride along 2
t.test(datavars[(datavarsdf$C.Car.Use.Ride.Along %in% 2),"CCBI"], datavars[!(datavarsdf$C.Car.Use.Ride.Along %in% 2),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Use.Ride.Along %in% 2),"CCB"], datavars[!(datavarsdf$C.Car.Use.Ride.Along %in% 2),"CCB"])
#carshare3
t.test(datavars[(datavarsdf$C.Car.Use.Carshare %in% 3),"CCBI"], datavars[!(datavarsdf$C.Car.Use.Carshare %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Use.Carshare %in% 3),"CCB"], datavars[!(datavarsdf$C.Car.Use.Carshare %in% 3),"CCB"])
#access4
t.test(datavars[(datavarsdf$C.Car.Use.Access %in% 4),"CCBI"], datavars[!(datavarsdf$C.Car.Use.Access %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Use.Access %in% 4),"CCB"], datavars[!(datavarsdf$C.Car.Use.Access %in% 4),"CCB"])

#Driving distance: 1:5
#1
t.test(datavars[(datavarsdf$C.Car.Distance %in% 1),"CCBI"], datavars[!(datavarsdf$C.Car.Distance %in% 1),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Distance %in% 1),"CCB"], datavars[!(datavarsdf$C.Car.Distance %in% 1),"CCB"])
#2
t.test(datavars[(datavarsdf$C.Car.Distance %in% 2),"CCBI"], datavars[!(datavarsdf$C.Car.Distance %in% 2),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Distance %in% 2),"CCB"], datavars[!(datavarsdf$C.Car.Distance %in% 2),"CCB"])
#3
t.test(datavars[(datavarsdf$C.Car.Distance %in% 3),"CCBI"], datavars[!(datavarsdf$C.Car.Distance %in% 3),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Distance %in% 3),"CCB"], datavars[!(datavarsdf$C.Car.Distance %in% 3),"CCB"])
#4
t.test(datavars[(datavarsdf$C.Car.Distance %in% 4),"CCBI"], datavars[!(datavarsdf$C.Car.Distance %in% 4),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Distance %in% 4),"CCB"], datavars[!(datavarsdf$C.Car.Distance %in% 4),"CCB"])
#5
t.test(datavars[(datavarsdf$C.Car.Distance %in% 5),"CCBI"], datavars[!(datavarsdf$C.Car.Distance %in% 5),"CCBI"])
sd(datavars[(datavarsdf$C.Car.Distance %in% 5),"CCBI"])
sd(datavars[!(datavarsdf$C.Car.Distance %in% 5),"CCBI"])
t.test(datavars[(datavarsdf$C.Car.Distance %in% 5),"CCB"], datavars[!(datavarsdf$C.Car.Distance %in% 5),"CCB"])
sd(datavars[(datavarsdf$C.Car.Distance %in% 5),"CCB"], na.rm=TRUE)
sd(datavars[!(datavarsdf$C.Car.Distance %in% 5),"CCB"], na.rm=TRUE)
