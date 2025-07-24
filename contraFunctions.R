## Contrahedonic Functions Script
rm(list=ls())
## ---- Packages ----
library(tidyverse)
library(psych)
library(lme4)
library(lsmeans)
library(sjPlot)
library(reshape2)
library(corrplot)
library(pbkrtest)
library(lmerTest)
`%notin%` <- Negate(`%in%`)

##---- Data Load ----
setwd("/Users/gracehart/ccsLab/contrahedonic")
t1dat <- read.csv("t1_cleaned_070722_anon.csv")
t2dat <- read.csv("t2_cleaned_010722_anon.csv")
t3dat <- read.csv("t3_cleaned_011622_anon.csv")

##---- Data Prep ----

# 1. Filter out participants who didn't respond to any contrahedonic items
# 2. Add extra participants from t2 who ARE NOT already in t3

contra_vars <- c("feel_bad", "feel_neg_gen", "inc_bad_feelings", "inc_neg_gen", "keep_feel_bad",
                 "keep_feel_neg_gen", "stop_feel_good", "reduc_good_gen")

# filter t2 data 
contra_t2 <- t2dat %>% select(ID, contains(contra_vars)) %>% filter(rowSums(is.na(.)) < 32) 
contra_t2$time <- "T2"
colSums(!is.na(contra_t2)) 

# filter t3 data 
contra_t3 <- t3dat %>% select(ID, contains(contra_vars)) %>% filter(rowSums(is.na(.)) < 32) 
contra_t3$time <- "T3"
colSums(!is.na(contra_t3)) 

# add extra participants from t2 who ARE NOT already in t3 
contra_t2_add <- contra_t2 %>% filter(ID %notin% contra_t3$ID)
contra <- rbind(contra_t3, contra_t2_add)
dim(contra) # 137 participants total

# organize so items go restrict -> binge -> purge -> nssi 
contra <- contra %>% 
  relocate(contains("nssi")) %>% 
  relocate(contains("purge")) %>% 
  relocate(contains("binge")) %>% 
  relocate(contains("restrict")) %>% 
  relocate(ID)
colSums(!is.na(contra)) # 114 for restrict, 77 for binge, 36 for purge, and 66 for NSSI 

# gather 'main' dataset with other t2, t3 vars in addition to contrahedonic vars
t2_incontra <- t2dat %>% filter(ID %in% contra_t2_add$ID)
t3_incontra <- t3dat %>% filter(ID %in% contra_t3$ID)

setdiff(names(t2_incontra), names(t3_incontra)) 
setdiff(names(t3_incontra), names(t2_incontra))

# rename some columns so they match 
t3_incontra <- rename(t3_incontra, 
                      purge_month_times = purge_month, 
                      atn_check_EDEQ = EDEQ34,
                      gender_3_TEXT = gender_4_TEXT,
                      atn_check_tfeq = tfeq_test)

# get rid of the rest mismatches in column names - not important
t2_full_tomerge <- t2_incontra %>% select(!setdiff(names(t2_incontra), names(t3_incontra)))
t3_full_tomerge <- t3_incontra %>% select(!setdiff(names(t3_incontra), names(t2_incontra)))
setdiff(names(t2_full_tomerge), names(t3_full_tomerge)) 

fulldf <- rbind(t3_full_tomerge, t2_full_tomerge)

# clean up global environment 
rm(contra_t2, contra_t3, contra_t2_add, t3_full_tomerge, t2_full_tomerge, t2_incontra, t3_incontra)

# write to csv 
#write.csv(contra, "contra.csv", row.names = FALSE)

##---- demographics ----
# Race / ethnicity
#contra_demogs <- contra_demogs %>% mutate(
#  race_white = ifelse(grepl("1", contra_demogs$race), TRUE, FALSE),
#  race_black = ifelse(grepl("2", contra_demogs$race), TRUE, FALSE),
#  eth_hispanic = ifelse(grepl("3", contra_demogs$race), TRUE, FALSE),
#  race_asian = ifelse(grepl("4", contra_demogs$race), TRUE, FALSE),
#  race_native = ifelse(grepl("5", contra_demogs$race), TRUE, FALSE),
#  race_pacisl = ifelse(grepl("6", contra_demogs$race), TRUE, FALSE),
#  race_other = ifelse(grepl("6", contra_demogs$race), TRUE, FALSE),
#  race_biracial = ifelse(grepl("8", contra_demogs$race), TRUE, FALSE),
#  
#)

# Ensure consistent data types across datasets
t1dat <- t1dat %>% mutate(
  sex_birth = as.character(sex_birth),
  gender = as.character(gender),
  trans = as.character(trans),
  sexual_orientation = as.character(sexual_orientation),
  crush = as.character(crush),
  sexual_attraction = as.character(sexual_attraction),
  relationship_status = as.character(relationship_status),
  height_measured = as.character(height_measured),
  diagnosis = as.character(diagnosis)
)

t2dat <- t2dat %>% mutate(
  sex_birth = as.character(sex_birth),
  gender = as.character(gender),
  trans = as.character(trans),
  sexual_orientation = as.character(sexual_orientation),
  crush = as.character(crush),
  sexual_attraction = as.character(sexual_attraction),
  relationship_status = as.character(relationship_status),
  height_measured = as.character(height_measured),
  diagnosis = as.character(diagnosis)
)

t3dat <- t3dat %>% mutate(
  sex_birth = as.character(sex_birth),
  gender = as.character(gender),
  trans = as.character(trans),
  sexual_orientation = as.character(sexual_orientation),
  crush = as.character(crush),
  sexual_attraction = as.character(sexual_attraction),
  relationship_status = as.character(relationship_status),
  height_measured = as.character(height_measured),
  diagnosis = as.character(diagnosis)
)

# Combine data from t1dat, t2dat, and t3dat with a 'timepoint' column
contra_demogs <- bind_rows(
  t1dat %>% filter(ID %in% contra$ID) %>% mutate(timepoint = "t1"),
  t2dat %>% filter(ID %in% contra$ID) %>% mutate(timepoint = "t2"),
  t3dat %>% filter(ID %in% contra$ID) %>% mutate(timepoint = "t3")
)

# Keep only the most recent timepoint for each participant
contra_demogs <- contra_demogs %>%
  group_by(ID) %>%
  filter(timepoint == max(timepoint)) %>%
  ungroup()

# Add back the "race" column from t1
contra_demogs <- contra_demogs %>%
  left_join(t1dat %>% select(ID, race), by = "ID")

contra_demogs <- contra_demogs %>%
  rename(race = race.y) %>%
  select(-race.x)

# Add race and ethnicity columns using mutate
contra_race <- contra_demogs %>% mutate(
  race_white = ifelse(grepl("1", race), TRUE, FALSE),
  race_black = ifelse(grepl("2", race), TRUE, FALSE),
  eth_hispanic = ifelse(grepl("3", race), TRUE, FALSE),
  race_asian = ifelse(grepl("4", race), TRUE, FALSE),
  race_native = ifelse(grepl("5", race), TRUE, FALSE),
  race_pacisl = ifelse(grepl("6", race), TRUE, FALSE),
  race_other = ifelse(grepl("7", race), TRUE, FALSE),  # corrected to "7" for race_other
  race_biracial = ifelse(grepl("8", race), TRUE, FALSE)
)

contra_race %>% 
  select(contains("race_")) %>% 
  summary()

contra_race %>% 
  select(contains("eth_")) %>% 
  summary()

contra_race %>%
  summarise(
    pct_white = mean(race_white) * 100,
    pct_black = mean(race_black) * 100,
    pct_hispanic = mean(eth_hispanic) * 100,
    pct_asian = mean(race_asian) * 100,
    pct_native = mean(race_native) * 100,
    pct_pacisl = mean(race_pacisl) * 100,
    pct_other = mean(race_other) * 100,
    pct_biracial = mean(race_biracial) * 100
  )

# Age
contra_demogs %>% 
  filter(age_calculated > 10) %>% 
  select(age_calculated) %>% summary

contra_demogs %>%
  filter(age_calculated > 10) %>%
  summarise(
    mean_age = mean(age_calculated, na.rm = TRUE),
    sd_age = sd(age_calculated, na.rm = TRUE)
  )

# Sexual orientation
orientation_demogs <- contra_demogs %>% mutate(
  bisexual = ifelse(sexual_orientation == "Bisexual", TRUE, FALSE),
  straight = ifelse(sexual_orientation == "Straight", TRUE, FALSE),
  gay_lesbian = ifelse(sexual_orientation == "Gay/Lesbian", TRUE, FALSE),
  not_sure = ifelse(sexual_orientation == "NotSure", TRUE, FALSE),
  not_listed = ifelse(sexual_orientation == "NotListed", TRUE, FALSE)
)

orientation_demogs %>% 
  select(contains("bisexual"), contains("straight"), contains("gay_lesbian"),
         contains("not_sure"), contains("not_listed")) %>%
  summary()

orientation_demogs %>% 
  summarise(
    pct_bisexual = mean(bisexual) * 100,
    pct_straight = mean(straight) * 100,
    pct_gay_lesbian = mean(gay_lesbian) * 100,
    pct_not_sure = mean(not_sure) * 100,
    pct_not_listed = mean(not_listed) * 100
  )

# Gender
gender_demogs <- contra_demogs%>% mutate(
  cis_female = ifelse(gender == "Female" & trans == "No", TRUE, FALSE),
  cis_male = ifelse(gender == "Male" & trans == "No", TRUE, FALSE),
  trans_female = ifelse(gender == "Female" & trans == "Yes", TRUE, FALSE),
  trans_male = ifelse(gender == "Male" & trans == "Yes", TRUE, FALSE)
)

gender_demogs %>% 
  select(contains("cis_"), contains("trans_")) %>%
  summary()

gender_demogs %>% 
  summarise(
    pct_cis_female = mean(cis_female) * 100,
    pct_cis_male = mean(cis_male) * 100,
    pct_trans_female = mean(trans_female) * 100,
    pct_trans_male = mean(trans_male) * 100
  )

#BMI
contra_demogs <- contra_demogs %>%
  mutate(
    weight_measured = as.numeric(weight_measured),
    height_measured = as.numeric(height_measured)
  )

calculate_bmi <- function(weight_pounds, height_inches) {
  weight_kg <- weight_pounds * 0.453592
  height_m <- height_inches * 0.0254
  bmi <- weight_kg / (height_m ^ 2)
  
  return(bmi)
}

contra_demogs <- contra_demogs %>% mutate(
  BMI = calculate_bmi(weight_measured, height_measured)
)

mean_bmi <- mean(contra_demogs$BMI, na.rm = TRUE)
sd_bmi <- sd(contra_demogs$BMI, na.rm = TRUE)

mean_bmi
sd_bmi

## --- past month events ---
mean_restrict <- mean(fulldf$restrict_month, na.rm = TRUE)
sd_retrict <- sd(fulldf$restrict_month, na.rm = TRUE)
min_restrict <- min(fulldf$restrict_month, na.rm = TRUE)
max_restrict <- max(fulldf$restrict_month, na.rm = TRUE)

mean_binge <- mean(fulldf$binge_month, na.rm = TRUE)
sd_binge <- sd(fulldf$binge_month, na.rm = TRUE)
min_binge <- min(fulldf$binge_month, na.rm = TRUE)
max_binge <- max(fulldf$binge_month, na.rm = TRUE)

mean_purge <- mean(fulldf$purge_month, na.rm = TRUE)
sd_purge <- sd(fulldf$purge_month, na.rm = TRUE)
min_purge <- min(fulldf$purge_month, na.rm = TRUE)
max_purge <- max(fulldf$purge_month, na.rm = TRUE)

## --- average YEDEQ ---
edeq <- fulldf %>%
  select(matches("^EDEQ([1-9]|1[0-6]|3[0-9])$"), -matches("EDEQ34"))

edeq$edeq_scores <- rowMeans(edeq, na.rm = TRUE)
mean_edeq_scores <- mean(edeq$edeq_scores, na.rm = TRUE)
sd_edeq_scores <- sd(edeq$edeq_scores, na.rm = TRUE)

##---- endorsement ----

#how many participants endorsed at least 1 contra item
contra_any <- contra %>%
  mutate(contra_any = if_else(
    feel_bad_restrict > 0 |
      keep_feel_bad_restrict > 0 |
      feel_neg_gen_restrict > 0 |
      keep_feel_neg_gen_restrict > 0 |
      inc_bad_feelings_restrict > 0 |
      inc_neg_gen_restrict > 0 |
      stop_feel_good_restrict > 0 |
      reduc_good_gen_restrict > 0 |
      feel_bad_binge > 0 |
      keep_feel_bad_binge > 0 |
      feel_neg_gen_binge > 0 |
      keep_feel_neg_gen_binge > 0 |
      inc_bad_feelings_binge > 0 |
      inc_neg_gen_binge > 0 |
      stop_feel_good_binge > 0 |
      reduc_good_gen_binge > 0 |
      feel_bad_purge > 0 |
      keep_feel_bad_purge > 0 |
      feel_neg_gen_purge > 0 |
      keep_feel_neg_gen_purge > 0 |
      inc_bad_feelings_purge > 0 |
      inc_neg_gen_purge > 0 |
      stop_feel_good_purge > 0 |
      reduc_good_gen_purge > 0 |
      feel_bad_nssi > 0 |
      keep_feel_bad_nssi > 0 |
      feel_neg_gen_nssi > 0 |
      keep_feel_neg_gen_nssi > 0 |
      inc_bad_feelings_nssi > 0 |
      inc_neg_gen_nssi > 0 |
      stop_feel_good_nssi > 0 |
      reduc_good_gen_nssi > 0,
    1, 0
  ))

sum(contra_any$contra_any, na.rm = TRUE)

# Define pro items
pro_items <- c(
  "neg_gen_restrict", "neg_gen_binge", "neg_gen_nssi", "neg_gen_purge",
  "reduce_anxiety_restrict", "reduce_anxiety_binge", "reduce_anxiety_nssi", "reduce_anxiety_purge",
  "escapebad_restrict", "escapebad_binge", "escapebad_nssi", "escapebad_purge",
  "slowthoughts_restrict", "slowthoughts_binge", "slowthoughts_nssi", "slowthoughts_purge",
  "prevent_badfeel_restrict", "prevent_badfeel_binge", "prevent_badfeel_nssi", "prevent_badfeel_purge",
  "cope_restrict", "cope_binge", "cope_nssi", "cope_purge",
  "ground_restrict", "ground_binge", "ground_nssi", "ground_purge",
  "relax_restrict", "relax_binge", "relax_nssi", "relax_purge",
  "feelsomethingevenpain_restrict", "feelsomethingevenpain_binge", "feelsomethingevenpain_nssi", "feelsomethingevenpain_purge",
  "proud_restrict", "proud_binge", "proud_nssi", "proud_purge",
  "feelgood_restrict", "feelgood_binge", "feelgood_nssi", "feelgood_purge",
  "punish_restrict", "punish_binge", "punish_nssi", "punish_purge",
  "reaction_restrict", "reaction_binge", "reaction_nssi", "reaction_purge",
  "commun_desperation_restrict", "commun_desperation_binge", "commun_desperation_nssi", "commun_desperation_purge",
  "commun_bad_restrict", "commun_bad_binge", "commun_bad_nssi", "commun_bad_purge",
  "getnotice_restrict", "getnotice_binge", "getnotice_nssi", "getnotice_purge",
  "attention_restrict", "attention_binge", "attention_nssi", "attention_purge",
  "special_restrict", "special_binge", "special_nssi", "special_purge",
  "dosomething_alone_restrict", "dosomething_alone_binge", "dosomething_alone_nssi", "dosomething_alone_purge",
  "avoid_unpleasant_restrict", "avoid_unpleasant_binge", "avoid_unpleasant_nssi", "avoid_unpleasant_purge",
  "avoid_work_restrict", "avoid_work_binge", "avoid_work_nssi", "avoid_work_purge",
  "avoid_avoidothers_restrict", "avoid_avoidothers_binge", "avoid_avoidothers_nssi", "avoid_avoidothers_purge",
  "dosomething_bored_restrict", "dosomething_bored_binge", "dosomething_bored_nssi", "dosomething_bored_purge",
  "avoid_attention_restrict", "avoid_attention_binge", "avoid_attention_nssi", "avoid_attention_purge",
  "feel_strong_restrict", "feel_strong_binge", "feel_strong_nssi", "feel_strong_purge",
  "feel_cntrl_restrict", "feel_cntrl_binge", "feel_cntrl_nssi", "feel_cntrl_purge",
  "get_cntrl_situation_restrict", "get_cntrl_situation_binge", "get_cntrl_situation_nssi", "get_cntrl_situation_purge",
  "get_cntrl_body_restrict", "get_cntrl_body_binge", "get_cntrl_body_nssi", "get_cntrl_body_purge",
  "cntrl_feelings_restrict", "cntrl_feelings_binge", "cntrl_feelings_nssi", "cntrl_feelings_purge"
)

pro <- fulldf %>%
  select(ID, all_of(pro_items))

pro <- pro %>%
  mutate(pro_any = if_else(
    neg_gen_restrict > 0 |
      neg_gen_binge > 0 |
      neg_gen_nssi > 0 |
      neg_gen_purge > 0 |
      reduce_anxiety_restrict > 0 |
      reduce_anxiety_binge > 0 |
      reduce_anxiety_nssi > 0 |
      reduce_anxiety_purge > 0 |
      escapebad_restrict > 0 |
      escapebad_binge > 0 |
      escapebad_nssi > 0 |
      escapebad_purge > 0 |
      slowthoughts_restrict > 0 |
      slowthoughts_binge > 0 |
      slowthoughts_nssi > 0 |
      slowthoughts_purge > 0 |
      prevent_badfeel_restrict > 0 |
      prevent_badfeel_binge > 0 |
      prevent_badfeel_nssi > 0 |
      prevent_badfeel_purge > 0 |
      cope_restrict > 0 |
      cope_binge > 0 |
      cope_nssi > 0 |
      cope_purge > 0 |
      ground_restrict > 0 |
      ground_binge > 0 |
      ground_nssi > 0 |
      ground_purge > 0 |
      relax_restrict > 0 |
      relax_binge > 0 |
      relax_nssi > 0 |
      relax_purge > 0 |
      feelsomethingevenpain_restrict > 0 |
      feelsomethingevenpain_binge > 0 |
      feelsomethingevenpain_nssi > 0 |
      feelsomethingevenpain_purge > 0 |
      proud_restrict > 0 |
      proud_binge > 0 |
      proud_nssi > 0 |
      proud_purge > 0 |
      feelgood_restrict > 0 |
      feelgood_binge > 0 |
      feelgood_nssi > 0 |
      feelgood_purge > 0 |
      punish_restrict > 0 |
      punish_binge > 0 |
      punish_nssi > 0 |
      punish_purge > 0 |
      reaction_restrict > 0 |
      reaction_binge > 0 |
      reaction_nssi > 0 |
      reaction_purge > 0 |
      commun_desperation_restrict > 0 |
      commun_desperation_binge > 0 |
      commun_desperation_nssi > 0 |
      commun_desperation_purge > 0 |
      commun_bad_restrict > 0 |
      commun_bad_binge > 0 |
      commun_bad_nssi > 0 |
      commun_bad_purge > 0 |
      getnotice_restrict > 0 |
      getnotice_binge > 0 |
      getnotice_nssi > 0 |
      getnotice_purge > 0 |
      attention_restrict > 0 |
      attention_binge > 0 |
      attention_nssi > 0 |
      attention_purge > 0 |
      special_restrict > 0 |
      special_binge > 0 |
      special_nssi > 0 |
      special_purge > 0 |
      dosomething_alone_restrict > 0 |
      dosomething_alone_binge > 0 |
      dosomething_alone_nssi > 0 |
      dosomething_alone_purge > 0 |
      avoid_unpleasant_restrict > 0 |
      avoid_unpleasant_binge > 0 |
      avoid_unpleasant_nssi > 0 |
      avoid_unpleasant_purge > 0 |
      avoid_work_restrict > 0 |
      avoid_work_binge > 0 |
      avoid_work_nssi > 0 |
      avoid_work_purge > 0 |
      avoid_avoidothers_restrict > 0 |
      avoid_avoidothers_binge > 0 |
      avoid_avoidothers_nssi > 0 |
      avoid_avoidothers_purge > 0 |
      dosomething_bored_restrict > 0 |
      dosomething_bored_binge > 0 |
      dosomething_bored_nssi > 0 |
      dosomething_bored_purge > 0 |
      avoid_attention_restrict > 0 |
      avoid_attention_binge > 0 |
      avoid_attention_nssi > 0 |
      avoid_attention_purge > 0 |
      feel_strong_restrict > 0 |
      feel_strong_binge > 0 |
      feel_strong_nssi > 0 |
      feel_strong_purge > 0 |
      feel_cntrl_restrict > 0 |
      feel_cntrl_binge > 0 |
      feel_cntrl_nssi > 0 |
      feel_cntrl_purge > 0 |
      get_cntrl_situation_restrict > 0 |
      get_cntrl_situation_binge > 0 |
      get_cntrl_situation_nssi > 0 |
      get_cntrl_situation_purge > 0 |
      get_cntrl_body_restrict > 0 |
      get_cntrl_body_binge > 0 |
      get_cntrl_body_nssi > 0 |
      get_cntrl_body_purge > 0 |
      cntrl_feelings_restrict > 0 |
      cntrl_feelings_binge > 0 |
      cntrl_feelings_nssi > 0 |
      cntrl_feelings_purge > 0,
    1, 0
  ))

sum(pro$pro_any, na.rm = TRUE)

##---- Aim A: Explore the presence of contra-hedonic functions of NSSI and ED behaviors ----
##---- t-tests ----
describe(contra)

tres <- contra %>% # create a new dataframe called "tres" by pulling from the existing dataframe "contra"...
  select(-c(ID, time)) %>% # ...every column except ID and time.
  map_df(~ broom::tidy(t.test(., mu = 0, alternative = "two.sided")), .id = 'var') #map_daf applies a function (in this case, broom::tidy(t.test...), which applies a t test
                                                                                   #stores the result in a tidy dataframe) to each column. mu = 0 sets the null hypothesis
                                                                                   #(i.e., that the true mean of the sample = 0)
print(tres, n = 32) # result: all significantly different than zero 
# var = the variable (item/behavior pair) under examination
# estimate = sample mean of the variable; that is, the mean of all responses to the variable in the current sample
# statistic = t-statistic
# p.value = p value
# parameter = degrees of freedom (i.e., number of observations for that variable, minus 1)
# conf.low = lower bound of the 95% confidence interval for the mean
# conf.high = upper bound of the 95% confidence interval for the mean
# method = one sample t test
# alternative = two.sided refers to us having run a two sided t test, which tests whether the means being tested are different from one another without specifying in which 
#               direction. In our case, we are testing whether the mean of each variable is different from 0

##---- Wrangle Data for Viz ----

## WRANGLE DATA TO STANDARD LONG FORM 
n_participants <- dim(contra)[1] # saves the number of rows in contra to n_participants; dim returns the dimensions of contra (first number is rows, second is columns). [1] takes the first number. 
n_obs_bx <- n_participants * 8 # saves the total observations to n_obs_bx; n_participants * 8 because there are 8 contra functions

melted_contra <- contra %>% select(-time) %>% melt(., id.vars = "ID") # takes all columns in contra except "time" and melts (i.e., reshapes into long format) based on ID

# add var for behavior (creates a new column called "behavior", which holds restrict/binge/purge/nssi for n_obs_bx number of rows, one after another, in this order) 
melted_contra$behavior <- c(rep("restrict", n_obs_bx), rep("binge", n_obs_bx), 
                            rep("purge", n_obs_bx), rep("nssi", n_obs_bx))

# add var for function 
contra_vars_v2 <- c("make self feel bad", "keep feeling bad", "make self angry, sad, etc.", # assigns functions to variables
                    "keep feeling angry, sad, etc.", "increase bad feelings", 
                    "increase anger, sadness, etc.", "stop feeling good", "feel less happy, good, etc")
melted_contra$func <- c(rep(rep(contra_vars_v2, each = n_participants), 4)) # repeats each function (8) exactly n_participants (137) times in a row, and repeats this whole sequence 4 times (once for each behavior)

# set structure for melted df (assign categorical variables as factors so that R "knows" they are categorical)
melted_contra$behavior <- factor(melted_contra$behavior, levels = c("restrict", "binge", "purge", "nssi")) # converts behavior column to a factor
melted_contra$func <- factor(melted_contra$func) # converts func column to a factor

## WRANGLE DATA FOR STACKED BAR CHART 
stacked <- melted_contra %>% 
  group_by(variable) %>% 
  count(value) %>% 
  filter(!is.na(value)) 
stacked_ungroup <- as.data.frame(stacked)
stacked_ungroup <- stacked_ungroup %>% add_row(variable = "stop_feel_good_purge", value = 2, n = 0, .before = 91)
stacked_ungroup$behavior <- c(rep("restrict", 32), rep("binge", 32), rep("purge", 32), rep("nssi", 32))
stacked_ungroup$func <- c(rep(rep(contra_vars_v2, each = 4), 4))

# set structure of variables 
stacked_ungroup$value <- factor(stacked_ungroup$value, levels = c("0", "1", "2", "3"), 
                                labels = c("Never", "Rarely", "Sometimes", "Often"))
stacked_ungroup$behavior <- factor(stacked_ungroup$behavior, levels = c("restrict", "binge", "purge", "nssi"))
stacked_ungroup$func <- factor(stacked_ungroup$func)

# add percent for aes text labels
stacked_toplot <- stacked_ungroup %>% group_by(variable) %>% mutate(perc = round(n / sum(n) * 100, 1))
stacked_toplot$perc <- ifelse(stacked_toplot$perc == 0, NA, stacked_toplot$perc)

##---- Stacked Bar Chart ----

# stacked bar chart 
ggplot(stacked_toplot, aes(x = behavior, y = perc, fill = value)) + 
  geom_bar(position = "stack", stat = "identity") + 
  facet_wrap(~func, nrow = 2) + 
  scale_fill_brewer(palette = "Paired") + 
  theme_bw(base_size = 18) + 
  geom_text(aes(x = behavior, label = paste0(perc, '%')), colour = 'black', 
            position = position_stack(vjust = 0.5), size = 4) + 
  labs(x = 'Behavior', y = 'Percent', fill = 'Frequency') + 
  theme(
    panel.background = element_rect(fill = 'transparent'), 
    plot.background = element_rect(fill = 'transparent', color = NA), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = 'transparent'), 
    legend.box.background = element_rect(fill = 'transparent'),
    legend.text = element_text(size = 12),       # legend text size
    legend.title = element_text(size = 12),      # legend title size
    legend.key.size = unit(0.25, "lines"),       # box around each legend item size
    legend.key.height = unit(.6, "cm"),  # Make legend color boxes longer
    legend.key.width = unit(.3, "cm"),  # Make legen color boxes wider
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15, angle = 45, hjust = 1),  # Rotate x-axis labels
    strip.text = element_text(size = 13),  # facet title size
    plot.margin = margin(5, 5, 5, 5)  # Add margin around the entire plot
  )

ggsave("/Users/gracehart/ccsLab/contrahedonic/figures/2025.7.15_stackedBar.pdf",
       width = 14, height = 8)

# stacked bar chart - rotated
ggplot(stacked_toplot, aes(x = behavior, y = perc, fill = value)) + 
  geom_bar(position = "stack", stat = "identity") + 
  facet_wrap(~func, ncol = 1) +
  scale_fill_brewer(palette = "Paired") + 
  theme_bw(base_size = 18) + 
  geom_text(aes(label = paste0(perc, '%')), colour = 'black', 
            position = position_stack(vjust = 0.5), size = 4) + 
  labs(x = 'Behavior', y = 'Percent', fill = 'Frequency') + 
  theme(
    panel.background = element_rect(fill = 'transparent'),
    panel.spacing = unit(1, "lines"),
    plot.background = element_rect(fill = 'transparent', color = NA), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = 'transparent'), 
    legend.box.background = element_rect(fill = 'transparent'),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15),
    strip.text = element_text(size = 13),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_flip()

#ggsave("2025.6.25_rotatedStackedBar.png", plot = last_plot(), width = 8, height = 12, dpi = 500)

#ggsave("/Users/gracehart/ccsLab/contrahedonic/2025.6.25_rotatedStackedBar.pdf",
#       width = 14, height = 8)

##---- Aim b: Test for potential differences in the level of endorsement of contra-hedonic functions across behaviors ----
##---- 8 Mixed Models ----
## Modeling how level of endorsement of each function differs across behaviors, adjusting for differences between participants

# 1. To make yourself feel bad
feel_bad <- contra %>% select(ID, contains("feel_bad")) %>% select(-contains("keep")) # create a new dataframe that takes, from the dataframe
#                                                                                     "contra", columns by ID that contain "feel_bad" in the
#                                                                                     tile, but not if it also has "keep" in the title

melted_feel_bad <- melt(feel_bad, id.vars = "ID") # convert the dataframe into long format (1 row per response, rather than 1 row per ID)

feel_bad_lmer <- lmer(value ~ variable + (1 | ID), data = melted_feel_bad) # fits a lmem predicting endorsement rating ("value", 0-3) from the specific
#                                                                          behavior/item pair ("variable"; independent/predictor variable/fixed effect)
#                                                                          accounting for repeated measures from the same participant (random intercept 
#                                                                          for each "ID") since each participant may endorse multiple behaviors

tab_model(feel_bad_lmer) # creates table of lmem's output
anova(feel_bad_lmer, type = 2) # performs a type II anova on the lmem, testing whether overall fixed effect of "variable" is significant and to explore if
#                              post-hoc tests are warranted
lsmeans::lsmeans(feel_bad_lmer, pairwise ~ variable, adjust = "tukey") # estimates the marginal (i.e., adjusted) means of the outcome variable (endorsement)
#                                                                      for each level (i.e., nssi/restrict/binge/purge) of a predictor variable (i.e., behavior 
#                                                                      type), after accounting for other variables in the model. Then performs post-hoc pairwise 
#                                                                      comparisons between the levels of "variable' w/ Tukey correction for multiple comparisons

nrow(melted_feel_bad)                          # number of rows in original data
nobs(feel_bad_lmer)                            # number of observations used in model
nrow(melted_feel_bad) - nobs(feel_bad_lmer)    # number of rows dropped

# 2. To make yourself feel angry, sad, lonely, anxious, etc.
feel_neg_gen <- contra %>% select(ID, contains("feel_neg_gen")) %>% select(-contains("keep"))
melted_feel_neg_gen <- melt(feel_neg_gen, id.vars = "ID")
feel_neg_gen_lmer <- lmer(value ~ variable + (1 | ID), data = melted_feel_neg_gen)
tab_model(feel_neg_gen_lmer)
anova(feel_neg_gen_lmer, type = 2)
lsmeans::lsmeans(feel_neg_gen_lmer, pairwise ~ variable, adjust = "tukey")

# 3. To increase bad feelings
inc_bad_feelings <- contra %>% select(ID, contains("inc_bad_feelings")) %>% select(-contains("keep"))
#inc_bad_feelings <- subset(t3dat[, c(18, grep("inc_bad_feelings", colnames(t3dat)))])
melted_inc_bad_feelings <- melt(inc_bad_feelings, id.vars = "ID")
inc_bad_feelings_lmer <- lmer(value ~ variable + (1 | ID), data = melted_inc_bad_feelings)
tab_model(inc_bad_feelings_lmer)
anova(inc_bad_feelings_lmer, type = 2)
lsmeans::lsmeans(inc_bad_feelings_lmer, pairwise ~ variable, adjust = "tukey")

# 4. To increase feelings of anger, sadness, loneliness, anxiety, etc.
inc_neg_gen <- contra %>% select(ID, contains("inc_neg_gen")) %>% select(-contains("keep"))
#inc_neg_gen <- subset(t3dat[, c(18, grep("inc_neg_gen", colnames(t3dat)))])
melted_inc_neg_gen <- melt(inc_neg_gen, id.vars = "ID")
inc_neg_gen_lmer <- lmer(value ~ variable + (1 | ID), data = melted_inc_neg_gen)
tab_model(inc_neg_gen_lmer)
anova(inc_neg_gen_lmer, type = 2)
lsmeans::lsmeans(inc_neg_gen_lmer, pairwise ~ variable, adjust = "tukey")

# 5. To keep feeling bad
#keep_feel_bad <- contra %>% select(ID, contains("keep_feel_bad")) %>% select(-contains("keep"))
#keep_feel_bad <- subset(t3dat[, c(18, grep("keep_feel_bad", colnames(t3dat)))])
keep_feel_bad <- contra %>% select(ID, contains("keep_feel_bad"))
melted_keep_feel_bad <- melt(keep_feel_bad, id.vars = "ID")
keep_feel_bad_lmer <- lmer(value ~ variable + (1 | ID), data = melted_keep_feel_bad)
tab_model(keep_feel_bad_lmer)
anova(keep_feel_bad_lmer, type = 2)
lsmeans::lsmeans(keep_feel_bad_lmer, pairwise ~ variable, adjust = "tukey")

# 6. To feel less happy, good, or proud of yourself
feel_less_happy <- contra %>% select(ID, contains("reduc_good_gen"))
#feel_less_happy <- subset(t3dat[, c(18, grep("reduc_good_gen", colnames(t3dat)))])
melted_reduc_good_gen <- melt(feel_less_happy, id.vars = "ID")
feel_less_happy_lmer <- lmer(value ~ variable + (1 | ID), data = melted_reduc_good_gen)
tab_model(feel_less_happy_lmer)
anova(feel_less_happy_lmer, type = 2)
lsmeans::lsmeans(feel_less_happy_lmer, pairwise ~ variable, adjust = "tukey")

# 7. To stop feeling good
stop_feel_good <- contra %>% select(ID, contains("stop_feel_good"))
#stop_feel_good <- subset(t3dat[, c(18, grep("stop_feel_good", colnames(t3dat)))])
melted_stop_feel_good <- melt(stop_feel_good, id.vars = "ID")
stop_feel_good_lmer <- lmer(value ~ variable + (1 | ID), data = melted_stop_feel_good)
tab_model(stop_feel_good_lmer)
anova(stop_feel_good_lmer, type = 2)
lsmeans::lsmeans(stop_feel_good_lmer, pairwise ~ variable, adjust = "tukey")

# 8. To keep feeling angry, sad, lonely, anxious, etc.
keep_feel_angry <- contra %>% select(ID, contains("keep_feel_neg_gen"))
#stop_feel_good <- subset(t3dat[, c(18, grep("stop_feel_good", colnames(t3dat)))])
melted_keep_feel_angry <- melt(keep_feel_angry, id.vars = "ID")
keep_feel_angry_lmer <- lmer(value ~ variable + (1 | ID), data = melted_keep_feel_angry)
tab_model(keep_feel_angry_lmer)
anova(keep_feel_angry_lmer, type = 2)
lsmeans::lsmeans(keep_feel_angry_lmer, pairwise ~ variable, adjust = "tukey")

sum(is.na(melted_contra$value))

##---- Visualize distributions ----
setwd("/Users/gracehart/ccsLab/contrahedonic/")
ggplot(melted_contra, aes(x = value)) + 
  geom_bar() + 
  facet_wrap(~ variable) + 
  scale_x_discrete()
#ggsave("2025.7.2_contrahedonic_distributions.pdf")

##---- Visualize Means ----
melted_contra$behavior <- c(rep("restrict", 1096), rep("binge", 1096), rep("purge", 1096), rep("NSSI", 1096))

restmean <- ggplot(subset(melted_contra, behavior == 'restrict'), aes(x = variable, y = value)) + 
  geom_bar(stat = "summary", fun = "mean") + 
  ggtitle("Restrictive Eating") +
  scale_x_discrete(labels = melted_contra) +
  ylim(0, 3)

bingemean <- ggplot(subset(melted_contra, behavior == 'binge'), aes(x = variable, y = value)) + 
  geom_bar(stat = "summary", fun = "mean") + 
  scale_x_discrete(labels = melted_contra) + 
  ggtitle("Binge Eating") +
  ylim(0, 3)

purgemean <- ggplot(subset(melted_contra, behavior == 'purge'), aes(x = variable, y = value)) + 
  geom_bar(stat = "summary", fun = "mean")+ 
  scale_x_discrete(labels = melted_contra)+ 
  ggtitle("Compensatory Behaviors [i.e., purging]") +
  ylim(0, 3)

nssimean <- ggplot(subset(melted_contra, behavior == 'NSSI'), aes(x = variable, y = value)) + 
  geom_bar(stat = "summary", fun = "mean")+ 
  scale_x_discrete(labels = melted_contra)+ 
  ggtitle("NSSI") +
  ylim(0, 3)

restmean <- restmean + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
bingemean <- bingemean + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
purgemean <- purgemean + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
nssimean <- nssimean + theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))

##---- Aim C: Examine the relationship between contra-hedonic and pro-hedonic functions of these behaviors ----
##---- Correlational Analyses ----
# calculate srs_total2 to confirm if it matches srs_total
# extract srs items into matrix, do NOT inlcude srs_9
srs_items <- cbind(
  fulldf$SRS_1, fulldf$SRS_2, fulldf$SRS_3,
  fulldf$SRS_4, fulldf$SRS_5, fulldf$SRS_6,
  fulldf$SRS_7, fulldf$SRS_8
)
# compute row means while ignoring NAs
srs_avg <- rowMeans(srs_items, na.rm = TRUE)

# store result in a new column in the original dataframe
fulldf$srs_total2 <- srs_avg

corDat <- fulldf %>% 
  filter(ID %in% contra$ID) %>% 
  select(contains(c("_ANR", "_APR", "_SNR", "_SPR")),
         restrict_3months_tim, binge_3months_times, purge_3months_times, NSSI_3months_times,
         contains(c("punish")), srs_total2, 
         names(contra)[-34])

# restrict - create a new dataframe that contains pairwise correlations between items whose columns contain "restrict" in the title, and srs_total2
restrict_cor <- corDat %>% 
  select("srs_total2", contains("restrict")) %>% 
  cor(., use = "pairwise.complete.obs") # computes Pearson correlation coefficients using only rows where both variables in that pair are not missing

restrict_pvals <- corDat %>% # compute p values for items whose columns contain "restrict" in the title, and srs_total2
  select("srs_total2", contains("restrict")) %>% 
  cor.mtest(., conf.level = 0.95) # at the 95% confidence level

restrict_pvals_vec <- as.vector(restrict_pvals$p) # flatten original p-value matrix

restrict_pvals_adj_vec <- p.adjust(restrict_pvals_vec, method = "fdr") # adjust for multiple comparisons using Benjamini-Hochberg (i.e. fdr)

restrict_pvals_adj_mat <- matrix(restrict_pvals_adj_vec, # reshape back into matrix
                        nrow = nrow(restrict_pvals$p),
                        ncol = ncol(restrict_pvals$p),
                        dimnames = dimnames(restrict_pvals$p))  # preserves row/col names

# Plot
#pdf("/Users/gracehart/ccsLab/contrahedonic/2025.7.2_restrict_cor.pdf", width = 10, height = 10)
corrplot(restrict_cor, method = "color", 
         addCoef.col = "gray30", 
         insig = "blank", 
         tl.col = "black",
         tl.cex = 1.5,
         number.cex = 1, 
         p.mat = restrict_pvals_adj_mat, 
         bg = "lightgrey",
         tl.srt = 75,
         pch.cex = 1.2, 
         cl.pos = "b")
dev.off()

# Define more interpretable labels
labels <- c(
  "Self-criticism", 
  "ANR", 
  "APR", 
  "SNR", 
  "SPR", 
  "Frequency (3mo)", 
  "Self-punishment", 
  "Feel bad", 
  "Keep feeling bad", 
  "Feel angry, etc.", 
  "Keep feeling angry, etc.", 
  "Inc. bad feelings", 
  "Inc. feelings of anger, etc.", 
  "Stop feeling good", 
  "Feel less happy, etc."
)

# apply labels
rownames(restrict_cor) <- labels
colnames(restrict_cor) <- labels
rownames(restrict_pvals_adj_mat) <- labels
colnames(restrict_pvals_adj_mat) <- labels

# set new order
new_order <- c(
  "ANR", 
  "APR", 
  "SNR", 
  "SPR", 
  "Frequency (3mo)", 
  "Self-punishment",
  "Self-criticism", 
  "Feel bad", 
  "Keep feeling bad", 
  "Feel angry, etc.", 
  "Keep feeling angry, etc.", 
  "Inc. bad feelings", 
  "Inc. feelings of anger, etc.", 
  "Stop feeling good", 
  "Feel less happy, etc."
)

# Apply reordering of correlation matrix and p-value matrix
restrict_cor <- restrict_cor[new_order, new_order]
restrict_pvals_adj_mat <- restrict_pvals_adj_mat[new_order, new_order]

# Set row and column names again
rownames(restrict_cor) <- new_order
colnames(restrict_cor) <- new_order
rownames(restrict_pvals_adj_mat) <- new_order
colnames(restrict_pvals_adj_mat) <- new_order

# Plot
#pdf("/Users/gracehart/ccsLab/contrahedonic/2025.7.2_restrict_cor.pdf", width = 10, height = 10)
corrplot(restrict_cor, method = "color", 
         addCoef.col = "gray30", 
         insig = "blank", 
         tl.col = "black",
         tl.cex = 1.5,
         number.cex = 1, 
         p.mat = restrict_pvals_adj_mat, 
         bg = "lightgrey",
         tl.srt = 75,
         pch.cex = 1.2, 
         cl.pos = "b")
dev.off()

# binge
binge_cor <- corDat %>% 
  select("srs_total2", contains("binge")) %>% 
  cor(., use = "pairwise.complete.obs")

binge_pvals <- corDat %>% 
  select("srs_total2", contains("binge")) %>% 
  cor.mtest(., conf.level = 0.95)

binge_pvals_vec <- as.vector(binge_pvals$p) # flatten original p-value matrix

binge_pvals_adj_vec <- p.adjust(binge_pvals_vec, method = "fdr") # adjust for multiple comparisons using Benjamini-Hochberg (i.e. fdr)

binge_pvals_adj_mat <- matrix(binge_pvals_adj_vec, # reshape back into matrix
                                 nrow = nrow(binge_pvals$p),
                                 ncol = ncol(binge_pvals$p),
                                 dimnames = dimnames(binge_pvals$p))  # preserves row/col names

# Apply labels
rownames(binge_cor) <- labels
colnames(binge_cor) <- labels
rownames(binge_pvals_adj_mat) <- labels
colnames(binge_pvals_adj_mat) <- labels

# Apply reordering of correlation matrix and p-value matrix
binge_cor <- binge_cor[new_order, new_order]
binge_pvals_adj_mat <- binge_pvals_adj_mat[new_order, new_order]

# Set row and column names again
rownames(binge_cor) <- new_order
colnames(binge_cor) <- new_order
rownames(binge_pvals_adj_mat) <- new_order
colnames(binge_pvals_adj_mat) <- new_order

# Plot
#pdf("/Users/gracehart/ccsLab/contrahedonic/2025.7.2_binge_cor.pdf", width = 10, height = 10)
corrplot(binge_cor, method = "color", 
         addCoef.col = "gray30", 
         insig = "blank", 
         tl.col = "black",
         tl.cex = 1.5,
         number.cex = 1, 
         p.mat = binge_pvals_adj_mat, 
         bg = "lightgrey",
         tl.srt = 75,
         pch.cex = 1.2, 
         cl.pos = "b")
dev.off()

# purge
purge_cor <- corDat %>% 
  select("srs_total2", contains("purge")) %>% 
  cor(., use = "pairwise.complete.obs")

purge_pvals <- corDat %>% 
  select("srs_total2", contains("purge")) %>% 
  cor.mtest(., conf.level = 0.95)

purge_pvals_vec <- as.vector(purge_pvals$p) # flatten original p-value matrix

purge_pvals_adj_vec <- p.adjust(purge_pvals_vec, method = "fdr") # adjust for multiple comparisons using Benjamini-Hochberg (i.e. fdr)

purge_pvals_adj_mat <- matrix(purge_pvals_adj_vec, # reshape back into matrix
                                 nrow = nrow(purge_pvals$p),
                                 ncol = ncol(purge_pvals$p),
                                 dimnames = dimnames(purge_pvals$p))  # preserves row/col names

# Apply labels
rownames(purge_cor) <- labels
colnames(purge_cor) <- labels
rownames(purge_pvals_adj_mat) <- labels
colnames(purge_pvals_adj_mat) <- labels

# Apply reordering of correlation matrix and p-value matrix
purge_cor <- purge_cor[new_order, new_order]
purge_pvals_adj_mat <- purge_pvals_adj_mat[new_order, new_order]

# Set row and column names again
rownames(purge_cor) <- new_order
colnames(purge_cor) <- new_order
rownames(purge_pvals_adj_mat) <- new_order
colnames(purge_pvals_adj_mat) <- new_order

# Plot
#pdf("/Users/gracehart/ccsLab/contrahedonic/2025.7.2_purge_cor.pdf", width = 10, height = 10)
corrplot(purge_cor, method = "color", 
         addCoef.col = "gray30", 
         insig = "blank", 
         tl.col = "black",
         tl.cex = 1.5,
         number.cex = 1, 
         p.mat = purge_pvals_adj_mat, 
         bg = "lightgrey",
         tl.srt = 75,
         pch.cex = 1.2, 
         cl.pos = "b")
dev.off()

# nssi
nssi_cor <- corDat %>% 
  select("srs_total2", contains("nssi")) %>% 
  cor(., use = "pairwise.complete.obs")

nssi_pvals <- corDat %>% 
  select("srs_total2", contains("nssi")) %>% 
  cor.mtest(., conf.level = 0.95)

nssi_pvals_vec <- as.vector(nssi_pvals$p) # flatten original p-value matrix

nssi_pvals_adj_vec <- p.adjust(nssi_pvals_vec, method = "fdr") # adjust for multiple comparisons using Benjamini-Hochberg (i.e. fdr)

nssi_pvals_adj_mat <- matrix(nssi_pvals_adj_vec, # reshape back into matrix
                              nrow = nrow(nssi_pvals$p),
                              ncol = ncol(nssi_pvals$p),
                              dimnames = dimnames(nssi_pvals$p))  # preserves row/col names

# Apply labels
rownames(nssi_cor) <- labels
colnames(nssi_cor) <- labels
rownames(nssi_pvals_adj_mat) <- labels
colnames(nssi_pvals_adj_mat) <- labels

# Apply reordering of correlation matrix and p-value matrix
nssi_cor <- nssi_cor[new_order, new_order]
nssi_pvals_adj_mat <- nssi_pvals_adj_mat[new_order, new_order]

# Set row and column names again
rownames(nssi_cor) <- new_order
colnames(nssi_cor) <- new_order
rownames(nssi_pvals_adj_mat) <- new_order
colnames(nssi_pvals_adj_mat) <- new_order

# Plot
#pdf("/Users/gracehart/ccsLab/contrahedonic/2025.7.2_nssi_cor.pdf", width = 10, height = 10)
corrplot(nssi_cor, method = "color", 
         addCoef.col = "gray30", 
         insig = "blank", 
         tl.col = "black",
         tl.cex = 1.5,
         number.cex = 1, 
         p.mat = nssi_pvals_adj_mat, 
         bg = "lightgrey",
         tl.srt = 75,
         pch.cex = 1.2, 
         cl.pos = "b")
dev.off()

## Psychometric properties
# Youth EDEQ
edeqAlpha <- psych::alpha(edeq %>% select(-edeq_scores), check.keys = TRUE)
print(edeqAlpha)                

# SRS - do NOT include srs_9!
srs <- contra_demogs %>%
  select("SRS_1","SRS_2","SRS_3","SRS_4","SRS_5","SRS_6","SRS_7","SRS_8")

srsAlpha <- psych::alpha(srs, check.keys = TRUE)
print(srsAlpha)

# FAMB
famb_alpha <- cbind(pro, contra)

# Restrictive
fambRestrict <- famb_alpha %>%
  select(ends_with("_restrict"))

fambRestrict <- fambRestrict %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .)) # replace NAs with imputed means

fambRestrictAlpha <- psych::alpha(fambRestrict, check.keys = TRUE)
print(fambRestrictAlpha)

#sapply(fambRestrict, function(x) length(unique(x))) # to check warning message; there are at 5 unique values in each item, so it is fine to proceed

# Binge
fambBinge <- famb_alpha %>%
  select(ends_with("_binge"))

fambBinge[] <- lapply(fambBinge, function(x) if(is.factor(x) | is.character(x)) as.numeric(as.character(x)) else x)

fambBinge <- fambBinge %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .)) # replace NAs with imputed means

fambBingeAlpha <- psych::alpha(fambBinge, check.keys = TRUE)
print(fambBingeAlpha)

#sapply(fambBinge, function(x) length(unique(x))) # to check warning message; there are at least 4 unique values in each item, so it is fine to proceed

# Purge
fambPurge <- famb_alpha %>%
  select(ends_with("_purge"))

fambPurge <- fambPurge %>%
  select(where(~ length(unique(.)) > 1))  # keep only columns with more than 1 unique values

fambPurge <- fambPurge %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .)) # replace NAs with imputed means

fambPurgeAlpha <- psych::alpha(fambPurge, check.keys = TRUE)
print(fambPurgeAlpha)

#sapply(fambPurge, function(x) length(unique(x))) # to check first warning message; there are at least 4 unique values in each item, so it is fine to proceed
# exploring the second warning
#which(duplicated(t(fambPurge))) # no duplicates
#sapply(fambPurge, sd, na.rm = TRUE) # no near-zero variances
#cor_mat <- cor(fambPurge, use = "pairwise.complete.obs") # no multicollinearity
#which(abs(cor_mat) > 0.99 & abs(cor_mat) < 1, arr.ind = TRUE)

# NSSI
fambNSSI <- famb_alpha %>%
  select(ends_with("_nssi"))

fambNSSI <- fambNSSI %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .)) # replace NAs with imputed means

fambNSSIAlpha <- psych::alpha(fambNSSI, check.keys = TRUE)
print(fambNSSIAlpha)

#sapply(fambNSSI, function(x) length(unique(x))) # to check warning message; there are at least 4 unique values in each item, so it is fine to proceed

