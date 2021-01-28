# Data Pre-Processing
# Rithu Uppalapati, rurithu@umich.edu
# January 27, 2021

# Introduction: --------------------------------------------------------------
# Interested to Determine Dietary Information on Heart Attack Outcome

# Libraries: -----------------------------------------------------------------
library('foreign') 
library('tidyverse') 
setwd("/Users/rithuuppalapati/Desktop/Heart Attack Prediction")

# Import: --------------------------------------------------------------------
# Demographic Data 
# https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.htm
demo = read.xport("DEMO_J.XPT") %>%
  select(id = SEQN, gender = RIAGENDR, age = RIDAGEYR, race = RIDRETH1, 
         edu = DMDEDUC2) 

demo <- demo[- c(1,2), ]

# Medical History Data 
# https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/MCQ_J.htm
mhd = read.xport("MCQ_J.XPT") %>% 
  select(id = SEQN, heart_attack = MCQ160E, age_heart_att = MCD180E, 
        relative_heart_attack = MCQ300A) %>%
  mutate(
   heart_attack = ifelse(heart_attack==1, 1, 0),
   heart_attack = replace_na(heart_attack, 999),
   age_heart_att = replace_na(age_heart_att, 0),
   relative_heart_attack = ifelse(relative_heart_attack==1, 1, 0)
  ) 
mhd <- mhd[- c(1,2), ]

# Blood Pressure and Cholesterol 
# https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BPQ_J.htm
bpc = read.xport("BPQ_J.XPT") %>%
  select(id = SEQN, BP1 = BPQ020, BP2 = BPQ030, age_BP = BPD035, 
         high_chol = BPQ080) %>%
  mutate(
    BP1 = ifelse(BP1==1, 1, 0),
    BP2 = replace_na(BP2, 2),
    BP2 = ifelse(BP2==1, 1, 0),
    age_BP = replace_na(age_BP, 999),
    high_chol = ifelse(high_chol==1, 1, 0)
  )

# Dietary Data 
# https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1IFF_J.htm
diet = read.xport("DR1IFF_J.XPT") %>% 
 select(id = SEQN, protein = DR1IPROT, carbs = DR1ICARB, sugar = DR1ISUGR, 
        tot_fat = DR1ITFAT, sat_fat = DR1ISFAT, chol_int = DR1ICHOL, 
       sodium = DR1ISODI) 
 diet <- diet[!duplicated(diet$id), ]

 diet <- diet[-1, ]

# Dietary Data 2 
# https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1TOT_J.htm
diet2 = read.xport("DR1TOT_J.XPT") %>% 
  select(id = SEQN, shrimp = DRD350HQ, tuna = DRD370BQ, salmon = DRD370MQ, 
         folate = DR1TFF) 
diet2 <- diet2[- c(1,2), ]

# Data Prep: -----------------------------------------------------------------
full_dat = demo %>%
  left_join(bpc, by = "id") %>%
  left_join(mhd, by = "id") %>%
  left_join(diet, by = "id") %>%
  left_join(diet2, by = "id") %>%
  drop_na()

