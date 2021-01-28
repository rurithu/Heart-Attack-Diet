# Model Fitting and Selection
# Rithu Uppalapati, rurithu@umich.edu
# January 27, 2021

# Introduction: --------------------------------------------------------------
# Interested to Determine Dietary Information on Heart Attack Outcome

# Libraries: -----------------------------------------------------------------
library('caret') 
library('tidyverse') 
library('MASS')

# Model Splitting: -----------------------------------------------------------
nhanes = read.csv("nhanes.csv")
set.seed(123)
train_ind <-  createDataPartition(nhanes$heart_attack, p = .7)[[1]]
train_dat <- nhanes[train_ind, ]
test_dat <- nhanes[-train_ind, ]

# Model Fitting: -------------------------------------------------------------
# Covariate Selection
glm_full <- glm(heart_attack ~ ., data = nhanes)
model_select <- stepAIC(glm_full, direction = "both")
summ_selec <- summary(model_select)
# Select for BP1, age_BP, age_heart_att, tot_fat, folate, sat_fat, salmon, 
# sodium, and carbs

# Logistic Regression - MODEL 1
glm_mod <- glm(heart_attack ~BP1+age_BP+age_heart_att+tot_fat+sat_fat+salmon+
                 +folate+sodium+carbs, data = train_dat)
summary_glm <- summary(glm_mod)

prediction_glm <- predict(glm_mod, newdata= test_dat, type = "response")
prediction_glm <- ifelse(prediction_glm > 0.5, "M", "B")

# Decision Trees - MODEL 1
tree_mod <- train(as.factor(heart_attack) ~BP1+age_BP+age_heart_att+tot_fat+
                    sat_fat+salmon+folate+sodium+carbs,
                  method="rpart", data = train_dat)
prediction_tree <- predict(tree_mod, newdata= test_dat)


