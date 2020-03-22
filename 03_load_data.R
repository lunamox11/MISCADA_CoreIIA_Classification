# Write code here to load the data you downloaded in download_data.R

adult <- readRDS("Data/adult.rds")

# You might choose to do any resampling here to ensure it is consistent across
# models

set.seed(212) # set seed for reproducibility

library("rsample")
library(tidyr)
library("mlr3verse")
library("recipes")

set.seed(212) # set seed for reproducibility
credit_task <- TaskClassif$new(id = "incomeamount",
                               backend = adult, # <- NB: no na.omit() this time
                               target = "income",
                               positive = ">50K")

cv5 <- rsmp("cv", folds = 5)
cv5$instantiate(credit_task)


set.seed(212)

# First get the training
credit_split <- initial_split(adult)
credit_train <- training(credit_split)
credit_test <- testing(credit_split)

credit_train_x <- credit_train_final %>%
 select(-starts_with("income_")) %>%
 as.matrix()
credit_train_y <- credit_train_final %>%
 select(income_X.50K) %>%
 as.matrix()



 cake <- recipe(income ~ ., data = adult) %>%
   step_meanimpute(all_numeric()) %>% # impute missings on numeric values with the mean
   step_center(all_numeric()) %>% # center by subtracting the mean from all numeric features
   step_scale(all_numeric()) %>% # scale by dividing by the standard deviation on all numeric features
   step_unknown(all_nominal(), -all_outcomes()) %>% # create a new factor level called "unknown" to account for NAs in factors, except for the outcome (response can't be NA)
   step_dummy(all_nominal(), one_hot = TRUE) %>% # turn all factors into a one-hot coding
   prep(training = credit_train) # learn all the parameters of preprocessing on the training data

 credit_train_final <- bake(cake, new_data = credit_train) # apply preprocessing to training data
 credit_test_final <- bake(cake, new_data = credit_test) # apply preprocessing to testing data

credit_test_x <- credit_test_final %>%
 select(-starts_with("income_")) %>%
 as.matrix()
credit_test_y <- credit_test_final %>%
 select(income_X.50K) %>%
 as.matrix()
