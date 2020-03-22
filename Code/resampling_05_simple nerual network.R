# Load the data
source("03_load_data.R")

# Let's fit a simple nerual network
library(nnet)
nn <- multinom(income ~ ., data = credit_train)
pred_test_prob5 <- predict(nn, newdata = credit_test, type = 'probs')
pred_test_res5<- predict(nn, newdata = credit_test, type = 'class')

nn_test_res<-ifelse(pred_test_res5=='>50K',1,0)
table(nn_test_res, credit_test_y)
nn_acc <- yardstick::accuracy_vec(as.factor(credit_test_y),
                        as.factor(nn_test_res))
nn_acc
nn_auc <- yardstick::roc_auc_vec(as.factor(credit_test_y),
                       c(pred_test_prob5))
nn_auc
