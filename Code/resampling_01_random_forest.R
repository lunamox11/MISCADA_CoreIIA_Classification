# Load the data
source("03_load_data.R")

# Let's fit a simple random forest
library("ranger")
library(randomForest)

rf3 <- randomForest(income ~ ., data = credit_train, ntree = 1000)
pred_test_prob2 <- predict(rf3, newdata = credit_test, type = 'prob')
pred_test_res2<- predict(rf3, newdata = credit_test, type = 'class')

rf_test_res<-ifelse(pred_test_res2=='>50K',1,0)
rf_test_prob <- pred_test_prob2[,2]

table(rf_test_res, credit_test_y)
tree_acc <- yardstick::accuracy_vec(as.factor(credit_test_y),
                        as.factor(rf_test_res))
tree_acc
tree_auc <- yardstick::roc_auc_vec(as.factor(credit_test_y),
                       c(rf_test_prob))
tree_auc
