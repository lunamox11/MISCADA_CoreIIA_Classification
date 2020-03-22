# Load the data
source("03_load_data.R")


library(rpart)

cart <- rpart(income ~ ., data = credit_train, method = 'class', cp = 1e-3)
pred_test_prob3 <- predict(cart, newdata = credit_test, type = 'prob')
pred_test_res3<- predict(cart, newdata = credit_test,, type = 'class')

cart_test_prob<- pred_test_prob3 [,2]
cart_test_res<-ifelse(pred_test_res3=='>50K',1,0)
table(cart_test_res, credit_test_y)
cart_acc<-yardstick::accuracy_vec(as.factor(credit_test_y),
                        as.factor(cart_test_res))
cart_acc
cart_auc<-yardstick::roc_auc_vec(as.factor(credit_test_y),
                       c(cart_test_prob))
cart_auc
