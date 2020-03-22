# Load the data
source("03_load_data.R")


library(kernlab)

svm4 <- ksvm(income ~ ., data = credit_train)
svm4.pred_prob <- predict(svm4, newdata = credit_test, type = 'decision')
svm4.pred_res <- predict(svm4, newdata = credit_test, type = 'response')


svm4_test_res<-ifelse(svm4.pred_res=='>50K',1,0)
table(svm4_test_res, credit_test_y)
svm_acc<-yardstick::accuracy_vec(as.factor(credit_test_y),
                        as.factor(svm4_test_res))
svm_acc
svm_auc<-yardstick::roc_auc_vec(as.factor(credit_test_y),
                       c(svm4.pred_prob))
svm_auc
