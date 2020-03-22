# Please run all the resampling code at first

library(ggplot2)

#SVM
pr1 <- prediction(svm4_test_res, credit_test_y)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
dd1 <- data.frame(FP = prf1@x.values[[1]], TP = prf1@y.values[[1]])

# CART
pr2 <- prediction(cart_test_res, credit_test_y)
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
dd2 <- data.frame(FP = prf2@x.values[[1]], TP = prf2@y.values[[1]])

# RF
pr3 <- prediction(rf_test_res, credit_test_y)
prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
dd3 <- data.frame(FP = prf3@x.values[[1]], TP = prf3@y.values[[1]])

#deep learning

pr4 <- prediction(pred_test_res1, credit_test_y)
prf4 <- performance(pr4, measure = "tpr", x.measure = "fpr")
dd4 <- data.frame(FP = prf4@x.values[[1]], TP = prf4@y.values[[1]])

#NN
pr5 <- prediction(nn_test_res, credit_test_y)
prf5 <- performance(pr5, measure = "tpr", x.measure = "fpr")
dd5 <- data.frame(FP = prf5@x.values[[1]], TP = prf5@y.values[[1]])

# plot ROC curve for logistic regression
g <- ggplot() +
  geom_line(data = dd1, aes(x = FP, y = TP, color = 'SVM')) +
  geom_line(data = dd2, aes(x = FP, y = TP, color = 'CART')) +
  geom_line(data = dd3, aes(x = FP, y = TP, color = 'Random Forest')) +
  geom_line(data = dd4, aes(x = FP, y = TP, color = 'Deep learning')) +
  geom_line(data = dd5, aes(x = FP, y = TP, color = 'NN')) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
  ggtitle('ROC Curve') +
  labs(x = 'False Positive Rate', y = 'True Positive Rate')


g +  scale_colour_manual(name = 'Classifier',
                        values = c( 'SVM'='#56B4E9', 'CART'='#009E73', 'Random Forest'='#D55E00','Deep learning'='#E69F00',"NN"='#0072B2'))

# AUC
auc <- rbind(
             svm_auc,
             cart_auc,
             tree_auc,
             deepl_auc,
             nn_auc)
rownames(auc) <- (c('SVM', 'CART','Random Forest', 'Deep learing',"NN"))
colnames(auc) <- 'Area Under ROC Curve'
auc

Accuracy<-data.frame(Model=c('SWM', 'CART','Random Forest', 'Deep learing','NN',"best in super learner"),Accuracy=c(round(svm_acc,4),round(cart_acc,4),round(tree_acc,4),round(deepl_acc,4),round(nn_acc,4),0.8623))

gg<-ggplot(Accuracy,aes(x=Model,y=Accuracy,fill=Model))+geom_bar(stat = 'identity')+theme_bw()+ggtitle('Accuracies of Models')+geom_text(aes(label = Accuracy))
gg
