---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Cmd+Shift+Enter*. 

```{r}
library(ggplot2)
library(dplyr)
library(e1071)
library(ranger)
library(keras)
library(tensorflow)

```

```{r}
adult <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 
                    sep = ',', fill = F, strip.white = T)
colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 
                     'education_num', 'marital_status', 'occupation',   'relationship', 'race', 'sex', 
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')


str(adult)

```

```{r}
#install.packages("skimr")
skimr::skim(adult)

```

```{r}
#install.packages("DataExplorer")
library(DataExplorer)
DataExplorer::plot_bar(adult, nrow = 1)

DataExplorer::plot_histogram(adult, nrow = 1)
```


```{r}
DataExplorer::plot_boxplot(adult, by = "income", ncol = 3)
```

```{r}
ggplot(adult) + aes(x=as.numeric(age), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
```
```{r}
# histogram of age by gender group
ggplot(adult) + aes(x=as.numeric(age), group=sex, fill=sex) + 
  geom_histogram(binwidth=1, color='black')
```

```{r}
library(plyr)
library(dplyr)
library(sqldf)
library(reshape2)
library(gridExtra)
df  <- adult
df$income<-ifelse(df$income=='>50K',1,0)
df$workclass<-ifelse(df$workclass=='?','Unknown',as.character(df$workclass))
Work_class<-sqldf('SELECT workclass, count(workclass) as Count 
                  ,sum(income) as Above from df group by workclass')
table<-data.frame(Class=Work_class$workclass, Proportion=Work_class$Above/Work_class$Count)
Work_class$Below<-Work_class$Count-Work_class$Above
Work_class<-Work_class[,c(1,3,4)]
Workclass<-melt(Work_class,id.vars = 'workclass')
gg<-ggplot(Workclass,aes(x=workclass,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('red','green'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different classes')
tbl <- tableGrob(t(table), rows=NULL)
grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))
```

```{r}
education<-sqldf('SELECT education, count(education) as Count 
                  ,sum(income) as Above from df group by education')
education$Below<-education$Count-education$Above
table<-data.frame(Class=education$education, Proportion=education$Above/education$Count)
education<-education[,c(1,3,4)]
edu<-melt(education,id.vars = 'education')
gg<-ggplot(edu,aes(x=education,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('red','green'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different education level')
tbl <- tableGrob(t(table), rows=NULL)
grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))
```

```{r}
colnames(df)[6]<-'Marital'
marital<-sqldf('SELECT Marital, count(Marital) as Count 
                  ,sum(income) as Above from df group by Marital')
marital$Below<-marital$Count-marital$Above
table<-data.frame(Marital=marital$Marital, Proportion=marital$Above/marital$Count)
marital<-marital[,c(1,3,4)]
mar<-melt(marital,id.vars = 'Marital')
gg<-ggplot(mar,aes(x=Marital,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('red','green'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different marital status')
tbl <- tableGrob(t(table), rows=NULL)
grid.arrange(tbl, gg,
             nrow=2,
             as.table=TRUE,
             heights=c(1,4))
```

```{r}
# summarize the class distribution
percentage <- prop.table(table(adult$income)) * 100
cbind(freq=table(adult$income), percentage=percentage)
```

```{r}

library("mlr3verse")
set.seed(212) # set seed for reproducibility
credit_task <- TaskClassif$new(id = "incomeamount",
                               backend = adult, # <- NB: no na.omit() this time
                               target = "income",
                               positive = ">50K")

```

```{r}

cv5 <- rsmp("cv", folds = 5)
cv5$instantiate(credit_task)
```

```{r}
lrn_baseline <- lrn("classif.featureless", predict_type = "prob")
lrn_cart     <- lrn("classif.rpart", predict_type = "prob")
lrn_cart_cp  <- lrn("classif.rpart", predict_type = "prob", cp = 0.016, id = "cartcp")
lrn_ranger   <- lrn("classif.ranger", predict_type = "prob")
lrn_xgboost  <- lrn("classif.xgboost", predict_type = "prob")

```

```{r}
# Define a super learner
lrnsp_log_reg <- lrn("classif.rpart", predict_type = "prob", id = "super")
# Missingness imputation pipeline
pl_missing <- po("fixfactors") %>>%
  po("removeconstants") %>>%
  po("imputesample", affect_columns = selector_type(c("ordered", "factor"))) %>>%
  po("imputemean")
# Factors coding pipeline
pl_factor <- po("encode")
```

```{r}
# Now define the full pipeline
spr_lrn <- gunion(list(
  # First group of learners requiring no modification to input
  gunion(list(
    po("learner_cv", lrn_baseline),
    po("learner_cv", lrn_cart),
    po("learner_cv", lrn_cart_cp)
  )),
  # Next group of learners requiring special treatment of missingness
  pl_missing %>>%
    gunion(list(
      po("learner_cv", lrn_ranger),
      po("nop") # This passes through the original features adjusted for
                # missingness to the super learner
    )),
  # Last group needing factor encoding
  pl_factor %>>%
    po("learner_cv", lrn_xgboost)
)) %>>%
  po("featureunion") %>>%
  po(lrnsp_log_reg)
```

```{r}
res_spr <- resample(credit_task, spr_lrn, cv5, store_models = TRUE)
res_spr$prediction()$score(list(msr("classif.ce"),
                                msr("classif.acc"),
                                msr("classif.auc"),
                                msr("classif.fpr"),
                                msr("classif.fnr")))
```

```{r}
#install.packages("precrec")
library(precrec)
res_spr$prediction()$confusion
autoplot(res_spr, type = "roc")
autoplot(res_spr, type = "prc")
```




```{r}
#install.packages("tidyr")
library("rsample")
set.seed(212)

# First get the training
credit_split <- initial_split(adult)
credit_train <- training(credit_split)

credit_test <- testing(credit_split)

```



```{r}
#install.packages("recipes")
library("recipes")

cake <- recipe(income ~ ., data = adult) %>%
  step_meanimpute(all_numeric()) %>% # impute missings on numeric values with the mean
  step_center(all_numeric()) %>% # center by subtracting the mean from all numeric features
  step_scale(all_numeric()) %>% # scale by dividing by the standard deviation on all numeric features
  step_unknown(all_nominal(), -all_outcomes()) %>% # create a new factor level called "unknown" to account for NAs in factors, except for the outcome (response can't be NA)
  step_dummy(all_nominal(), one_hot = TRUE) %>% # turn all factors into a one-hot coding
  prep(training = credit_train) # learn all the parameters of preprocessing on the training data

credit_train_final <- bake(cake, new_data = credit_train) # apply preprocessing to training data
#credit_validate_final <- bake(cake, new_data = credit_validate) # apply preprocessing to validation data
credit_test_final <- bake(cake, new_data = credit_test) # apply preprocessing to testing data


```



```{r}
credit_train_x <- credit_train_final %>%
  select(-starts_with("income_")) %>%
  as.matrix()
credit_train_y <- credit_train_final %>%
  select(income_X.50K) %>%
  as.matrix()


credit_test_x <- credit_test_final %>%
  select(-starts_with("income_")) %>%
  as.matrix()
credit_test_y <- credit_test_final %>%
  select(income_X.50K) %>%
  as.matrix()
```

```{r}
#install.packages("keras")

library(keras)


```

```{r}
#install.packages("tensorflow")
library(tensorflow)

#install_tensorflow()
deep.net <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu",
              input_shape = c(ncol(credit_train_x))) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1, activation = "sigmoid")
```

```{r}
deep.net
```


```{r}
deep.net %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)
```


```{r}
deep.net %>% fit(
  credit_train_x, credit_train_y,
  epochs = 50, batch_size = 32,
  validation_split = 0.25,
)
```

```{r}
# To get the probability predictions on the test set:
pred_test_prob1 <- deep.net %>% predict_proba(credit_test_x)
# To get the raw classes (assuming 0.5 cutoff):
pred_test_res1 <- deep.net %>% predict_classes(credit_test_x)

```

```{r}
# Confusion matrix/accuracy/AUC metrics

table(pred_test_res1, credit_test_y)
deepl_acc<-yardstick::accuracy_vec(as.factor(credit_test_y),
                        as.factor(pred_test_res1))
deepl_acc
deepl_auc<-yardstick::roc_auc_vec(as.factor(credit_test_y),
                       c(pred_test_prob1))
deepl_auc
```


```{r}
library(randomForest)
rf3 <- randomForest(income ~ ., data = credit_train, ntree = 1000)
pred_test_prob2 <- predict(rf3, newdata = credit_test, type = 'prob')
pred_test_res2<- predict(rf3, newdata = credit_test, type = 'class')


```



```{r}
rf_test_res<-ifelse(pred_test_res2=='>50K',1,0)
rf_test_prob <- pred_test_prob2[,2]

table(rf_test_res, credit_test_y)
tree_acc <- yardstick::accuracy_vec(as.factor(credit_test_y),
                        as.factor(rf_test_res))
tree_acc
tree_auc <- yardstick::roc_auc_vec(as.factor(credit_test_y),
                       c(rf_test_prob))
tree_auc
```

```{r}
#NN
#install.packages("nnet")
library(nnet)
nn <- multinom(income ~ ., data = credit_train)
pred_test_prob5 <- predict(nn, newdata = credit_test, type = 'probs')
pred_test_res5<- predict(nn, newdata = credit_test, type = 'class')

```
```{r}
nn_test_res<-ifelse(pred_test_res5=='>50K',1,0)
table(nn_test_res, credit_test_y)
nn_acc <- yardstick::accuracy_vec(as.factor(credit_test_y),
                        as.factor(nn_test_res))
nn_acc
nn_auc <- yardstick::roc_auc_vec(as.factor(credit_test_y),
                       c(pred_test_prob5))
nn_auc
```


```{r}
#cart
library(rpart)
cart <- rpart(income ~ ., data = credit_train, method = 'class', cp = 1e-3)
pred_test_prob3 <- predict(cart, newdata = credit_test, type = 'prob')
pred_test_res3<- predict(cart, newdata = credit_test,, type = 'class')
```

```{r}

cart_test_prob<- pred_test_prob3 [,2]
cart_test_res<-ifelse(pred_test_res3=='>50K',1,0)
table(cart_test_res, credit_test_y)
cart_acc<-yardstick::accuracy_vec(as.factor(credit_test_y),
                        as.factor(cart_test_res))
cart_acc
cart_auc<-yardstick::roc_auc_vec(as.factor(credit_test_y),
                       c(cart_test_prob))
cart_auc

```

```{r}
#install.packages("kernlab")
library(kernlab)
svm4 <- ksvm(income ~ ., data = credit_train)
svm4.pred_prob <- predict(svm4, newdata = credit_test, type = 'decision')
svm4.pred_res <- predict(svm4, newdata = credit_test, type = 'response')

```

```{r}
svm4_test_res<-ifelse(svm4.pred_res=='>50K',1,0)
table(svm4_test_res, credit_test_y)
svm_acc<-yardstick::accuracy_vec(as.factor(credit_test_y),
                        as.factor(svm4_test_res))
svm_acc
svm_auc<-yardstick::roc_auc_vec(as.factor(credit_test_y),
                       c(svm4.pred_prob))
svm_auc
```



```{r}
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
```


```{r}
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


g +  scale_colour_manual(name = 'Classifier', values = c( 'SVM'='#56B4E9', 'CART'='#009E73', 'Random Forest'='#D55E00','Deep learning'='#E69F00',"NN"='#0072B2'))
```


```{r}
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
```

```{r}

Accuracy<-data.frame(Model=c('SWM', 'CART','Random Forest', 'Deep learing','NN',"best in super learner"),Accuracy=c(round(svm_acc,4),round(cart_acc,4),round(tree_acc,4),round(deepl_acc,4),round(nn_acc,4),0.8623))

gg<-ggplot(Accuracy,aes(x=Model,y=Accuracy,fill=Model))+geom_bar(stat = 'identity')+theme_bw()+ggtitle('Accuracies of Models')+geom_text(aes(label = Accuracy))
gg
```

