# Load the data
source("03_load_data.R")

# Let's fit a deep nerual network
library(keras)
library(tensorflow)

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

deep.net

deep.net %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

deep.net %>% fit(
  credit_train_x, credit_train_y,
  epochs = 50, batch_size = 32,
  validation_split = 0.25,
)

# To get the probability predictions on the test set:
pred_test_prob1 <- deep.net %>% predict_proba(credit_test_x)
# To get the raw classes (assuming 0.5 cutoff):
pred_test_res1 <- deep.net %>% predict_classes(credit_test_x)

# Confusion matrix/accuracy/AUC metrics

table(pred_test_res1, credit_test_y)
deepl_acc<-yardstick::accuracy_vec(as.factor(credit_test_y),
                        as.factor(pred_test_res1))
deepl_acc
deepl_auc<-yardstick::roc_auc_vec(as.factor(credit_test_y),
                       c(pred_test_prob1))
deepl_auc
