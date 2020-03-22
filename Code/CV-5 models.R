# Load the data
source("03_load_data.R")

# do cross validation, find the best model in 5 models

library("mlr3verse")

lrn_baseline <- lrn("classif.featureless", predict_type = "prob")
lrn_cart     <- lrn("classif.rpart", predict_type = "prob")
lrn_cart_cp  <- lrn("classif.rpart", predict_type = "prob", cp = 0.016, id = "cartcp")
lrn_ranger   <- lrn("classif.ranger", predict_type = "prob")
lrn_xgboost  <- lrn("classif.xgboost", predict_type = "prob")

# Define a super learner
lrnsp_log_reg <- lrn("classif.rpart", predict_type = "prob", id = "super")
# Missingness imputation pipeline
pl_missing <- po("fixfactors") %>>%
  po("removeconstants") %>>%
  po("imputesample", affect_columns = selector_type(c("ordered", "factor"))) %>>%
  po("imputemean")
# Factors coding pipeline
pl_factor <- po("encode")

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

res_spr <- resample(credit_task, spr_lrn, cv5, store_models = TRUE)
res_spr$prediction()$score(list(msr("classif.ce"),
                                  msr("classif.acc"),
                                  msr("classif.auc"),
                                  msr("classif.fpr"),
                                  msr("classif.fnr")))
