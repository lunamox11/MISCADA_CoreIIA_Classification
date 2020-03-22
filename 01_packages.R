# Define the packages your code requires in the vector below.
# This will allow the marker of your report to easily get all the packages they
# require
# Specify packages on CRAN:
report.packages.cran <- c("dplyr",
                          "forcats",
                          "rsample",
                          "tidyr",
                          "ranger",
                          "pROC",
                          "ggplot2",
                          "keras",
                          "tensorflow",
                          "e1071",
                          "DataExplorer",
                          "mlr3verse",
                          "precrec",
                          "recipes",
                          "nnet",
                          "rpart",
                          "kernlab",
                          "plyr",
                          "sqldf",
                          "reshape2",
                          "gridExtra",
                          "skimr")



# The following code will install anything that is missing:
# CRAN
to.install <- setdiff(report.packages.cran, installed.packages()[,"Package"])
if(length(to.install) > 0)
  install.packages(to.install)
