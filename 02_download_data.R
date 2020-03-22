# Write code here to download the data you are using for your report.
# DO NOT push the data to your Github repository.

# For example, to download the simple Iris data from the UCI Machine Learning
# Repository
adult <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data',
                    sep = ',', fill = F, strip.white = T)

library("dplyr")
library("forcats")

# Apply the names as defined on the website https://archive.ics.uci.edu/ml/datasets/
# and update the class labels to be shorter
colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'education',
                     'education_num', 'marital_status', 'occupation',   'relationship', 'race', 'sex',
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')

# Save into Data directory which is not pushed to Github
saveRDS(adult, "Data/adult.rds")
