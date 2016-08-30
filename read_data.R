### IMPORT THE DATA AND GIVE PROPER NAME TO COLUMNS
learn <- read.csv("./us_census_full/census_income_learn.csv", na.strings = "?", header = FALSE, strip.white = TRUE)
test <- read.csv("./us_census_full/census_income_test.csv", na.strings = "?", header = FALSE, strip.white = TRUE)
lab <- read.csv("labels.csv", stringsAsFactors = FALSE, header = FALSE)
colnames(learn) <- lab[, 1]
colnames(test) <- lab[, 1]
learnWeights <- learn$MARSUPWT
testWeights <- test$MARSUPWT

### ADD A NUMERICAL COLUMN CORRESPONDING TO SAV
library(dplyr)
learn <- mutate(learn, SAVN = as.numeric(learn$SAV=="50000+."))
test <- mutate(test, SAVN = as.numeric(test$SAV=="50000+."))

### MAKE SURE THAT THE CODE ARE INTENDED AS FACTORS AND NOT AS NUMS
learn$ADTIND <- as.factor(learn$ADTIND)
learn$ADTOCC <- as.factor(learn$ADTOCC)
learn$SEOTR <- as.factor(learn$SEOTR)
learn$VETYN <- as.factor(learn$VETYN)

test$ADTIND <- as.factor(test$ADTIND)
test$ADTOCC <- as.factor(test$ADTOCC)
test$SEOTR <- as.factor(test$SEOTR)
test$VETYN <- as.factor(test$VETYN)
