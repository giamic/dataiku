# FIT WITH A RANDOM FOREST
library(randomForest)
source("read_data.R")
# fitFormulaRF <- SAV ~ 
#   ASEX+RACEWA+AGEMD+HOUSECHL+HOUSEHLD+
#   EDLOW+EDBAC+EDMAS+EDSUP+
#   WKSWORK+NOEMP+WRKINC+WRKFED+WRKFT+OCCADM+OCCPRF+OCCDEF+OCCSLS+OCCNOT+SEOTR+
#   DIVYES+CAPYES
# fitRF <- randomForest(fitFormulaRF, data=learn)

learnRF <- select(learn, -contains("MIG"), -contains("NTVTY"), -contains("NATVTY"), -GRINST)
testRF <- select(test, -contains("MIG"), -contains("NTVTY"), -contains("NATVTY"), -GRINST)
fitRF <- randomForest(SAV ~ ., data=learnRF)
summary(fitRF)
# TEST THE FIT
predFactorLearnRF <- predict(fitRF, learnRF, type="class")
TabLearnRF <- table(learn$SAV, predFactorLearnRF, dnn=c("actual", "predicted"))
TP <- TabLearnRF[2,2]    # True positive
TN <- TabLearnRF[1,1]    # True negative
FP <- TabLearnRF[1,2]    # False positive
FN <- TabLearnRF[2,1]    # False negative
FScoreLearnRF <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FScoreLearnRF

levels(learnRF$HHDFMX) <- levels(testRF$HHDFMX)
predFactorTestRF <- predict(fitRF, testRF, type="class")
TabTestRF <- table(test$SAV, predFactorTestRF, dnn=c("actual", "predicted"))
TP <- TabTestRF[2,2]    # True positive
TN <- TabTestRF[1,1]    # True negative
FP <- TabTestRF[1,2]    # False positive
FN <- TabTestRF[2,1]    # False negative
FScoreTestRF <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FScoreTestRF
