library(dplyr)
source("read_data.R")

learnLRF <- select(learn, -contains("MIG"), -contains("NTVTY"), -contains("NATVTY"), -GRINST)
testLRF <- select(test, -contains("MIG"), -contains("NTVTY"), -contains("NATVTY"), -GRINST)

# FIT WITH A LOGISTIC REGRESSION MODEL
fitFormulaLRF <- SAV ~ .
fitLRF <- glm(fitFormulaLRF, data=learnLRF, family=binomial(link="logit"))
summary(fitLRF)

# TEST THE FIT
predLearnLRF <- predict(fitLRF, learnLRF, type="response")
predFactorLearnLRF <- cut(predLearnLRF, breaks=c(0, 0.5, 1), labels=c("-50000.", "50000+"))
TabLearnLRF <- table(learn$SAV, predFactorLearnLRF, dnn=c("actual", "predicted"))
TP <- TabLearnLRF[2,2]    # True positive
TN <- TabLearnLRF[1,1]    # True negative
FP <- TabLearnLRF[1,2]    # False positive
FN <- TabLearnLRF[2,1]    # False negative
FScoreLearnLRF <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FScoreLearnLRF

predTestLRF <- predict(fitLRF, testLRF, type="response")
predFactorTestLRF <- cut(predTestLRF, breaks=c(0, 0.5, 1), labels=c("-50000.", "50000+"))
TabTestLRF <- table(test$SAV, predFactorTestLRF, dnn=c("actual", "predicted"))
TP <- TabTestLRF[2,2]    # True positive
TN <- TabTestLRF[1,1]    # True negative
FP <- TabTestLRF[1,2]    # False positive
FN <- TabTestLRF[2,1]    # False negative
FScoreTestLRF <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FScoreTestLRF
