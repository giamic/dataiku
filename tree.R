# FIT WITH A DECISION TREE
library(rpart)
source("read_data.R")
learn <- select(learn, -SAVN)
fitRT <- rpart(SAV ~ ., data=learn, method="anova")
printcp(fitRT)
plotcp(fitRT)
plot(fitRT)
text(fitRT)

# TEST THE FIT
predLearnRT <- predict(fitRT, learn, type="vector")
predFactorLearnRT <- cut(predLearnRT, breaks=c(1, 1.5, 2), labels=c("-50000.", "50000+"))
TabLearnRT <- table(learn$SAV, predFactorLearnRT, dnn=c("actual", "predicted"))
TP <- TabLearnRT[2,2]    # True positive
TN <- TabLearnRT[1,1]    # True negative
FP <- TabLearnRT[1,2]    # False positive
FN <- TabLearnRT[2,1]    # False negative
FScoreLearnRT <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FScoreLearnRT

predTestRT <- predict(fitRT, test, type="vector")
predFactorTestRT <- cut(predTestRT, breaks=c(1, 1.5, 2), labels=c("-50000.", "50000+"))
TabTestRT <- table(test$SAV, predFactorTestRT, dnn=c("actual", "predicted"))
TP <- TabTestRT[2,2]    # True positive
TN <- TabTestRT[1,1]    # True negative
FP <- TabTestRT[1,2]    # False positive
FN <- TabTestRT[2,1]    # False negative
FscoreTestRT <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FscoreTestRT