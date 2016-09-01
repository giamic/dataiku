# FIT WITH A DECISION TREE, ALGORITHM C5.0
library(C50)
source("read_data.R")
learn <- select(learn, -SAVN)
fitC50 <- C5.0(SAV ~ ., data=learn, trials=1)
print(fitC50)

# TEST THE FIT
predFactorLearnC50 <- predict(fitC50, learn, type="class")
TabLearnC50 <- table(learn$SAV, predFactorLearnC50, dnn=c("actual", "predicted"))
TP <- TabLearnC50[2,2]    # True positive
TN <- TabLearnC50[1,1]    # True negative
FP <- TabLearnC50[1,2]    # False positive
FN <- TabLearnC50[2,1]    # False negative
FScoreLearnC50 <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FScoreLearnC50

predFactorTestC50 <- predict(fitC50, test, type="class")
TabTestC50 <- table(test$SAV, predFactorTestC50, dnn=c("actual", "predicted"))
TP <- TabTestC50[2,2]    # True positive
TN <- TabTestC50[1,1]    # True negative
FP <- TabTestC50[1,2]    # False positive
FN <- TabTestC50[2,1]    # False negative
FscoreTestC50 <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FscoreTestC50