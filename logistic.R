# WITH THE KNOWLEDGE OF THE GRAPHIC ANALYSIS, CREATE A NEW BETTER VARIABLE FOR THE AGE
# THIS NEEDS TO BE DONE!
library(dplyr)
learn <- mutate(learn, AGEFIT = exp(-((AAGE-50)/10)^2))
test <- mutate(test, AGEFIT = exp(-((AAGE-50)/10)^2))

# FIT WITH A LOGISTIC REGRESSION MODEL
fit <- glm(SAV ~ AGEFIT+ACLSWKR+ADTOCC+AHGA+ARACE+ASEX, data=learn,family=binomial())

# TEST THE FIT
predTest <- predict(fit, test, type="response")
predFac <- cut(predTest, breaks=c(-Inf, 0.5, Inf), labels=c("-50000.", "50000+"))
cTab <- table(test$SAV, predFac, dnn=c("actual", "predicted"))
addmargins(cTab)