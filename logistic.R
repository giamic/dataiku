# WITH THE KNOWLEDGE OF THE GRAPHIC ANALYSIS, CREATE A NEW BETTER VARIABLE FOR THE AGE
# THIS NEEDS TO BE DONE!
library(dplyr)
### CREATE APPROPRIATE VALUES FOR THE MODEL

# CHANGE THE EDUCATION
patternLow <- "Children|Less than 1st grade|1st 2nd 3rd or 4th grade|5th or 6th grade|7th and 8th grade|9th grade|10th grade|11th grade|12th grade no diploma"
patternMid <- "High school graduate|Some college but no degree|Associates degree-occup /vocational|Associates degree-academic program"

learn$EDLOW <- as.factor(grepl(patternLow, learn$AHGA))
#learn$EDMID <- as.factor(grepl(patternMid, learn$AHGA))
learn$EDBAC <- as.factor(grepl("Bachelors degree\\(BA AB BS\\)", learn$AHGA))
learn$EDMAS <- as.factor(grepl("Masters degree\\(MA MS MEng MEd MSW MBA\\)", learn$AHGA))
learn$EDSUP <- as.factor(grepl("Doctorate degree\\(PhD EdD\\)|Prof school degree \\(MD DDS DVM LLB JD\\)", learn$AHGA))

test$EDLOW <- as.factor(grepl(patternLow, test$AHGA))
#test$EDMID <- as.factor(grepl(patternMid, test$AHGA))
test$EDBAC <- as.factor(grepl("Bachelors degree(BA AB BS)", test$AHGA))
test$EDMAS <- as.factor(grepl("Masters degree(MA MS MEng MEd MSW MBA)", test$AHGA))
test$EDSUP <- as.factor(grepl("Doctorate degree(PhD EdD)|Prof school degree (MD DDS DVM LLB JD)", test$AHGA))

# RACE - data say white and asian are the richest ( :-( )
learn$RACEWA <- as.factor(grepl("White|Asian", learn$ARACE))
test$RACEWA <- as.factor(grepl("White|Asian", test$ARACE))

# HOUSE
learn$HOUSEHLD <- as.factor(learn$HHDFMX=="Householder")
learn$HOUSECHL <- as.factor(grepl("Child|Grandchild|<18", learn$HHDFMX))
test$HOUSEHLD <- as.factor(test$HHDFMX=="Householder")
test$HOUSECHL <- as.factor(grepl("Child|Grandchild|<18", test$HHDFMX))
learn$HOUSE <- learn$HHDFMX

# WORK
learn$WRKINC <- as.factor(grepl("Self-employed-incorporated", learn$ACLSWKR))
learn$WRKFED <- as.factor(grepl("Federal government", learn$ACLSWKR))
learn$WRKFT <- as.factor(grepl("Full-time schedules", learn$AWKSTAT))
test$WRKINC <- as.factor(grepl("Self-employed-incorporated", test$ACLSWKR))
test$WRKFED <- as.factor(grepl("Federal government", test$ACLSWKR))
test$WRKFT <- as.factor(grepl("Full-time schedules", test$AWKSTAT))

# OCCUPATION
learn$OCCADM <- as.factor(grepl("Executive admin and managerial", learn$AMJOCC))
learn$OCCPRF <- as.factor(grepl("Professional specialty", learn$AMJOCC))
learn$OCCDEF <- as.factor(grepl("Armed Forces|Protective services", learn$AMJOCC))
learn$OCCSLS <- as.factor(grepl("Sales", learn$AMJOCC))
learn$OCCNOT <- as.factor(grepl("Not in universe", learn$AMJOCC))
learn$SEYES <- as.factor(learn$SEOTR == 1)
test$OCCADM <- as.factor(grepl("Executive admin and managerial", test$AMJOCC))
test$OCCPRF <- as.factor(grepl("Professional specialty", test$AMJOCC))
test$OCCDEF <- as.factor(grepl("Armed Forces|Protective services", test$AMJOCC))
test$OCCSLS <- as.factor(grepl("Sales", test$AMJOCC))
test$OCCNOT <- as.factor(grepl("Not in universe", test$AMJOCC))
test$SEYES <- as.factor(test$SEOTR == 1)

# AGE
learn$AGECH <- as.factor(learn$AAGE < 18)
learn$AGEYO <- as.factor(learn$AAGE > 18 & learn$AAGE < 35)
learn$AGEMD <- as.factor(learn$AAGE > 35 & learn$AAGE < 60)
#learn$AGEOL <- as.factor(learn$AAGE > 60)
test$AGECH <- as.factor(test$AAGE < 18)
test$AGEYO <- as.factor(test$AAGE > 18 & test$AAGE < 35)
test$AGEMD <- as.factor(test$AAGE > 35 & test$AAGE < 60)
#test$AGEOL <- as.factor(test$AAGE > 60)

# FINANCE
learn$CAPYES <- as.factor(learn$CAPGAIN > 0)
learn$DIVYES <- as.factor(learn$DIVVAL > 0)
test$CAPYES <- as.factor(test$CAPGAIN > 0)
test$DIVYES <- as.factor(test$DIVVAL > 0)


# FIT WITH A LOGISTIC REGRESSION MODEL
fitLogReg <- SAV ~ 
  ASEX+RACEWA+AGEMD+HOUSECHL+HOUSEHLD+
  EDLOW+EDBAC+EDMAS+EDSUP+
  WKSWORK+NOEMP+WRKINC+WRKFED+WRKFT+OCCADM+OCCPRF+OCCDEF+OCCSLS+OCCNOT+SEOTR+
  DIVYES+CAPYES
fit <- glm(fitLogReg, data=learn, family=binomial(link="logit"))
summary(fit)

# FIT WITH A DECISION TREE
library(rpart)
fitRegrTree <- SAV ~ 
  AAGE + NOEMP + DIVVAL + WKSWORK
  #OCCNOT+EDSUP+DIVYES+ASEX+RACEWA+AGEMD
  #HOUSEHLD+NOEMP+
  #OCCADM+OCCPRF+OCCDEF+OCCSLS+CAPYES+
  #EDBAC+EDMAS+
  #WKSWORK+WRKINC+WRKFED+WRKFT+SEOTR
fit <- rpart(fitRegrTree, data=learn, method="anova")
printcp(fit)
plot(fit)
text(fit)
predLearn <- predict(fit, learn, type="vector")
predFacL <- cut(predLearn, breaks=c(1, 1.5, 2), labels=c("-50000.", "50000+"))
predTest <- predict(fit, test, type="vector")
predFac <- cut(predTest, breaks=c(1, 1.5, 2), labels=c("-50000.", "50000+"))

# FIT WITH A RANDOM FOREST
library(randomForest)
fitRandF <- SAV ~ 
  ASEX+RACEWA+AGEMD+HOUSECHL+HOUSEHLD+
  EDLOW+EDBAC+EDMAS+EDSUP+
  WKSWORK+NOEMP+WRKINC+WRKFED+WRKFT+OCCADM+OCCPRF+OCCDEF+OCCSLS+OCCNOT+SEOTR+
  DIVYES+CAPYES
fit <- randomForest(fitRandF, data=learn)
summary(fit)

# TEST THE FIT
predLearn <- predict(fit, learn, type="response")
predFacL <- cut(predLearn, breaks=c(-Inf, 0.5, Inf), labels=c("-50000.", "50000+"))
lTab <- table(learn$SAV, predFacL, dnn=c("actual", "predicted"))
TP <- lTab[2,2]    # True positive
TN <- lTab[1,1]    # True negative
FP <- lTab[1,2]    # False positive
FN <- lTab[2,1]    # False negative
Fscore <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
Fscore


predTest <- predict(fit, test, type="response")
predFac <- cut(predTest, breaks=c(-Inf, 0.5, Inf), labels=c("-50000.", "50000+"))
cTab <- table(test$SAV, predFac, dnn=c("actual", "predicted"))
TP <- cTab[2,2]    # True positive
TN <- cTab[1,1]    # True negative
FP <- cTab[1,2]    # False positive
FN <- cTab[2,1]    # False negative
Fscore <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
Fscore
