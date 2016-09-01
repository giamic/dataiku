library(dplyr)
source("read_data.R")
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

# WORK
learn$WRKINC <- as.factor(grepl("Self-employed-incorporated", learn$ACLSWKR))
learn$WRKFED <- as.factor(grepl("Federal government", learn$ACLSWKR))
learn$WRKFT <- as.factor(grepl("Full-time schedules", learn$AWKSTAT))
test$WRKINC <- as.factor(grepl("Self-employed-incorporated", test$ACLSWKR))
test$WRKFED <- as.factor(grepl("Federal government", test$ACLSWKR))
test$WRKFT <- as.factor(grepl("Full-time schedules", test$AWKSTAT))

# OCCUPATION
learn$OCCADP <- as.factor(grepl("Executive admin and managerial|Professional specialty", learn$AMJOCC))
learn$OCCDEF <- as.factor(grepl("Armed Forces|Protective services", learn$AMJOCC))
learn$OCCSLT <- as.factor(grepl("Sales|Technicians and related support", learn$AMJOCC))
learn$OCCNOT <- as.factor(grepl("Not in universe|Other service|Private household services", learn$AMJOCC))
learn$SEYES <- as.factor(learn$SEOTR == 1)
test$OCCADP <- as.factor(grepl("Executive admin and managerial|Professional specialty", test$AMJOCC))
test$OCCDEF <- as.factor(grepl("Armed Forces|Protective services", test$AMJOCC))
test$OCCSLT <- as.factor(grepl("Sales|Technicians and related support", test$AMJOCC))
test$OCCNOT <- as.factor(grepl("Not in universe|Other service|Private household services", test$AMJOCC))
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
learn$CAPYES <- as.factor(learn$CAPGAIN > 7364)    # This comes from the decision tree
learn$DIVYES <- as.factor(learn$DIVVAL > 0)
test$CAPYES <- as.factor(test$CAPGAIN > 7364)    # This comes from the decision tree
test$DIVYES <- as.factor(test$DIVVAL > 0)

learnLR <- mutate(learn, WKSWORK = WKSWORK/52)
learnLR <- mutate(learnLR, NOEMP = NOEMP/6)
testLR <- mutate(test, WKSWORK = WKSWORK/52)
learnLR <- mutate(testLR, NOEMP = NOEMP/6)

# FIT WITH A LOGISTIC REGRESSION MODEL
fitFormulaLR <- SAV ~ 
  ASEX+RACEWA+AGEMD+HOUSECHL+HOUSEHLD+
  EDLOW+EDBAC+EDMAS+EDSUP+
  WKSWORK+NOEMP+WRKINC+WRKFED+WRKFT+OCCADP+OCCDEF+OCCSLT+OCCNOT+SEOTR+
  DIVYES+CAPYES
fitLR <- glm(fitFormulaLR, data=learn, family=binomial(link="logit"))
summary(fitLR)

# TEST THE FIT
predLearnLR <- predict(fitLR, learn, type="response")
predFactorLearnLR <- cut(predLearnLR, breaks=c(0, 0.5, 1), labels=c("-50000.", "50000+"))
TabLearnLR <- table(learn$SAV, predFactorLearnLR, dnn=c("actual", "predicted"))
TP <- TabLearnLR[2,2]    # True positive
TN <- TabLearnLR[1,1]    # True negative
FP <- TabLearnLR[1,2]    # False positive
FN <- TabLearnLR[2,1]    # False negative
FScoreLearnLR <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FScoreLearnLR

predTestLR <- predict(fitLR, test, type="response")
predFactorTestLR <- cut(predTestLR, breaks=c(0, 0.5, 1), labels=c("-50000.", "50000+"))
TabTestLR <- table(test$SAV, predFactorTestLR, dnn=c("actual", "predicted"))
TP <- TabTestLR[2,2]    # True positive
TN <- TabTestLR[1,1]    # True negative
FP <- TabTestLR[1,2]    # False positive
FN <- TabTestLR[2,1]    # False negative
FScoreTestLR <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FScoreTestLR
