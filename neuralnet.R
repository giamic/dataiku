library(neuralnet)
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
learn$HOUSE <- learn$HHDFMX

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

# FIT WITH A NEURAL NETWORK
# n <- names(learn)
# fitFormulaNN <- as.formula(paste("SAV ~", paste(n[!n %in% "SAV"], collapse = " + ")))
m <- model.matrix( ~
  SAV+ASEX+RACEWA+AGEMD+HOUSECHL+HOUSEHLD+EDLOW+EDBAC+EDMAS+EDSUP+WKSWORK+
    NOEMP+WRKINC+WRKFED+WRKFT+OCCADP+OCCDEF+OCCSLT+OCCNOT+SEOTR+DIVYES+CAPYES,
  data = learn
  )
colnames(m)[2] = "SAV50000"
fitFormulaNN <- SAV50000 ~
  ASEXMale+CAPYESTRUE+HOUSEHLDTRUE+NOEMP+WRKINCTRUE+OCCADPTRUE+RACEWATRUE+AGEMDTRUE+
  # HOUSECHLTRUE+
  # EDLOWTRUE+EDBACTRUE+EDMASTRUE+EDSUPTRUE+
  # WKSWORK+WRKFEDTRUE+WRKFTTRUE+OCCDEFTRUE+OCCSLTTRUE+OCCNOTTRUE+SEOTR1+SEOTR2+
  DIVYESTRUE
fitNN <- neuralnet(fitFormulaNN, data=m, hidden=c(10))
summary(fitNN)

# TEST THE FIT
predLearnNN <- predict(fitNN, learn, type="response")
predFactorLearnNN <- cut(predLearnNN, breaks=c(-Inf, 0.5, Inf), labels=c("-50000.", "50000+"))
TabLearnNN <- table(learn$SAV, predFactorLearnNN, dnn=c("actual", "predicted"))
TP <- TabLearnNN[2,2]    # True positive
TN <- TabLearnNN[1,1]    # True negative
FP <- TabLearnNN[1,2]    # False positive
FN <- TabLearnNN[2,1]    # False negative
FScoreLearnNN <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FScoreLearnNN

predTestNN <- predict(fitNN, test, type="response")
predFactorTestNN <- cut(predTestNN, breaks=c(-Inf, 0.5, Inf), labels=c("-50000.", "50000+"))
TabTestNN <- table(test$SAV, predFactorTestNN, dnn=c("actual", "predicted"))
TP <- TabTestNN[2,2]    # True positive
TN <- TabTestNN[1,1]    # True negative
FP <- TabTestNN[1,2]    # False positive
FN <- TabTestNN[2,1]    # False negative
FScoreTestNN <- 2*TP/(2*TP+FN+FP)    # Gives a measure of the goodness of the prediction, comprised between 0 (bad) and 1 (perfect)
FScoreTestNN
