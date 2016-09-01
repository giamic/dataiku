### UNIVARIATE AUDIT
library(SDMTools)
source("read_data.R")
### ADD A NUMERICAL COLUMN CORRESPONDING TO SAV
library(dplyr)
learn <- mutate(learn, SAVN = as.numeric(learn$SAV=="50000+."))
test <- mutate(test, SAVN = as.numeric(test$SAV=="50000+."))


### NUMERICAL VARIABLES
learnNum <- learn[,sapply(learn, is.numeric)]
learnNum <- mutate(learn, SAVN = as.numeric(learn$SAV=="50000+."))
### VARIOUS STATISTICAL CALCULATIONS
learnWMean <- sapply(learnNum, function(x) wt.mean(x, learnNum$MARSUPWT))
learnSd <- sapply(learnNum, function(x) wt.sd(x, learnNum$MARSUPWT))
learnMax <- sapply(learnNum, max)
learnMin <- sapply(learnNum, min)
learnNa <- sapply(learn, function(x) sum(is.na(x)))

### TO PLOTSHOW THE % OF A CATEGORY EARNING MORE THAN 50000+, CHANGE CAPLOSS WITH THE DESIRED CATEGORY
# Weight the results according to the instance weight
xwNum <- learnNum$SAVN * learnNum$MARSUPWT
# Group by parameter (ex. AAGE) and use SAVW = sum(SAVN * MARSUPWT)/sum(MARSUPWT) to compute the appropriate P+
df <- aggregate(xwNum ~ AAGE, learnNum, sum)
df$weigth <- aggregate(MARSUPWT ~ AAGE, learnNum, sum)$MARSUPWT
df$SAVW <- df$xwNum / df$weigth
# REMOVE USELESS STUFF
df <- df[, -2]
#Plot
library(ggplot2)
ggplot(df, aes(x=AAGE, y=SAVW-0.062)) + 
  geom_point(size=log(df$weigth, base=1000)) + 
  geom_smooth() + 
  xlab("AAGE") + 
  ylab("P+")

# SOME OLD STUFF THAT I KEEP HERE FOR NO REASON
#learnStats <- sapply(learnNum, function(x) aggregate(learnNum$SAVN, by=list("x" = x), FUN = mean))

### CATEGORICAL VARIABLES
learnCat <- learn[,!sapply(learn, is.numeric)]
learnCat$SAVN <- learnNum$SAVN
learnCat$MARSUPWT <- learnNum$MARSUPWT
xwCat <- learnCat$SAVN * learnNum$MARSUPWT

# CALCULATE WHAT TO PLOT
df <- aggregate(xwCat ~ AMJOCC, learnCat, sum)
df$weigth <- aggregate(MARSUPWT ~ AMJOCC, learnCat, sum)$MARSUPWT
df$SAVW <- df$xwCat / df$weigth
# REMOVE USELESS STUFF
df <- df[, -2]
# PLOT
df$AMJOCC <-factor(df$AMJOCC, levels=df[order(df$SAVW), "AMJOCC"]) # This just reorders the result to have a nicer plot
ggplot(df, aes(x=AMJOCC, y=100*SAVW, label=paste(round(100*df$weigth/sum(df$weigth), digits=2), "%"))) +
  geom_bar(stat="identity", position="identity") +
  geom_label(aes(y = 6.2)) +
  coord_flip() +
  xlab("AMJOCC") +
  ylab("P+in %")

# ALTERNATE PLOTTING
# ggplot(learnCat, aes(x=SAV)) + geom_bar(aes(fill=ARACE))