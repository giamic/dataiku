### UNIVARIATE AUDIT
library(SDMTools)

### NUMERICAL VARIABLES
learnNum <- learn[,sapply(learn, is.numeric)]
### VARIOUS STATISTICAL CALCULATIONS
#learnWMean <- sapply(learnNum, function(x) wt.mean(x, learnNum$MARSUPWT))
#learnSd <- sapply(learnNum, function(x) wt.sd(x, learnNum$MARSUPWT))
#learnMax <- sapply(learnNum, max)
#learnMin <- sapply(learnNum, min)
#learnNa <- sapply(learn, function(x) sum(is.na(x)))

### TO PLOTSHOW THE % OF A CATEGORY EARNING MORE THAN 50000+, CHANGE CAPLOSS WITH THE DESIRED CATEGORY
# Weight the results according to the instance weight
xwNum <- learnNum$SAVN * learnNum$MARSUPWT
# Group by parameter (ex. DIVYES) and use SAVW = sum(SAVN * MARSUPWT)/sum(MARSUPWT) to compute the appropriate P+
df <- aggregate(xwNum ~ DIVYES, learnNum, sum)
df$weigth <- aggregate(MARSUPWT ~ DIVYES, learnNum, sum)$MARSUPWT
df$SAVW <- df$xwNum / df$weigth
# REMOVE USELESS STUFF
df <- df[, -2]
#Plot
library(ggplot2)
ggplot(df, aes(x=DIVYES, y=SAVW)) + 
  geom_point(size=log(df$weigth, base=1000)) + 
  geom_smooth() + 
  xlab("DIVYES") + 
  ylab("P+")

# SOME OLD STUFF THAT I KEEP HERE FOR NO REASON
#learnStats <- sapply(learnNum, function(x) aggregate(learnNum$SAVN, by=list("x" = x), FUN = mean))

### CATEGORICAL VARIABLES
learnCat <- learn[,!sapply(learn, is.numeric)]
learnCat$SAVN <- learnNum$SAVN
learnCat$MARSUPWT <- learnNum$MARSUPWT
xwCat <- learnCat$SAVN * learnNum$MARSUPWT

# CALCULATE WHAT TO PLOT
df <- aggregate(xwCat ~ HHDFMX, learnCat, sum)
df$weigth <- aggregate(MARSUPWT ~ HHDFMX, learnCat, sum)$MARSUPWT
df$SAVW <- df$xwCat / df$weigth
# REMOVE USELESS STUFF
df <- df[, -2]
# PLOT
df$HHDFMX <-factor(df$HHDFMX, levels=df[order(df$SAVW), "HHDFMX"]) # This just reorders the result to have a nicer plot
ggplot(df, aes(x=HHDFMX, y=SAVW, label=paste(round(100*df$weigth/sum(df$weigth), digits=2), "%"))) +
  geom_bar(stat="identity") +
  geom_label(position="identity") +
  coord_flip() +
  xlab("HHDFMX") +
  ylab("P+")

# ALTERNATE PLOTTING
# ggplot(learnCat, aes(x=SAV)) + geom_bar(aes(fill=ARACE))