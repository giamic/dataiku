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

### TO PLOTSHOW THE % OF A CATEGORY EARNING MORE THAN 50000+, CHANGE AAGE WITH THE DESIRED CATEGORY
# Weight the results according to the instance weight
xw <- learnNum$SAVN * learnNum$MARSUPWT
# Group by parameter (ex. AAGE) and use SAVW = sum(SAVN * MARSUPWT)/sum(MARSUPWT) to compute the appropriate P+
df <- aggregate(xw ~ AAGE, learnNum, sum)
df$weigth <- aggregate(MARSUPWT ~ AAGE, learnNum, sum)$MARSUPWT
df$SAVW <- df$xw / df$weigth
# Remove a useless line
df <- df[, -2]
#Plot
library(ggplot2)
ggplot(df, aes(x=AAGE, y=SAVW)) + 
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
xw <- learnCat$SAVN * learnNum$MARSUPWT

# CALCULATE WHAT TO PLOT
df <- aggregate(xw ~ PENATVTY, learnCat, sum)
df$weigth <- aggregate(MARSUPWT ~ PENATVTY, learnCat, sum)$MARSUPWT
df$SAVW <- df$xw / df$weigth
df <- df[, -2]
df$PENATVTY <-factor(df$PENATVTY, levels=df[order(df$SAVW), "PENATVTY"])

# PLOT
ggplot(df, aes(x=PENATVTY, y=SAVW, label=paste(round(100*df$weigth/sum(df$weigth), digits=2), "%"))) +
  geom_bar(stat="identity") +
  geom_label(position="identity") +
  coord_flip() +
  xlab("PENATVTY") +
  ylab("P+")

# ALTERNATE PLOTTING
# ggplot(learnCat, aes(x=SAV)) + geom_bar(aes(fill=ARACE))