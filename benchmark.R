library(plyr)
library(beanplot)
library(doBy)

# Save to PNG given in argument
png(commandArgs(TRUE)[2])

# Read the data
bench <- read.table(commandArgs(TRUE)[1], sep=" ", header=FALSE, col.names=c("TestName", "Actors", "Time", "CPUTime"), fill=TRUE)

# Sort by actors
bench <- ddply(bench, ~ Actors + Time, transform)

# Normalize
bench <- ddply(bench, ~ TestName, transform, Time = Time[Actors == 0] / Time)

# Drop elements with only one actor
bench <- subset(bench, Actors != 0, c(Actors, Time))

# Draw the beanplot
beanplot(Time ~ Actors, data=bench, what = c(1,1,1,0), log="", ylab="Speedup", xlab="Actors", las=2) # ylim=c(0.5, 4))

# Finish the image
dev.off()

# To plot line bars:
# dfc <- summarySE(bench, measurevar="Time", groupvars=c("Actors"))
# ggplot(dfc, aes(x=Actors, y=Time)) + geom_errorbar(aes(ymin=Time-se, ymax=Time+se), width=.1) + geom_line() + geom_point()
