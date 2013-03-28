library(plyr)
library(beanplot)
library(doBy)

# Read the data
bench <- read.table("benchmark_results.txt", sep=" ", header=FALSE, col.names=c("TestName", "Actors", "Time"), fill=TRUE)

# Sort by actors
bench <- ddply(bench, ~ Actors + Time, transform)

# Normalize
bench <- ddply(bench, ~ TestName, transform, Time = Time[Actors == 1] / Time)

# Drop elements with only one actor
bench <- subset(bench, Actors != 1, c(Actors, Time))

# Draw the beanplot

beanplot(Time ~ Actors, data=bench, what = c(1,1,1,0), log="", ylab="Speedup", xlab="Actors", las=2)
