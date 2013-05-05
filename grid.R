library(plyr)
library(beanplot)
library(doBy)
library(ggplot2)

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This is does the summary; it's not easy to understand...
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun= function(xx, col, na.rm) {
                           c( N    = length2(xx[,col], na.rm=na.rm),
                              mean = mean   (xx[,col], na.rm=na.rm),
                              sd   = sd     (xx[,col], na.rm=na.rm)
                              )
                          },
                    measurevar,
                    na.rm
             )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean"=measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}


# Read the data
raw <- read.table(commandArgs(TRUE)[1], sep=" ", header=FALSE, col.names=c("TestName", "Actors", "Time", "CPUTime", "GridSize", "Clients", "Completion", "Specific"), fill=TRUE)

# Draw the beanplot for the overall speedup
png("images/grid-beanplot.png")
beanplot(Time ~ Actors, data=raw, what = c(1,1,1,0), log="", ylab="Time", xlab="Actors", las=2, ylim=c(0, 500000))
dev.off()

# Draw the line graph for the overall speedup
data <- summarySE(raw, measurevar="Time", groupvars=c("Actors", "GridSize"))
data$Actors <- as.character(data$Actors)
png("images/grid-lines.png")
ggplot(data, aes(x=GridSize, y=Time, colour=Actors)) + geom_errorbar(aes(ymin=Time-se, ymax=Time+se), width=.1) + geom_line() + geom_point()
dev.off()
