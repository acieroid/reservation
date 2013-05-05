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

# Sort by actors
#bench <- ddply(raw, ~ Actors + Time, transform)

# Normalize
#bench <- ddply(bench, ~ TestName, transform, Time = Time[Actors == 0] / Time)

# Drop elements with only one actor
#bench <- subset(bench, Actors != 0, c(Actors, Time))

# Draw the beanplot for the overall speedup
png("images/overall-beanplot.png")
beanplot(Time ~ Actors, data=raw, what = c(1,1,1,0), log="", ylab="Time", xlab="Actors", las=2, ylim=c(0, 500000))
dev.off()

# Draw the line graph for the overall speedup
data <- summarySE(raw, measurevar="Time", groupvars=c("Actors"))
png("images/overall-lines.png")
ggplot(data, aes(x=Actors, y=Time)) + geom_errorbar(aes(ymin=Time-se, ymax=Time+se), width=.1) + geom_line() + geom_point()
dev.off()

# Increase of performance given the number of clients
data <- summarySE(raw, measurevar="Time", groupvars=c("Actors", "Clients"))
png("images/speedup-clients.png")
data$Clients <- as.character(data$Clients)
ggplot(data, aes(x=Actors, y=Time, colour=Clients)) + geom_errorbar(aes(ymin=Time-se, ymax=Time+se), width=.1) + geom_point() + geom_path(aes(colour=Clients))
dev.off()

# Increase of performance given the number of actors, depending on the number of clients
data <- summarySE(raw, measurevar="Time", groupvars=c("Actors", "Clients"))
png("images/speedup-clients-actors.png")
data$Actors <- as.character(data$Actors)
ggplot(data, aes(x=Clients, y=Time, colour=Actors)) + geom_errorbar(aes(ymin=Time-se, ymax=Time+se), width=3) + geom_point() + geom_line()
dev.off()

# Same without the 1000 clients (no error bars for readability)
data <- data[data$Clients != 1000,]
png("images/speedup-clients-actors-no-1000-clients.png")
ggplot(data, aes(x=Clients, y=Time, colour=Actors)) + geom_point() + geom_line()
dev.off()

# Increase of perf. given the number of actors, depending on the completion percentage
data <- summarySE(raw, measurevar="Time", groupvars=c("Actors", "Completion"))
png("images/speedup-completion-actors.png")
data$Actors <- as.character(data$Actors)
ggplot(data, aes(x=Completion, y=Time, colour=Actors)) + geom_errorbar(aes(ymin=Time-se, ymax=Time+se), width=3) + geom_point() + geom_line()
dev.off()

# Increase of perf. given the number of actors, depending on the specific requests percentage
data <- summarySE(raw, measurevar="Time", groupvars=c("Actors", "Specific"))
png("images/speedup-specific-actors.png")
data$Actors <- as.character(data$Actors)
ggplot(data, aes(x=Specific, y=Time, colour=Actors)) + geom_errorbar(aes(ymin=Time-se, ymax=Time+se), width=3) + geom_point() + geom_line()
dev.off()

# Same without error bars
data <- summarySE(raw, measurevar="Time", groupvars=c("Actors", "Specific"))
png("images/speedup-specific-actors-no-error.png")
data$Actors <- as.character(data$Actors)
ggplot(data, aes(x=Specific, y=Time, colour=Actors)) + geom_point() + geom_line()
dev.off()

# Same without error bars, without 1000 clients
data <- summarySE(raw[raw$Clients != 1000,], measurevar="Time", groupvars=c("Actors", "Specific"))
png("images/speedup-specific-actors-no-error-no-1000-clients.png")
data$Actors <- as.character(data$Actors)
ggplot(data, aes(x=Specific, y=Time, colour=Actors)) + geom_point() + geom_line()
dev.off()

# Only with 1000 clients
data <- summarySE(raw[raw$Clients == 1000,], measurevar="Time", groupvars=c("Actors", "Specific"))
png("images/speedup-specific-actors-no-error-1000-clients.png")
data$Actors <- as.character(data$Actors)
ggplot(data, aes(x=Specific, y=Time, colour=Actors)) + geom_point() + geom_line()
dev.off()
