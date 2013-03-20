options <- commandArgs(trailingOnly = TRUE)
input = options[1]
data = as.matrix(read.csv(input, header=FALSE))
data = data[, -ncol(data)] # drop last column

xLabels <- c(1:ncol(data))
yLabels <- c(1:nrow(data))

reverse <- nrow(data) : 1
yLabels <- yLabels[reverse]
data <- data[reverse,]

layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
ColorRamp <- rgb(seq(1,1,length=256),
                 seq(1,0,length=256),
                 seq(1,0,length=256))

image(1:length(xLabels), 1:length(yLabels), t(data), col=ColorRamp, xlab="",
      ylab="", axes=FALSE)
axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1, cex.axis=0.7)