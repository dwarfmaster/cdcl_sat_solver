#!/usr/bin/Rscript

trials <- c("perso_no", "vsids_no", "vsids_geom", "vsids_luby", "pvsids_no", "pvsids_geom", "pvsids_luby")
idxs <- c(1,2,3,4,5,6,7)
colors <- rainbow(length(idxs))
lim <- 60.0
rdata <- read.table("data/res", head=TRUE, sep=",")
data <- rdata[rdata$trial %in% trials,]
data$trial <- factor(data$trial)

# Individual scatter plots
scplot <- function(i, adt) {
    dt <- adt[adt$trial == trials[i],]
    smoothScatter(dt$pop_size, dt$time,xlab="Taille de la population",ylab="Temps",main=trials[i])
    points(dt$pop_size, dt$time,col=colors[i])
}

for (i in idxs) {
    pdf(paste("output/",trials[i],".pdf",sep=""))
    scplot(i,data)
    dev.off()
}

# Barplot of those over limit time
countOver <- function(v) {
    return (length(which(v >= lim)))
}

overplot <- function() {
    # Count the values over
    ag <- aggregate(time ~ trial+pop_size, data, countOver)
    # Order them
    od <- ag[with(ag, order(trial,pop_size)),]
    # Create the matrix
    mx <- matrix(od$time,ncol=length(unique(od$pop_size)),byrow=TRUE)
    colnames(mx) <- unique(od$pop_size)
    rownames(mx) <- levels(od$trial)
    # Plot
    barplot(mx, col=colors, width=2, beside=TRUE, xlab="Taille de la population", ylab="Nombre de dépassements")
    legend('topleft',levels(od$trial),fill=colors)
}

pdf("output/over.pdf")
overplot()
dev.off()

# Common scatter for less than 1 second
cscplot <- function(id) {
    dt <- data[data$trial == trials[id],]
    points(dt$pop_size, dt$time, col=colors[i])
}

pdf("output/small.pdf")
par(mar=c(5.1,4.1,4.1,8.1))
plot(1, type="n", xlab="Taille de la population", ylab="Temps", xlim=c(8,25), ylim=c(0,0.05))
for (i in idxs) {
    cscplot(i)
}
legend('right', inset=c(-0.32,0), trials,col=colors,pch=1,xpd=TRUE)
dev.off()

# Mean
meanplot <- function(id) {
    dt <- data[data$trial == trials[id],]
    ag <- aggregate(time ~ pop_size, dt, mean)
    lines(ag$pop_size, ag$time, col=colors[id])
}

pdf("output/mean.pdf")
xl = c(min(data$pop_size), max(data$pop_size))
plot(1, type="n", xlab="Taille de la population", ylab="Temps moyen", xlim=xl, ylim=c(0,lim))
legend('topleft',trials,col=colors,lty=1)
for (i in idxs) {
    meanplot(i)
}
dev.off()

# Standart deviation
sdplot <- function(id) {
    dt <- data[data$trial == trials[id],]
    ag <- aggregate(time ~ pop_size, dt, sd)
    lines(ag$pop_size, ag$time, col=colors[id])
}

pdf("output/sd.pdf")
xl = c(min(data$pop_size), max(data$pop_size))
plot(1, type="n", xlab="Taille de la population", ylab="Écart type", xlim=xl, ylim=c(0,lim/2))
legend('topleft',trials,col=colors,lty=1)
for (i in idxs) {
    sdplot(i)
}
dev.off()


