#!/usr/bin/Rscript

trials <- c("perso_no", "vsids_no", "vsids_geom", "vsids_luby")
idxs <- c(1,2,3,4)
lim <- 30.0
# TODO add pvsids_no, pvsids_geom, pvsids_luby
rdata <- read.table("data/res", head=TRUE, sep=",")
data <- rdata[rdata$trial %in% trials,]
data$trial <- factor(data$trial)

# Individual scatter plots
scplot <- function(nm, adt) {
    dt <- adt[adt$trial == nm,]
    smoothScatter(dt$pop_size, dt$time,xlab="Taille de la population",ylab="Temps",main=nm)
    points(dt$pop_size, dt$time)
}

for (i in idxs) {
    pdf(paste("output/",trials[i],".pdf",sep=""))
    scplot(trials[i],data)
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
    barplot(mx, col=idxs, width=2, beside=TRUE, xlab="Taille de la population", ylab="Nombre de dépassements")
}

pdf("output/over.pdf")
overplot()
dev.off()

# Common scatter for less than 1 second
cscplot <- function(id) {
    dt <- data[data$trial == trials[id],]
    points(dt$pop_size, dt$time, col=id)
}

pdf("output/small.pdf")
plot(1, type="n", xlab="Taille de la population", ylab="Temps", xlim=c(8,15), ylim=c(0,0.05))
for (i in idxs) {
    cscplot(i)
}
dev.off()

# Mean
meanplot <- function(id) {
    dt <- data[data$trial == trials[id],]
    ag <- aggregate(time ~ pop_size, dt, mean)
    lines(ag$pop_size, ag$time, col=id)
}

pdf("output/mean.pdf")
xl = c(min(data$pop_size), max(data$pop_size))
plot(1, type="n", xlab="Taille de la population", ylab="Temps moyen", xlim=xl, ylim=c(0,lim))
for (i in idxs) {
    meanplot(i)
}
dev.off()

# Standart deviation
sdplot <- function(id) {
    dt <- data[data$trial == trials[id],]
    ag <- aggregate(time ~ pop_size, dt, sd)
    lines(ag$pop_size, ag$time, col=id)
}

pdf("output/sd.pdf")
xl = c(min(data$pop_size), max(data$pop_size))
plot(1, type="n", xlab="Taille de la population", ylab="Écart type", xlim=xl, ylim=c(0,lim/2))
for (i in idxs) {
    sdplot(i)
}
dev.off()


