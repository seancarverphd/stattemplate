sdn1 <- function(x) {
  n <- length(x)
  dev <- x - mean(x)
  dev2 <- dev^2
  sumdev2 <- sum(dev2)
  avedev2 <- sumdev2/(n-1)
  stddev <- sqrt(avedev2)
  return(stddev)
}

sdn0 <- function(x) {
  n <- length(x)
  dev <- x - mean(x)
  dev2 <- dev^2
  sumdev2 <- sum(dev2)
  avedev2 <- sumdev2/n
  stddev <- sqrt(avedev2)
  return(stddev)
}

estimates.of.sd <- function(pop,n,K,seed=1) {
  err0 <- c()
  err1 <- c()
  for (k in 1:K) {
    err0 <- c(err0, sdn0(sample(pop,n))-sdn0(pop))
    err1 <- c(err1, sdn1(sample(pop,n))-sdn0(pop))
  }
  lab0 <- rep('Error n', K)
  lab1 <- rep('Error n-1',K)
  labs <- rbind(cbind(lab0),cbind(lab1))
  colnames(labs) <- 'lab'
  errs <- rbind(cbind(err0),cbind(err1))
  colnames(errs) <- 'err'
  df <- data.frame(errs, labs)
  ggplot(df, aes(x=lab,y=err)) + geom_violin() + geom_boxplot(width=0.2) + stat_summary(fun.y=mean, geom="point", size=2, color="red")
}

diamonds.price.sample.of.n <- function(sample.size) {
  return(sample(diamonds$price, sample.size))
}

m.statistics.of.n <- function(statistic,number.of.samples,sample.size) {
  stats <- c()
  for (k in 1:number.of.samples) {
    stats <- c(stats, statistic(diamonds.price.sample.of.n(sample.size)))
  }
  return(stats)
}

set.seed(12)
m <- 1000
stats <- cbind()
labels <- cbind()
levels <- c(1,4,16,64,256,1024)
for (i in c(1,4,16,64,256,1024)) {
  new.stats <- cbind(m.statistics.of.n(statistic=mean, number.of.samples=m, sample.size=i))
  new.labels <- cbind(rep(as.character(i), m))
  stats <- rbind(stats, new.stats)
  labels <- rbind(labels, new.labels)
}
df <- data.frame(stats,labels)
df$labels <- factor(df$labels,levels=as.character(levels))
ggplot(df, aes(x=labels, y=stats)) + geom_violin() + geom_boxplot(width=0.1) + xlab("Sample Size") + ylab("Sample Mean") + stat_summary(fun.y=mean, geom="point", size=2, color="red") + geom_hline(yintercept=mean(diamonds$price))
