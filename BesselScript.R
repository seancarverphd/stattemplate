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