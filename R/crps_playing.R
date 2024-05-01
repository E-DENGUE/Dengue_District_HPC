
library(scoringutils)
sd.vals <- c(0.5,1,2,3,4,5,6,7,8)
crps_test <- sapply(sd.vals, function(X){
  crps_sample(c(3,5),  matrix(c(rnorm(1000,5,X),rnorm(1000,5,X)), nrow=2,byrow=T))
})

plot(sd.vals,crps_test[1,])

plot(sd.vals,crps_test[2,])



hist(rnorm(1000,mean=5,1), xlim=c(-20,20))
abline(v=3)

hist(rnorm(1000,mean=5,3), xlim=c(-20,20))
abline(v=3)

hist(rnorm(1000,mean=5,2), xlim=c(-20,20))
abline(v=3)

hist(rnorm(1000,mean=5,4),xlim=c(-20,20))
abline(v=3)
