# change to root (parent ".." of "/scripts" directory)
setwd("/../path/to/project/")

NUM_REPLICATES <- 10000

# bootstrap
set.seed(1)
rmse2 <- sapply(1:NUM_REPLICATES, function(i) {
  boot <- test[sample.int(nrow(test), replace=T), ]
  resids <- predict.lm(fit2, boot) - boot$TCMNOM_Y10
  sqrt(mean(resids^2))
})
rmse31 <- sapply(1:NUM_REPLICATES, function(i) {
  boot <- test[sample.int(nrow(test), replace=T), ]
  resids <- predict.lm(fit31, boot) - boot$TCMNOM_Y10
  sqrt(mean(resids^2))
})

summary(rmse2)
summary(rmse31)

write(rmse2, "misc/RMSE2.txt")
write(rmse31, "misc/RMSE31.txt")

# Histogram Grey Color
hist(rmse2, col=rgb(0,0,1,0.5), breaks=seq(from=0.007, to=0.01, by=5e-5), ylim=c(0, 800), xlim=c(0.007, 0.01),
     main="Histograms of Bootstrapped Statistics", xlab="Estimated RMSE")
hist(rmse31, col=rgb(1,0,0,0.5), breaks=seq(from=0.007, to=0.01, by=5e-5), ylim=c(0, 800), xlim=c(0.007, 0.01),
     add=T)
box()

# mean RMSEs
mean(rmse2)
mean(rmse31)