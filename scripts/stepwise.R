# change to root (parent ".." of "/scripts" directory)
setwd("/../path/to/project/")

# package to include
library(leaps)

# bidirectional search
steps <- summary(regsubsets(TCMNOM_Y10 ~ .*., data=train, nvmax=30, method="seqrep"))

# plot adjusted R^2
which.max(steps$adjr2)
max(steps$adjr2)
plot(steps$adjr2, xlab="Number of features in model (d)",
     ylab="Adjusted R^2", main="Stepwise Selection")
lines(subsets$adjr2, xlab="Number of features in model (d)",
      ylab="Adjusted R^2", main="Stepwise Selection", type='l', col="blue")

# which features were chosen?
sapply(1:4, function(d) {
  w <- steps$which[d,]
  w[w]
})

# model with lowest adjusted R^2
steps31 <- steps$which[31,]
names31 <- names(steps31[steps31])[-1]
form31 <- as.formula(paste0("TCMNOM_Y10 ~ ", paste(names31, collapse="+")))
fit31 <- lm(form31, data=train)
summary(fit31)
plot(fit31)
sqrt(mean(fit31$residuals^2))