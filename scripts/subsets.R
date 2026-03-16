# change to root (parent ".." of "/scripts" directory)
setwd("/../path/to/project/")

# packages to include
library(leaps)  # regsubsets
library(multitaper)  # spec.mtm

# all subsets
idx <- which(names(train) == "TCMNOM_Y10")
x <- train[, -idx]
y <- train[, idx]
subsets <- summary(regsubsets(x, y, nvmax=30, method="exhaustive"))

# plot adjusted R^2
which.max(subsets$adjr2)
max(subsets$adjr2)
plot(subsets$adjr2, xlab="Number of predictors in model (d)",
     ylab="Adjusted R^2", main="Best Subsets")
lines(subsets$adjr2, xlab="Number of predictors in model (d)",
     ylab="Adjusted R^2", main="Best Subsets", type='l', col="blue")

subsets$which[1:2,]

# fit model
fit2 <- lm(TCMNOM_Y10 ~ TCMNOM_Y7 + TCMNOM_Y20, data=train)

# extract residuals
resids  <- fit2$residuals

# GM assumption) i. linearity, TCMNOM_Y7
par(mfrow=c(1,2))
plot(train$TCMNOM_Y7, resids, xlab="TCMNOM_Y7", ylab="Residuals")
lm(y~x, data.frame(x=train$TCMNOM_Y7, y=resids))
abline(0, 0, col="blue")
pr2 <- resids + fit2$coefficients[2] * train$TCMNOM_Y7
plot(train$TCMNOM_Y7, pr2, xlab="TCMNOM_Y7", ylab="Partial Residuals")
lm(y~x, data.frame(x=train$TCMNOM_Y7, y=pr2))
abline(0, 0.565, col="blue")
par(mfrow=c(1,1))

# GM assumption) i. linearity, TCMNOM_Y20
par(mfrow=c(1,2))
plot(train$TCMNOM_Y20, resids, xlab="TCMNOM_Y20", ylab="Residuals")
lm(y~x, data.frame(x=train$TCMNOM_Y20, y=resids))
abline(0, 0, col="blue")
pr3 <- resids + fit2$coefficients[3] * train$TCMNOM_Y20
plot(train$TCMNOM_Y20, pr3, xlab="TCMNOM_Y20", ylab="Partial Residuals")
lm(y~x, data.frame(x=train$TCMNOM_Y20, y=pr3))
abline(0, 0.4325, col="blue")
par(mfrow=c(1,1))

# GM assumption) ii. autocorrelation
bgtest(TCMNOM_Y10 ~ TCMNOM_Y7 + TCMNOM_Y20, data=train, order=5, type="F")

# GM assumption) ii. periodicity
mtm <- spec.mtm(resids, jackknife=F, nw=15, k=30, Ftest=T, plot=T, deltat=1/length(resids),
                main="MTM (Spectral Density) Plot for TCMNOM_Y10")$mtm
week <- 5
month <- 22
mtm$Ftest[c(week, month)]
qf(0.975, 2, 58)

# GM assumption) iii. heteroscedasticity
plot(fit2, 1)

# normality of residuals
plot(fit2, 2)

# coefficients
summary(fit2)
confint(fit2)

# SSE
sqrt(mean(fit2$residuals^2))