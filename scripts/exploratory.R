# change to root (parent ".." of "/scripts" directory)
setwd("/../path/to/project/")

# packages to include
library(MASS)  # boxcox
library(moments)  # skewness, kurtosis
library(pcaPP)  # cor.fk

# skewness
sapply(train, skewness)

# excess kurtosis
sapply(train, kurtosis) - 3

# PRIME_NA
table(train$PRIME_NA)

# FF_O
table(round(train$FF_O, 2))  # rounded to fix limited-precision representation
which.max(train$FF_O)
row.names(train)[which.max(train$FF_O)]

# TB_WK4
table(round(train$TB_WK4, 2))  # rounded to fix limited-precision representation
which.max(train$TB_WK4)
row.names(train)[which.max(train$TB_WK4)]

# TCMNOM_M1
table(round(train$TCMNOM_M1, 2))  # rounded to fix limited-precision representation
which.max(train$TCMNOM_M1)
row.names(train)[which.max(train$TCMNOM_M1)]

# TB_M6
table(round(train$TB_M6, 2))  # rounded to fix limited-precision representation
which.min(train$TB_M6)
row.names(train)[which.min(train$TB_M6)]

# timeseries plots
par(mfrow=c(2,2))  # 2x2 plot layout
plot(train$FF_O, type='l', col="blue", xlab="Observation Number", ylab="FF_O")
plot(train$TB_WK4, type='l', col="blue", xlab="Observation Number", ylab="TB_WK4")
plot(train$TCMNOM_M1, type='l', col="blue", xlab="Observation Number", ylab="TCMNOM_M1")
plot(train$TB_M6, type='l', col="blue", xlab="Observation Number", ylab="TB_M6")
par(mfrow=c(1,1))  # reset to 1x1 layout

# response
plot(train$TCMNOM_Y10, type='l', col="blue", xlab="Observation Number", ylab="TCMNOM_Y10")
hist(train$TCMNOM_Y10, col="gray", main="TCMNOM_Y10", xlab="value (% change in yield rate)")

# Pearson's rho
correlations <- cor(train)
fix(correlations)
write.csv(correlations, "correlations.csv")

# Kendall's tau
correlations2 <- cor.fk(train)
fix(correlations2)
write.csv(correlations2, "correlations2.csv")

# variance inflation factors
sapply(setdiff(colnames(train), "TCMNOM_Y10"), function(name) {
  form <- as.formula(paste0(name, " ~ ."))
  rsq <- summary(lm(form, data=train))$r.squared
  1 / (1 - rsq)
})

# Box-Cox test
EPSILON <- 1e-6
delta <- EPSILON - min(train$TCMNOM_Y10)
boxcox(TCMNOM_Y10 ~ ., data=train + delta)

# response ~ predictor plots
par(mfrow=c(3,3))  # 3x3 plot layout
y <- train$TCMNOM_Y10
for (name in c("LTAVG_Y10P", "TCMII_Y10", "TCMII_Y30",
               "TCMII_Y20", "TCMII_Y7", "TCMII_Y5",
               "TB_Y1", "TB_M6", "TCMNOM_M6")) {
  x <- train[, name]
  scatter.smooth(x, y, xlab=name, ylab="TCMNOM_Y10", span=2/3)
  abline(lm(y~x, data=data.frame(x, y)), col="blue")
}
par(mfrow=c(1,1))  # reset to 1x1 layout
