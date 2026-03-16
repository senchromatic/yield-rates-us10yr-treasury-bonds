# change to root (parent ".." of "/scripts" directory)
setwd("/../path/to/project/")

# packages to include
library(lmtest)  # bgtest
library(stats)  # ks.test
library(xts)  # diff.xts

# remove missing rows
daily_rates <- read.csv("data/daily_rates.csv")
no_missing <- na.omit(daily_rates)
cleaned <- no_missing[, !names(daily_rates) %in% "X"]
write.csv(cleaned, "data/cleaned.csv", row.names=F)

# label rows using date column
dates <- cleaned$date
formatted <- cleaned[, !names(daily_rates) %in% "date"]
row.names(formatted) <- dates

# compute arithmetic differences (% change)
diffs <- sapply(formatted, function(timeseries) {
  diff.xts(timeseries, arithmetic=T)
})
row.names(diffs) <- dates
diffs <- as.data.frame(na.omit(diffs))
write.csv(diffs, "data/diffs.csv")
write.csv(diffs, "data/diffs_nodates.csv", row.names=F)

# number of distinct values per predictor
distincts <- sapply(diffs, function(timeseries) length(table(timeseries)))
hist(distincts, main="Histogram", col="gray",
     xlab="Number of Distinct Values", ylab="Frequency (# of predictors)")

# Breusch-Godfrey test
bgtest(TCMNOM_Y10 ~ ., data=diffs, order=5, type="F")

# partitioning
n_obs <- nrow(diffs)
k_pred <- ncol(diffs)
train <- diffs[seq(from=1, to=n_obs, by=2), ]
test <- diffs[seq(from=2, to=n_obs, by=2), ]
write.csv(train, "data/train.csv")
write.csv(train, "data/train_nodates.csv", row.names=F)
write.csv(test, "data/test.csv")
write.csv(train, "data/test_nodates.csv", row.names=F)

# K-S test for distributions
sapply(colnames(diffs), function(name) {
  ks.test(train[, name], test[, name], alternative="two.sided")$p.value
})

# Breusch-Godfrey test on training subset
bgtest(TCMNOM_Y10 ~ ., data=train, order=5, type="F")

# Breusch-Godfrey test on test subset
bgtest(TCMNOM_Y10 ~ ., data=test, order=5, type="F")
