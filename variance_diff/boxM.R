library(HDtest)

boxM <- function(x, y) {
    l <- levels(y)
    p <- ncol(x)
    n <- nrow(x)
    id0 <- which(y == l[1])
    id1 <- which(y == l[2])
    n0 <- length(id0)
    n1 <- length(id1)
    x0 <- x[id0, ]
    x1 <- x[id1, ]
    s0 <- cov(x0)
    s1 <- cov(x1)
    s <- (s0 * (n0 - 1) + s1 * (n1 - 1)) / (n - 2)
    M <- (n - 2) * log(det(s)) -
        (n0 - 1) * log(det(s0)) - (n1 - 1) * log(det(s1))
    c <- (1 / (n0 - 1) + 1 / (n1 - 1) - 1 / (n - 2)) * (2 * p ^ 2 + 3 * p - 1) /
        (6 * (p + 1))
    df <- p * (p + 1) / 2
    pchisq(M * (1 - c), df, lower.tail = FALSE)
}

testCov_wrapper <- function(x, y, method = "ALL") {
    l <- levels(y)
    id0 <- which(y == l[1])
    id1 <- which(y == l[2])
    x0 <- x[id0, ]
    x1 <- x[id1, ]
    testCov(x0, x1, method = method)
}

bc <- read.csv("../real_data/breast-cancer-wisconsin.data", header = FALSE)
y <- as.factor(bc[, 11])
x <- data.matrix(bc[, 2:10])
boxM(x, y)
testCov_wrapper(x, y)

## box M test
## 0

## The method 'Scott' does not support skewed data.
## $HD

## 	Two-Sample HD test

## data:  x0 and x1
## Statistic = 16.889, p-value < 2.2e-16
## alternative hypothesis: two.sided


## $CLX

## 	Two-Sample CLX test

## data:  x0 and x1
## Statistic = 285.23, p-value < 2.2e-16
## alternative hypothesis: two.sided


## $Scott

## 	Two-Sample Scott test

## data:  x0 and x1
## Statistic = 299.75, p-value < 2.2e-16
## alternative hypothesis: two.sided


## [[4]]

## 	Two-Sample LC test

## data:  x0 and x1
## Statistic = 162.68, p-value < 2.2e-16
## alternative hypothesis: two.sided

fm <- read.table("../real_data/Meter A.data")
y <- as.factor(fm[, 37])
x <- data.matrix(fm[, 1:36])
boxM(x, y)
testCov_wrapper(x, y)

## box M test
## 3.760423e-98

## The method 'Scott' does not support skewed data.
## $HD

## 	Two-Sample HD test

## data:  x0 and x1
## Statistic = 9.1159, p-value < 2.2e-16
## alternative hypothesis: two.sided


## $CLX

## 	Two-Sample CLX test

## data:  x0 and x1
## Statistic = 83.1, p-value < 2.2e-16
## alternative hypothesis: two.sided


## $Scott

## 	Two-Sample Scott test

## data:  x0 and x1
## Statistic = 0.49154, p-value = 0.623
## alternative hypothesis: two.sided


## [[4]]

## 	Two-Sample LC test

## data:  x0 and x1
## Statistic = -0.24114, p-value = 0.5953
## alternative hypothesis: two.sided

hd <- read.csv("../real_data/heart.csv")
y <- as.factor(hd[, 14])
x <- data.matrix(hd[, 1:13])
boxM(x, y)
testCov_wrapper(x, y)

## box M test
## 2.274729e-14

## The method 'Scott' does not support skewed data.
## $HD

## 	Two-Sample HD test

## data:  x0 and x1
## Statistic = 6.4002, p-value < 2.2e-16
## alternative hypothesis: two.sided


## $CLX

## 	Two-Sample CLX test

## data:  x0 and x1
## Statistic = 40.963, p-value = 2.68e-08
## alternative hypothesis: two.sided


## $Scott

## 	Two-Sample Scott test

## data:  x0 and x1
## Statistic = 0.087685, p-value = 0.9301
## alternative hypothesis: two.sided


## [[4]]

## 	Two-Sample LC test

## data:  x0 and x1
## Statistic = -2.0806, p-value = 0.9813
## alternative hypothesis: two.sided

sd <- read.table("../real_data/segment.dat")
sd <- sd[which(sd[, 20] %in% c(1, 4)), ]
y <- as.factor(sd[, 20])
x <- data.matrix(sd[, c(2, 6:19)])
boxM(x, y)
testCov_wrapper(x, y)

## box M test
## 0

## The method 'Scott' does not support skewed data.
## $HD

## 	Two-Sample HD test

## data:  x0 and x1
## Statistic = 18.965, p-value < 2.2e-16
## alternative hypothesis: two.sided


## $CLX

## 	Two-Sample CLX test

## data:  x0 and x1
## Statistic = 359.65, p-value < 2.2e-16
## alternative hypothesis: two.sided


## $Scott

## 	Two-Sample Scott test

## data:  x0 and x1
## Statistic = 62.338, p-value < 2.2e-16
## alternative hypothesis: two.sided


## [[4]]

## 	Two-Sample LC test

## data:  x0 and x1
## Statistic = 48.398, p-value < 2.2e-16
## alternative hypothesis: two.sided

si <- read.table("../real_data/sat.trn")
si <- si[which(si[, 37] %in% c(1, 3)), ]
y <- as.factor(si[, 37])
x <- data.matrix(si[, 1:36])
boxM(x, y)
testCov_wrapper(x, y, method = "Scott")

## box M test
## 0

## Two-Sample HD test

## data:  x0 and x1
## Statistic = 25.336, p-value < 2.2e-16
## alternative hypothesis: two.sided

## Two-Sample CLX test

## data:  x0 and x1
## Statistic = 641.9, p-value < 2.2e-16
## alternative hypothesis: two.sided

## The method 'Scott' does not support skewed data.

## 	Two-Sample Scott test

## data:  x0 and x1
## Statistic = 293.92, p-value < 2.2e-16
## alternative hypothesis: two.sided
