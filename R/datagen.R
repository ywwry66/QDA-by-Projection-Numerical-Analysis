## This is used to generate independent compound symmetric/AR samples

datagen <- function(n=100, p=100, rho=0, type =1) {
    ## n=sample size, p=dimension, rho=correlation parameter,
    ## type 1=compound symmetry and type 2=AR
    x <- matrix(rnorm(n * p), n, p)

    if (rho == 0) return(x)   #IID Gaussian design


    if (type == 1) {
        a <- sqrt(rho / (1 - rho))
        w <- rnorm(n) %*% t(rep(1, p))
        x <- (x + a * w) / sqrt(1 + a ^ 2)
        return(x)
    }
    if (type == 2) {
        a <- sqrt(rho ^ 2 / (1 - rho ^ 2))
        for (i in 2:p) {
            x[, i] <- (x[, i] + a * x[, i - 1]) / sqrt(1 + a ^ 2)
        }
        return(x)
    }
}
