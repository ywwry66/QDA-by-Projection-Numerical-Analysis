## This function generates training data, test data, and
## oracle rules for simulations

library(MASS)

datagen_sl <- function(mu0, mu1, sigma0, sigma1,
                       m=100, ntest=100, n=c(100, 150, 200, 250, 300)) {
    len_n <- length(n)
    ## generating y's
    y <- list()
    ynew <- list()
    for (i in 1:len_n) {
        y[[i]] <- c(rep(0, n[i]), rep(1, n[i]))
        ynew[[i]] <- c(rep(0, ntest), rep(1, ntest))
    }
    ## generating x's
    x <- list()
    xnew <- list()
    for (i in 1:len_n) {
        x[[i]] <- list()
        xnew[[i]] <- list()
        for (j in 1:m) {
            x[[i]][[j]] <- rbind(mvrnorm(n[i], mu0, sigma0),
                                 mvrnorm(n[i], mu1, sigma1))
            xnew[[i]][[j]] <- rbind(mvrnorm(ntest, mu0, sigma0),
                                    mvrnorm(ntest, mu1, sigma1))
        }
    }
    ## generating oracle rules
    ldsigma0 <- log(det(sigma0))
    ldsigma1 <- log(det(sigma1))
    sigma0_inv <- solve(sigma0)
    sigma1_inv <- solve(sigma1)
    oracle_rule <- function(xnew)
        as.numeric(-ldsigma0 -
                   t(xnew - mu0) %*% sigma0_inv %*% (xnew - mu0) <
                   -ldsigma1 -
                   t(xnew - mu1) %*% sigma1_inv %*% (xnew - mu1))
    ## value returned
    return(list(x = x, y = y, xnew = xnew, ynew = ynew,
                oracle_rule = oracle_rule))
}
