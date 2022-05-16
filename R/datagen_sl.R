## This function generates training data, test data, and
## oracle rules for simulations

library(MASS)

mvrt <- function(n = 1, mu, sigma, nu) {
    p <- length(mu)
    y <- mvrnorm(n, rep(0, p), sigma)
    u <- rchisq(n, nu)
    x <- y * matrix(rep(sqrt(u / nu), p), nrow = n) +
        matrix(rep(mu, n), nrow = n, byrow = TRUE)
    return(x)
}

datagen_sl <- function(mu0, mu1, sigma0, sigma1, t_dist = FALSE, nu = t_dist,
                       m=100, ntest=1000, n=c(200, 300, 400, 500, 600)) {
    len_n <- length(n)
    p <- length(mu0)
    n0 <- floor(n / 2)
    n1 <- n - n0
    ntest0 <- floor(ntest / 2)
    ntest1 <- ntest - ntest0
    ## generating y's
    y <- list()
    ynew <- list()
    for (i in 1:len_n) {
        y[[i]] <- c(rep(0, n0[i]), rep(1, n1[i]))
        ynew[[i]] <- c(rep(0, ntest0), rep(1, ntest1))
    }
    ## generating x's
    x <- list()
    xnew <- list()
    if (t_dist == TRUE) {
        for (i in 1:len_n) {
            x[[i]] <- list()
            xnew[[i]] <- list()
            for (j in 1:m) {
                x[[i]][[j]] <- rbind(mvrt(n0[i], mu0, sigma0, nu),
                                     mvrt(n1[i], mu1, sigma1, nu))
                xnew[[i]][[j]] <- rbind(mvrt(ntest0, mu0, sigma0, nu),
                                        mvrt(ntest1, mu1, sigma1, nu))
            }
        }
        ## generating oracle rules
        dsigma0 <- det(sigma0)
        dsigma1 <- det(sigma1)
        sigma0_inv <- solve(sigma0)
        sigma1_inv <- solve(sigma1)
        oracle_rule <- function(xnew)
            as.numeric((1 + t(xnew - mu0) %*% sigma0_inv %*%
                        (xnew - mu0) / nu) ^ (-(nu + p) / 2) /
                       sqrt(dsigma0) <
                       (1 + t(xnew - mu1) %*% sigma1_inv %*%
                        (xnew - mu1) / nu) ^ (-(nu + p) / 2) /
                       sqrt(dsigma1))
    } else {
        for (i in 1:len_n) {
            x[[i]] <- list()
            xnew[[i]] <- list()
            for (j in 1:m) {
                x[[i]][[j]] <- rbind(mvrnorm(n0[i], mu0, sigma0),
                                     mvrnorm(n1[i], mu1, sigma1))
                xnew[[i]][[j]] <- rbind(mvrnorm(ntest0, mu0, sigma0),
                                        mvrnorm(ntest1, mu1, sigma1))
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
    }
    ## value returned
    return(list(x = x, y = y, xnew = xnew, ynew = ynew,
                oracle_rule = oracle_rule))
}
