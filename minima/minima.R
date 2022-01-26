f <- function(r, g) {
    if (g == 1) {
        return(pnorm(-abs(r) / 2))
    } else {
        return(1 / 2 * (1 +
                        pnorm((r - g * sqrt(r ^ 2 + (g ^ 2 - 1) * log(g ^ 2))) / (g ^ 2 - 1)) -
                        pnorm((r + g * sqrt(r ^ 2 + (g ^ 2 - 1) * log(g ^ 2))) / (g ^ 2 - 1)) +
                        pnorm((r * g + sqrt(r ^ 2 + (g ^ 2 - 1) * log(g ^ 2))) / (g ^ 2 - 1)) -
                        pnorm((r * g - sqrt(r ^ 2 + (g ^ 2 - 1) * log(g ^ 2))) / (g ^ 2 - 1))))
    }
}

h <- function(r, g) {
    if (g == 0) {
        return(pnorm(-abs(r) / 2))
    } else {
        return(1 / 2 * (1 +
                        pnorm((r - exp(g) * sqrt(r ^ 2 + (exp(2 * g) - 1) * 2 * g)) / (exp(2 * g) - 1)) -
                        pnorm((r + exp(g) * sqrt(r ^ 2 + (exp(2 * g) - 1) * 2 * g)) / (exp(2 * g) - 1)) +
                        pnorm((r * exp(g) + sqrt(r ^ 2 + (exp(2 * g) - 1) * 2 * g)) / (exp(2 * g) - 1)) -
                        pnorm((r * exp(g) - sqrt(r ^ 2 + (exp(2 * g) - 1) * 2 * g)) / (exp(2 * g) - 1))))
    }
}

r <- seq(-10, 10, length.out = 80)
g <- seq(-10, 10, length.out = 80)
z <- matrix(rep(0, 6400), nrow = 80)
for (i in seq_along(r)) {
    for (j in seq_along(g)) {
        z[i, j] <- h(r[i], g[j])
    }
}

r <- seq(-10, 10, length.out = 80)
g <- seq(0.1, 10, length.out = 40)
z <- matrix(rep(0, 3200), nrow = 80)
for (i in seq_along(r)) {
    for (j in seq_along(g)) {
        z[i, j] <- f(r[i], g[j])
    }
}

contour(r, g, z, xlab = "(m1-m0)/s1", ylab = "s0/s1")


A <- matrix(c(1, 1, 1, 3), nrow = 2)
B <- matrix(c(1, 2, 2, 5), nrow = 2)
g <- function(a) (t(a) %*% A %*% a) / (t(a) %*% B %*% a)

x <- seq(0, 2 * pi, length.out = 100)
y <- numeric(100)
for (i in seq_along(x)) y[i] <- g(c(cos(x[i]), sin(x[i])))

plot(x, y, type = "l")
