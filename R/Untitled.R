source('QDAp.R')

mu0 <- c(0,0)
mu1 <- c(1,0)
sigma0 <- diag(c(1,1))
sigma1 <- matrix(c(2,1,1,4), nrow = 2)
n <- 100000
a <- matrix(rep(0, 2*50), nrow = 2)
atrue <- drt(mu0, mu1, sigma0, sigma1, (sigma0+sigma1)/2, 1)$a
if (atrue[1] < 0) atrue <- -atrue
for (i in 1:50) {
  x0 <- mvrnorm(n, mu0, sigma0)
  x1 <- mvrnorm(n, mu1, sigma1)
  mu0h <- colMeans(x0)
  mu1h <- colMeans(x1)
  sigma0h <- cov(x0)
  sigma1h <- cov(x1)
  sigmah <- cov(rbind(x0,x1))
  b <- drt(mu0h, mu1h, sigma0h, sigma1h, sigmah, 1)$a
  if (b[1] < 0) b <- -b
  a[,i] <- b
}
d <- sqrt(n) * (atan(a[2,]/a[1,]) - atan(atrue[2]/atrue[1]))
var(d)
plot(d, rep(0,50))