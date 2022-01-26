var_diff_bootstrap <- function(x, y, seed = 2021) {
    if (!is.null(seed)) {
        ## reinstate system seed after simulation
        sys_seed <- .GlobalEnv$.Random.seed
        on.exit({
            if (!is.null(sys_seed)) {
                .GlobalEnv$.Random.seed <- sys_seed
            } else {
                rm(".Random.seed", envir = .GlobalEnv)
            }
        })
        set.seed(seed)
    }
    n <- nrow(x)
    var_diff <- data.frame(matrix(ncol = 2, nrow = 1000))
    colnames(var_diff) <- c("var_diff_qdap", "var_diff_lda")
    for (i in 1:1000) {
        id <- sample(n, replace = TRUE)
        x_boot <- x[id, ]
        y_boot <- y[id]
        qdap_fit <- QDAP::qdap(x_boot, y_boot)
        x_1d_qdap <- x_boot %*% qdap_fit$drt
        var_diff_qdap <- abs(log(var(x_1d_qdap[y_boot == 0]) /
                                 var(x_1d_qdap[y_boot == 1])))
        ## need to modify the source code of QDAP, because
        ## lda_drt is not a regular output
        x_1d_lda <- x_boot %*% qdap_fit$lda_drt
        var_diff_lda <- abs(log(var(x_1d_lda[y_boot == 0]) /
                                var(x_1d_lda[y_boot == 1])))
        var_diff[i, ] <- c(var_diff_qdap, var_diff_lda)
    }
    return(var_diff)
}

get_var_diff <- function(x, y) {
    qdap_fit <- QDAP::qdap(x, y)
    x_1d_qdap <- x %*% qdap_fit$drt
    var_diff_qdap <- abs(log(var(x_1d_qdap[y == 0]) /
                             var(x_1d_qdap[y == 1])))
    x_1d_lda <- x %*% qdap_fit$lda_drt
    var_diff_lda <- abs(log(var(x_1d_lda[y == 0]) /
                            var(x_1d_lda[y == 1])))
    return(c(var_diff_qdap = var_diff_qdap,
             var_diff_lda = var_diff_lda))
}

## Breast Cancer
bc <- read.csv("../real_data/breast-cancer-wisconsin.data", header = FALSE)
y <- bc[, 11]
id0 <- which(y == 2)
id1 <- which(y == 4)
y[id0] <- 0
y[id1] <- 1
x <- data.matrix(bc[, 2:10])
var_diff_bc <- get_var_diff(x, y)
var_diff_bs_bc <- var_diff_bootstrap(x, y)

## Ultrasonic Flowmeter
fm <- read.table("../real_data/Meter A.data")
y <- fm[, 37]
id0 <- which(y == 2)
id1 <- which(y == 1)
y[id0] <- 0
y[id1] <- 1
x <- data.matrix(fm[, 1:36])
var_diff_fm <- get_var_diff(x, y)
var_diff_bs_fm <- var_diff_bootstrap(x, y)

## Heart Disease
hd <- read.csv("../real_data/heart.csv")
y <- hd[, 14]
id0 <- which(y == 0)
id1 <- which(y == 1)
y[id0] <- 0
y[id1] <- 1
x <- data.matrix(hd[, 1:13])
var_diff_hd <- get_var_diff(x, y)
var_diff_bs_hd <- var_diff_bootstrap(x, y)

## Segment Data
sd <- read.table("../real_data/segment.dat")
sd <- sd[which(sd[, 20] %in% c(1, 4)), ]
y <- sd[, 20]
id0 <- which(y == 4)
id1 <- which(y == 1)
y[id0] <- 0
y[id1] <- 1
x <- data.matrix(sd[, c(2, 6:19)])
var_diff_sd <- get_var_diff(x, y)
var_diff_bs_sd <- var_diff_bootstrap(x, y)

## Satellite Image
si <- read.table("../real_data/sat.trn")
si <- si[which(si[, 37] %in% c(1, 3)), ]
y <- si[, 37]
id0 <- which(y == 3)
id1 <- which(y == 1)
y[id0] <- 0
y[id1] <- 1
x <- data.matrix(si[, 1:36])
var_diff_si <- get_var_diff(x, y)
var_diff_bs_si <- var_diff_bootstrap(x, y)

## Summary
data_set_name <- c("bc", "fm", "hd", "sd", "si")
var_diff <- lapply(paste0("var_diff_", data_set_name), get)
var_diff_bs <- lapply(paste0("var_diff_bs_", data_set_name), get)
names(var_diff) <- data_set_name
names(var_diff_bs) <- data_set_name
var_diff_bs_all <- do.call("cbind", var_diff_bs)

## var differences
var_diff

## $bc
## var_diff_qdap  var_diff_lda 
##      1.217933      1.137594 

## $fm
## var_diff_qdap  var_diff_lda 
##     0.4172832     0.3634180 

## $hd
## var_diff_qdap  var_diff_lda 
##     0.7753569     0.5836692 

## $sd
## var_diff_qdap  var_diff_lda 
##     0.3505150     0.2815874 

## $si
## var_diff_qdap  var_diff_lda 
##     0.5752480     0.4238321 

## means
lapply(var_diff_bs_all, mean)

## $bc.var_diff_qdap
## [1] 1.247648

## $bc.var_diff_lda
## [1] 1.154835

## $fm.var_diff_qdap
## [1] 0.4979707

## $fm.var_diff_lda
## [1] 0.5555629

## $hd.var_diff_qdap
## [1] 0.786508

## $hd.var_diff_lda
## [1] 0.5610129

## $sd.var_diff_qdap
## [1] 0.357693

## $sd.var_diff_lda
## [1] 0.2922269

## $si.var_diff_qdap
## [1] 0.5610449

## $si.var_diff_lda
## [1] 0.4027476

## 95% CI
lapply(var_diff_bs_all, function(x) {
    y <- sort(x); c(y[25], y[975])
})

## $bc.var_diff_qdap
## [1] 0.8466161 1.6951032

## $bc.var_diff_lda
## [1] 0.8406569 1.5148798

## $fm.var_diff_qdap
## [1] 0.02492808 1.18118012

## $fm.var_diff_lda
## [1] 0.04456205 1.28532671

## $hd.var_diff_qdap
## [1] 0.3008199 1.2287862

## $hd.var_diff_lda
## [1] 0.1995811 0.9152359

## $sd.var_diff_qdap
## [1] 0.07003837 0.67926986

## $sd.var_diff_lda
## [1] 0.04301558 0.57824208

## $si.var_diff_qdap
## [1] 0.3802242 0.7333322

## $si.var_diff_lda
## [1] 0.2593304 0.5419997

## histograms
pdf("out.pdf", paper = "letter")
par(mfrow = c(2, 2))
xlim <- c(0, 2.0)
breaks <- 10
invisible(mapply(hist, var_diff_bs_all, xlab = names(var_diff_bs_all),
                 main = paste("Histogram of", names(var_diff_bs_all)),
                 MoreArgs = list(breaks = breaks, xlim = xlim)))
dev.off()
