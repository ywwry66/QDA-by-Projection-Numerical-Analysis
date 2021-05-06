## data_analysis is a wrapper of our function QDAp and other competitors
## data_analysis_sl also includes oracle

source("R/save_restore_seed.R") #SQDA/DAP messes up seed
library(QDAP)
library(DAP)
library(rda)
library(dsda)
library(MASS) #for lda and qda

data_analysis <- function(x, y, xnew, ynew, lda=TRUE, qda=TRUE,
                          dsda=TRUE, sqda=TRUE, rda=TRUE) {
    ## Our method
    y[which(y == 2)] <- 0
    ynew[which(ynew == 2)] <- 0
    ypred <- qdap(x, as.factor(y), xnew, lambda = 0, iter = 1, optim = "codesc")$class
    pe_qdap <- mean(ynew != ypred)
    out <- data.frame("QDAP" = pe_qdap)
    ## LDA
    pe_lda <- NULL
    if (lda == TRUE) {
        fit_lda <- lda(x, as.factor(y))
        ypred <- predict(fit_lda, xnew)$class
        pe_lda <- mean(ynew != ypred)
        out <- data.frame(out, "LDA" = pe_lda)
    }
    ## QDA
    pe_qda <- NULL
    if (qda == TRUE) {
        fit_qda <- qda(x, as.factor(y))
        ypred <- predict(fit_qda, xnew)$class
        pe_qda <- mean(ynew != ypred)
        out <- data.frame(out, "QDA" = pe_qda)
    }
    ## DSDA
    pe_dsda <- NULL
    if (dsda == TRUE) {
        pe_dsda <- dsda(as.matrix(x), y, as.matrix(xnew), ynew)$error
        out <- data.frame(out, "DSDA" = pe_dsda)
    }
    y[which(y == 0)] <- 2
    ynew[which(ynew == 0)] <- 2
    ## SQDA
    pe_sqda <- NULL
    if (sqda == TRUE) {
        invisible(capture.output(
            pe_sqda <- save_restore_seed(apply_DAP, x, y, xnew, ynew)$error))
        out <- data.frame(out, "DAP" = pe_sqda)
    }
    ## RDA
    pe_rda <- NULL
    if (rda == TRUE) {
        fit_rda <- rda(t(x), as.factor(y))
        invisible(capture.output(
            err <- rda.cv(fit_rda, t(x), as.factor(y))$cv.err))
        par <- arrayInd(which.min(err), dim(err))
        fit_rda <- rda(t(x), as.factor(y),
                       alpha = seq(0, 0.99, len = 10)[par[1]],
                       delta = seq(0, 3, len = 10)[par[2]])
        ypred <- predict(fit_rda, t(x), as.factor(y), t(xnew), type = "class")
        pe_rda <- mean(ynew != ypred)
        out <- data.frame(out, "RDA" = pe_rda)
    }
    return(out)
}

data_analysis_sl <- function(x, y, xnew, ynew, oracle_rule=NULL,
                             lda=TRUE, qda=TRUE, dsda=TRUE,
                             sqda=TRUE, rda=TRUE) {
    out <- data_analysis(x = x, y = y, xnew = xnew, ynew = ynew,
                         lda = lda, qda = qda, dsda = dsda,
                         sqda = sqda, rda = rda)
    if (!is.null(oracle_rule)) {
        pe_oracle <- mean(ynew != apply(xnew, 1, oracle_rule))
        out <- data.frame(out, "Oracle" = pe_oracle)
    }
    return(out)
}
