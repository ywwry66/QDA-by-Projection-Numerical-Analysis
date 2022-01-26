## data_analysis is a wrapper of our function QDAp and other competitors
## data_analysis_sl also includes oracle

source("R/save_restore_seed.R") #SQDA/DAP messes up seed
library(QDAP)
library(DAP)
library(rda)
library(dsda)
library(MASS) #for lda and qda

data_analysis <- function(x, y, xnew, ynew, qdap=TRUE, lda=TRUE, qda=TRUE,
                          dsda=TRUE, sqda=TRUE, rda=TRUE, qdap_penal=FALSE) {
    out <- NULL
    ## Our method
    pe_qdap <- NULL
    if (qdap == TRUE) {
        y[which(y == 2)] <- 0
        ynew[which(ynew == 2)] <- 0
        ypred <- qdap(x, as.factor(y), xnew, lambda = 0, iter = 1)$class
        pe_qdap <- mean(ynew != ypred)
        out <- c(out, list("QDAP" = pe_qdap))
    }
    ## Our method penalization
    pe_qdap_penal <- NULL
    if (qdap_penal == TRUE) {
        ypred <- qdap_cv2(x, as.factor(y), xnew,
                          num_lambda = 9, lambda_min = 1e-3,
                          optim = "BFGS", folds = 5, seed = 2020,
                          margin_num = 50, margin_method = "ratio")$class
        pe_qdap_penal <- mean(ynew != ypred)
        out <- c(out, list("QDAP_PENAL" = pe_qdap_penal))
    }
    ## LDA
    pe_lda <- NULL
    if (lda == TRUE) {
        fit_lda <- lda(x, as.factor(y))
        ypred <- predict(fit_lda, xnew)$class
        pe_lda <- mean(ynew != ypred)
        out <- c(out, list("LDA" = pe_lda))
    }
    ## QDA
    pe_qda <- NULL
    if (qda == TRUE) {
        fit_qda <- qda(x, as.factor(y))
        ypred <- predict(fit_qda, xnew)$class
        pe_qda <- mean(ynew != ypred)
        out <- c(out, list("QDA" = pe_qda))
    }
    ## DSDA
    pe_dsda <- NULL
    if (dsda == TRUE) {
        pe_dsda <- dsda(as.matrix(x), y, as.matrix(xnew), ynew)$error
        out <- c(out, list("DSDA" = pe_dsda))
    }
    y[which(y == 0)] <- 2
    ynew[which(ynew == 0)] <- 2
    ## SQDA
    pe_sqda <- NULL
    if (sqda == TRUE) {
        invisible(capture.output(
            pe_sqda <- save_restore_seed(apply_DAP, x, y, xnew, ynew)$error))
        out <- c(out, list("DAP" = pe_sqda))
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
        ypred <- predict.rda(fit_rda, t(x), as.factor(y), t(xnew), type = "class")
        pe_rda <- mean(ynew != ypred)
        out <- c(out, list("RDA" = pe_rda))
    }
    return(as.data.frame(out))
}

data_analysis_sl <- function(x, y, xnew, ynew, oracle_rule=NULL,
                             qdap=TRUE, lda=TRUE, qda=TRUE, dsda=TRUE,
                             sqda=TRUE, rda=TRUE, qdap_penal=TRUE) {
    out <- data_analysis(x = x, y = y, xnew = xnew, ynew = ynew,
                         qdap = qdap, lda = lda, qda = qda, dsda = dsda,
                         sqda = sqda, rda = rda, qdap_penal = qdap_penal)
    if (!is.null(oracle_rule)) {
        pe_oracle <- mean(ynew != apply(xnew, 1, oracle_rule))
        out <- data.frame(out, "Oracle" = pe_oracle)
    }
    return(out)
}
