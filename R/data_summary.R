## summary_se gives means, standard errors and confidence intervals
## of the data frame containing prediction errors from different methods

summary_se <- function(data) {
    n <- dim(data)[1]
    method <- colnames(data)
    m <- apply(data, 2, mean)
    se <- apply(data, 2, sd) / sqrt(n)
    ci <- se * qt(0.975, n - 1)
    return(data.frame(prediction.error = m, method = method,
                      standard.error = se, ci.95 = ci,
                      row.names = NULL))
}
