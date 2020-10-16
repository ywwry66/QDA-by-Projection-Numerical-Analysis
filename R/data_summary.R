## summary_se gives means, standard errors and confidence intervals
## of the data frame containing prediction errors from different methods
## plot_data further plots the output of summary_se

library(ggplot2)

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

plot_data_sl <- function(data, n=c(100, 150, 200, 250, 300), out) {
    len_n <- length(n)
    data_summarized <- NULL
    for (i in 1:len_n) {
        data_summarized <-
            rbind(data_summarized, data.frame(summary_se(data[[i]]), n = n[i]))
    }
    pdf(out)
    pd <- position_dodge(8)
    plot <- ggplot(data_summarized, aes(x = n, y = prediction.error,
                                        colour = method)) +
        geom_errorbar(aes(ymin = prediction.error - ci.95,
                          ymax = prediction.error + ci.95),
                      width = 10, position = pd) +
        geom_line(position = pd) +
        geom_point(position = pd)
    print(plot)
    dev.off()
}

plot_data_rd <- function(data, per=c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8), out) {
    len_per <- length(per)
    data_summarized <- NULL
    for (i in 1:len_per) {
        data_summarized <-
            rbind(data_summarized, data.frame(summary_se(data[[i]]),
                                              per = per[i]))
    }
    pdf(out)
    pd <- position_dodge(0.01)
    plot <- ggplot(data_summarized, aes(x = per, y = prediction.error,
                                        colour = method)) +
        geom_errorbar(aes(ymin = prediction.error - ci.95,
                          ymax = prediction.error + ci.95),
                      width = .02, position = pd) +
        geom_line(position = pd) +
        geom_point(position = pd)
    print(plot)
    dev.off()
}
