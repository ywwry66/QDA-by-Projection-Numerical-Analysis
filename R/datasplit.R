## This function returns the id's of training data given the id's
## each class, percentage of training data and number of replicates

datasplit <- function(id0, id1,
                       m=100, per=c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) {
    length_per <- length(per)
    n0 <- length(id0)
    n1 <- length(id1)
    id <- list()
    for (i in 1:length_per) {
        id[[i]] <- list()
        for (j in 1:m) {
            id0tr <- sample(id0, round(per[i] * n0))
            id1tr <- sample(id1, round(per[i] * n1))
            id[[i]][[j]] <- c(id0tr, id1tr)
        }
    }
    return(id)
}
