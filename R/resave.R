## Code from https://stackoverflow.com/questions/11813096/
## updating-an-existing-rdata-file
## This is used to append R objects to existing RData file

resave <- function(..., list = character(), file) {
    if (file.exists(file)) {
        previous  <- load(file)
        var_names <- c(list, as.character(substitute(list(...)))[-1L])
        for (var in var_names) assign(var, get(var, envir = parent.frame()))
        save(list = unique(c(previous, var_names)), file = file)
    } else
        save(..., list = list, file = file)
}
