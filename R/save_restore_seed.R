## This is used to undo the untoward effects of fun on .Random.seed

save_restore_seed <- function(fun, ...) {
    seed_p <- exists(".Random.seed", .GlobalEnv)
    if (seed_p == TRUE) {
        seed <- .GlobalEnv$.Random.seed
    }
    out <- fun(...)
    if (seed_p == TRUE)
        .GlobalEnv$.Random.seed <- seed
    else
        suppressWarnings(rm(".Random.seed", envir = .GlobalEnv))
    return(out)
}
