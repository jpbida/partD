#! /usr/bin/env Rscript

# reducer.R - Wordcount program in R
# script for Reducer (R-Hadoop integration)

splitLine <- function(line) {
    val <- unlist(strsplit(line, "\t"))
    list(cat = val[1], cols = as.numeric(unlist(strsplit(val[2],":"))))
}

env <- new.env(hash = TRUE)

con <- file("stdin", open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    split <- splitLine(line)
    cat <- split$cat
    cols <- split$cols
    if (exists(cat, envir = env, inherits = FALSE)) {
        oldsum <- get(cat, envir = env)
        assign(cat, oldsum + cols, envir = env)
    }
    else assign(cat, cols, envir = env)
}
close(con)

for (w in ls(env, all = TRUE))
    cat(paste(unlist(strsplit(w,":")),collapse="\t"), "\t", paste(get(w, envir = env),collapse="\t"), "\n", sep = "")
