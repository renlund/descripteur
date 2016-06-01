     ## +----------------------------------------+ ##
     ## | comparing functions for real variables | ##
     ## +----------------------------------------+ ##

d.shift <- function(x, glist, ...){
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]

    (median(x1, na.rm = TRUE) - median(x2, na.rm = TRUE)) /
        IQR(x1, na.rm = TRUE)
}

cr_def <- list(
    "shift" = d.shift
)
attr(cr_def, "dtable") <- "comp"

d.std.diff <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    x1 <- x[glist[[1]]]*w*length(w)/sum(w)
    x2 <- x[glist[[2]]]*w*length(w)/sum(w)
    (mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE)) /
        sd(x1, na.rm = TRUE)
}

cr_sym <- list(
    "std.diff" = d.std.diff
)
attr(cr_sym, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for bnry variables | ##
     ## +----------------------------------------+ ##

d.RR <- function(x, glist, w = NULL, ...){
    d.p(x[glist[[1]]], w = w) / d.p(x[glist[[1]]], w = w)
}
d.OR <- function(x, glist, w = NULL, ...){
    d.odds(x[glist[[1]]], w = w) / d.odds(x[glist[[2]]], w = w)
}

cb_def <- list(
    ## "RR" = d.RR,
    "OR" = d.OR
)
attr(cb_def, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for date variables | ##
     ## +----------------------------------------+ ##

d.overlap <- function(x, glist, ...){
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    a <- max(min(x1, na.rm = TRUE), min(x2, na.rm = TRUE))
    b <- min(max(x1, na.rm = TRUE), max(x2, na.rm = TRUE))
    sum(x >= a & x <= b, na.rm = TRUE) ## / sum(!is.na(x))
}

cd_def <- list(
    "n.overlap" = d.overlap
)
attr(cd_def, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for catg variables | ##
     ## +----------------------------------------+ ##

d.cc_diff <- function(x, glist, useNA = FALSE, w = NULL, ...){
    p1 <- d.tab(x[glist[[1]]], w = w)
    p2 <- d.tab(x[glist[[2]]], w = w)
    dp <- p1 - p2
    if(useNA) c(dp, NA) else dp
}

d.cc_OR <- function(x, glist, useNA = FALSE, w = NULL, ...){
    p1 <- d.tab(x[glist[[1]]], w = w)
    p2 <- d.tab(x[glist[[2]]], w = w)
    OR <- (p1 / (1-p1)) / (p2 / (1-p2))
    if(useNA) c(OR, NA) else OR
}
## d.levels exists in two locations ... problem?
d.levels <- function(x, useNA = TRUE, w = NULL, ...){
    y <- d.catgify(x)
    if(useNA) c(levels(y), .missing_char) else levels(y)
}

cc_def <- list(
    "levels" = d.levels,
    "diff" = d.cc_diff,
    "OR" = d.cc_OR
)
attr(cc_def, "dtable") <- c("meta", "comp", "comp")
