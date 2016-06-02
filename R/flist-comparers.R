## .weighter <- function(x, w){
##     ok <- w != 0 & !is.na(x)
##     if(sum(w[ok]) == 0) stop("weights (to be used) sum to 0")
##     if(sum(w) == 0) warning("sum of weights is 0")
##     (x * w * length(w[ok]) / sum(w[ok]))[ok]
## }

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

d.std.r <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    x1 <- x[glist[[1]]]
    w1 <- w[glist[[1]]]
    x2 <- x[glist[[2]]]
    w2 <- w[glist[[2]]]
    (d.mean(x = x1, w = w1) - d.mean(x = x2, w = w2)) /
    sqrt((d.sd(x = x1, w = w1)^2 + d.sd(x = x2, w = w2)^2) / 2)
}

cr_sym <- list(
    "std" = d.std.r
)
attr(cr_sym, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for bnry variables | ##
     ## +----------------------------------------+ ##

d.RR <- function(x, glist, w = NULL, ...){
    d.p(x[glist[[1]]], w = w[glist[[1]]]) /
        d.p(x[glist[[2]]], w = w[glist[[2]]])
}
d.OR <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    d.odds(x = x1, w = w1) / d.odds(x2, w = w2)
}

d.std.b <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    p1 <- d.p.b(x1, w = w1)
    p2 <- d.p.b(x2, w = w2)
    (p1 - p2) / sqrt((p1*(1-p1) + p2*(1-p2)) / 2)
}
cb_def <- list(
    ## "RR" = d.RR,
    "std" = d.std.b,
    "OR" = d.OR
)
attr(cb_def, "dtable") <- rep("comp", 2)

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
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    p1 <- d.p.c(x[glist[[1]]], w = w1, useNA = useNA)
    p2 <- d.p.c(x[glist[[2]]], w = w2, useNA = useNA)
    ## dp <- p1 - p2
    ## if(useNA) c(dp, NA) else dp
    p1 - p2
}

d.cc_OR <- function(x, glist, useNA = FALSE, w = NULL, ...){
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    p1 <- d.p.c(x[glist[[1]]], w = w1, useNA=useNA)
    p2 <- d.p.c(x[glist[[2]]], w = w2, useNA=useNA)
    ## OR <- (p1 / (1-p1)) / (p2 / (1-p2))
    ## if(useNA) c(OR, NA) else OR
    (p1 / (1-p1)) / (p2 / (1-p2))
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

