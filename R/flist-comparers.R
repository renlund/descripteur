
     ## +----------------------------------------+ ##
     ## | comparing functions for real variables | ##
     ## +----------------------------------------+ ##

c_shift <- function(x, glist, ...){
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]

    (stats::median(x1, na.rm = TRUE) - stats::median(x2, na.rm = TRUE)) /
        stats::IQR(x1, na.rm = TRUE)
}
attr(c_shift, "dtable") <- "comp"

## cr_def <- list(
##     "shift" = c_shift
## )
## attr(cr_def, "dtable") <- "comp"

c_std.r <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    x1 <- x[glist[[1]]]
    w1 <- w[glist[[1]]]
    x2 <- x[glist[[2]]]
    w2 <- w[glist[[2]]]
    (d_mean(x = x1, w = w1) - d_mean(x = x2, w = w2)) /
    sqrt((d_sd(x = x1, w = w1)^2 + d_sd(x = x2, w = w2)^2) / 2)
}
attr(c_std.r, "dtable") <- "comp"

## cr_sym <- list(
##     "std" = c_std.r
## )
## attr(cr_sym, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for bnry variables | ##
     ## +----------------------------------------+ ##

c_RR <- function(x, glist, w = NULL, ...){
    d_p.b(x[glist[[1]]], w = w[glist[[1]]]) /
        d_p.b(x[glist[[2]]], w = w[glist[[2]]])
}
attr(c_RR, "dtable") <- "comp"
c_OR <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    d_odds(x = x1, w = w1) / d_odds(x2, w = w2)
}
attr(c_OR, "dtable") <- "comp"
c_std.b <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    p1 <- d_p.b(x1, w = w1)
    p2 <- d_p.b(x2, w = w2)
    (p1 - p2) / sqrt((p1*(1-p1) + p2*(1-p2)) / 2)
}
attr(c_std.b, "dtable") <- "comp"
## cb_def <- list(
##     ## "RR" = c_RR,
##     "std" = c_std.b,
##     "OR" = c_OR
## )
## attr(cb_def, "dtable") <- rep("comp", 2)

     ## +----------------------------------------+ ##
     ## | comparing functions for date variables | ##
     ## +----------------------------------------+ ##

c_overlap.d <- function(x, glist, ...){
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    a <- max(min(x1, na.rm = TRUE), min(x2, na.rm = TRUE))
    b <- min(max(x1, na.rm = TRUE), max(x2, na.rm = TRUE))
    sum(x >= a & x <= b, na.rm = TRUE) ## / sum(!is.na(x))
}
attr(c_overlap.d, "dtable") <- "comp"
## cd_def <- list(
##     "n.overlap" = c_overlap.d
## )
## attr(cd_def, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for catg variables | ##
     ## +----------------------------------------+ ##

c_diff.c <- function(x, glist, useNA = FALSE, w = NULL, ...){
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    p1 <- d_p.c(x[glist[[1]]], w = w1, useNA = useNA)
    p2 <- d_p.c(x[glist[[2]]], w = w2, useNA = useNA)
    p1 - p2
}
attr(c_diff.c, "dtable") <- "comp"
c_OR.c <- function(x, glist, useNA = FALSE, w = NULL, ...){
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    p1 <- d_p.c(x[glist[[1]]], w = w1, useNA=useNA)
    p2 <- d_p.c(x[glist[[2]]], w = w2, useNA=useNA)
    (p1 / (1-p1)) / (p2 / (1-p2))
}
attr(c_OR.c, "dtable") <- "comp"
## d_levels exists in two locations ... problem?
d_levels <- function(x, useNA = TRUE, w = NULL, ...){
    y <- make_catg(x)
    if(useNA) c(levels(y), .missing_char) else levels(y)
}
attr(d_levels, "dtable") <- "desc"

## cc_def <- list(
##     "levels" = d_levels,
##     "diff" = c_diff.c,
##     "OR" = c_OR.c
## )
## attr(cc_def, "dtable") <- c("meta", "comp", "comp")



