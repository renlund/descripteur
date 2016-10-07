
     ## +----------------------------------------+ ##
     ## | comparing functions for real variables | ##
     ## +----------------------------------------+ ##

##' various comparer functions for real types
##'
##' @param x vector
##' @param glist a grouping list
##' @param w weight
##' @param ... this is to be able to tolerate unnecessary arguments
c_real <- function(...) invisible(NULL)

##' @describeIn c_real median shift measured in units of original IQR, 2 groups only
##' @export
c_shift <- function(x, glist, ...){
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    (stats::median(x1, na.rm = TRUE) - stats::median(x2, na.rm = TRUE)) /
        stats::IQR(x1, na.rm = TRUE)
}
attr(c_shift, "dtable") <- "comp"

##' @describeIn c_real standardized difference, 2 groups only
##' @export
c_rstd <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    x1 <- x[glist[[1]]]
    w1 <- w[glist[[1]]]
    x2 <- x[glist[[2]]]
    w2 <- w[glist[[2]]]
    (d_mean(x = x1, w = w1) - d_mean(x = x2, w = w2)) /
    sqrt((d_sd(x = x1, w = w1)^2 + d_sd(x = x2, w = w2)^2) / 2)
}
attr(c_rstd, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for bnry variables | ##
     ## +----------------------------------------+ ##

##' various comparer functions for bnry types
##'
##' @param x vector
##' @param glist a grouping list
##' @param w weight
##' @param ... this is to be able to tolerate unnecessary arguments
c_bnry <- function(...) invisible(NULL)


##' @describeIn c_bnry risk ratio, 2 groups only
##' @export
c_RR <- function(x, glist, w = NULL, ...){
    d_bp(x[glist[[1]]], w = w[glist[[1]]]) /
        d_bp(x[glist[[2]]], w = w[glist[[2]]])
}
attr(c_RR, "dtable") <- "comp"

##' @describeIn c_bnry odds ratio, 2 groups only
##' @export
c_OR <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    d_odds(x = x1, w = w1) / d_odds(x2, w = w2)
}
attr(c_OR, "dtable") <- "comp"

##' @describeIn c_bnry standardized differences for binary variables, 2 groups only
##' @export
c_bstd <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    p1 <- d_bp(x1, w = w1)
    p2 <- d_bp(x2, w = w2)
    (p1 - p2) / sqrt((p1*(1-p1) + p2*(1-p2)) / 2)
}
attr(c_bstd, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for date variables | ##
     ## +----------------------------------------+ ##

##' various comparer functions for surv types
##'
##' @param x vector
##' @param glist a grouping list
##' @param ... this is to be able to tolerate unnecessary arguments
c_date <- function(...) invisible(NULL)

##' @describeIn c_date number of records in the overlapping time period, 2
##'     groups only
##' @export
c_overlap <- function(x, glist, ...){
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    a <- max(min(x1, na.rm = TRUE), min(x2, na.rm = TRUE))
    b <- min(max(x1, na.rm = TRUE), max(x2, na.rm = TRUE))
    sum(x >= a & x <= b, na.rm = TRUE) ## / sum(!is.na(x))
}
attr(c_overlap, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for catg variables | ##
     ## +----------------------------------------+ ##

##' various comparer functions for catg types
##'
##' @param x vector
##' @param glist a grouping list
##' @param useNA display info for missing?
##' @param w weight
##' @param ... this is to be able to tolerate unnecessary arguments
c_catg <- function(...) invisible(NULL)

##' @describeIn c_catg difference in proportions, 2 groups only
##' @export
c_pdiff <- function(x, glist, useNA = FALSE, w = NULL, ...){
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    p1 <- d_p.c(x[glist[[1]]], w = w1, useNA = useNA)
    p2 <- d_p.c(x[glist[[2]]], w = w2, useNA = useNA)
    p1 - p2
}
attr(c_pdiff, "dtable") <- "comp"

##' @describeIn c_catg odds ratios, 2 groups only
##' @export
c_cOR <- function(x, glist, useNA = FALSE, w = NULL, ...){
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    p1 <- d_p.c(x[glist[[1]]], w = w1, useNA=useNA)
    p2 <- d_p.c(x[glist[[2]]], w = w2, useNA=useNA)
    (p1 / (1-p1)) / (p2 / (1-p2))
}
attr(c_cOR, "dtable") <- "comp"

## d_levels exists in two locations ... problem?
## d_levels <- function(x, useNA = FALSE, w = NULL, ...){
##     y <- make_catg(x)
##     if(useNA) c(levels(y), .missing_char) else levels(y)
## }
## attr(d_levels, "dtable") <- "desc"

     ## +----------------------------------------+ ##
     ## | comparing functions for surv variables | ##
     ## +----------------------------------------+ ##

##' various comparer functions for surv types
##'
##' @param x vector
##' @param glist a grouping list
##' @param type type of censoring
##' @param w weight
##' @param ... this is to be able to tolerate unnecessary arguments
c_surv <- function(...) invisible(NULL)

##' @describeIn c_surv rate ratio, 2 groups only
##' @export
c_rr <- function(x, glist, type = "right", w = NULL, ...){
    survcheck(x)
    if(is.null(w)) w <- rep(1, length(x)/2)
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    if(type == "right"){
        check_right(x)
        d_rate(x1, type, w1) / d_rate(x2, type, w2)
    } else {
        stop("no type but 'right' has been implemented")
    }
}
attr(c_rr, "dtable") <- "comp"
