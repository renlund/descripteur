warn_if_wrong_glist_length <- function(glist, length){
    if(desc_get("warn_if_wrong_glist_length")){
        n <- length(glist)
        if(n != length){
            x <- paste0("glist of length ", n, " is given to a function ",
                        "that only makes use of ", length, ".")
            warning(x)
        }
    }
    invisible(NULL)
}

warn_if_weight_not_used <- function(...){
    if(desc_get("warn_if_weight_not_used")){
        l <- list(...)
        if("w" %in% names(l)){
            if(!is.null(l[['w']])){
                x <- paste0("'w' is being passed to a function that does not ",
                            "use weighting")
                warning(x)
            }
        }
    }
    invisible(NULL)
}

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
    warn_if_weight_not_used(...)
    warn_if_wrong_glist_length(glist, 2)
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    (stats::median(x1, na.rm = TRUE) - stats::median(x2, na.rm = TRUE)) /
        stats::IQR(x1, na.rm = TRUE)
}
attr(c_shift, "dtable") <- "comp"

##' @describeIn c_real standardized difference (2 groups only) assuming
##'     independence
##' @references \url{https://www.lerner.ccf.org/qhs/software/lib/stddiff.pdf}
##' @export
c_rstd <- function(x, glist, w = NULL, ...){
    warn_if_wrong_glist_length(glist, 2)
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
    warn_if_wrong_glist_length(glist, 2)
    d_bp(x[glist[[1]]], w = w[glist[[1]]]) /
        d_bp(x[glist[[2]]], w = w[glist[[2]]])
}
attr(c_RR, "dtable") <- "comp"

##' @describeIn c_bnry odds ratio, 2 groups only
##' @export
c_OR <- function(x, glist, w = NULL, ...){
    warn_if_wrong_glist_length(glist, 2)
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
##' @references \url{https://www.lerner.ccf.org/qhs/software/lib/stddiff.pdf}
c_bstd <- function(x, glist, w = NULL, ...){
    warn_if_wrong_glist_length(glist, 2)
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

##' various comparer functions for date types
##'
##' @param x vector
##' @param glist a grouping list
##' @param w weights
##' @param ... this is to be able to tolerate unnecessary arguments
c_date <- function(...) invisible(NULL)

##' @describeIn c_date proportion of records in the overlapping time period, 2
##'     groups only
##' @export
c_overlap <- function(x, glist, w = NULL, ...){
    warn_if_wrong_glist_length(glist, 2)
    if(is.null(w)) w <- rep(1, length(x))
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    a <- max(min(x1, na.rm = TRUE), min(x2, na.rm = TRUE))
    b <- min(max(x1, na.rm = TRUE), max(x2, na.rm = TRUE))
    ## sum(x >= a & x <= b, na.rm = TRUE) ## / sum(!is.na(x))
    overlap <- x >= a & x <= b
    sum(w[overlap], na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
}
attr(c_overlap, "dtable") <- "comp"

##' @describeIn c_date standardized difference for date variables (same as for
##'     real variables when the dates are interpreted as numericals)
##' @export
c_dstd <- function(x, glist, ...){
    c_rstd(x = as.numeric(x), glist = glist, ...)
}
attr(c_dstd, "dtable") <- "comp"

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
##' @seealso \code{\link{t_fisher.p}} and \code{\link{t_chisq.p}} which work for
##'     catg variables as well
c_catg <- function(...) invisible(NULL)

##' @describeIn c_catg difference in proportions, 2 groups only
##' @export
c_pdiff <- function(x, glist, useNA = FALSE, w = NULL, ...){
    warn_if_wrong_glist_length(glist, 2)
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    p1 <- d_cp(x[glist[[1]]], w = w1, useNA = useNA)
    p2 <- d_cp(x[glist[[2]]], w = w2, useNA = useNA)
    p1 - p2
}
attr(c_pdiff, "dtable") <- "comp"

##' @describeIn c_catg odds ratios, 2 groups only
##' @export
c_cOR <- function(x, glist, useNA = FALSE, w = NULL, ...){
    warn_if_wrong_glist_length(glist, 2)
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    p1 <- d_cp(x[glist[[1]]], w = w1, useNA=useNA)
    p2 <- d_cp(x[glist[[2]]], w = w2, useNA=useNA)
    (p1 / (1-p1)) / (p2 / (1-p2))
}
attr(c_cOR, "dtable") <- "comp"

##' @describeIn c_catg standardized differences (2 groups only) for each level
##' @export
c_cstd.each <- function(x, glist, useNA = FALSE, w = NULL, ...){
    warn_if_wrong_glist_length(glist, 2)
    useNA <- FALSE ## could this couse problems?
    if(is.null(w)) w <- rep(1, length(x))
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    p1 <- d_cp(x1, w = w1)
    p2 <- d_cp(x2, w = w2)
    (p1 - p2) / sqrt((p1*(1-p1) + p2*(1-p2)) / 2)
}
attr(c_cstd.each, "dtable") <- "comp"

##' @describeIn c_catg standardized differences (2 groups only) for the variable
##' @references \url{https://www.lerner.ccf.org/qhs/software/lib/stddiff.pdf}
##' @param expand.levels if only one value is returned for a multilevel categorical
##'     variable, should the results be expanded with \code{NA}:s to match the
##'     typical output length for a function of such a variable?
##' @export
c_cstd <- function(x, glist, useNA = FALSE, w = NULL,
                   expand.levels = TRUE, ...){
    warn_if_wrong_glist_length(glist, 2)
    useNA <- FALSE ## could this couse problems?
    if(is.null(w)) w <- rep(1, length(x))
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    p1 <- d_cp(x1, w = w1)[-1]
    p2 <- d_cp(x2, w = w2)[-1]
    k <- length(p1)
    S <- matrix(NA, nrow = k, ncol = k)
    for(i in 1:k){
        for(j in 1:k){
            if(i == j){
                S[i, j] <- (p1[i]*(1-p1[i]) + p2[i]*(1-p2[i])) / 2
            } else {
                S[i, j] <- -(p1[i]*p1[j] + p2[i]*p2[j]) / 2
            }
        }
    }
    ## r <- sqrt(t(p1-p2) %*% MASS::ginv(S) %*% (p1-p2))
    r <- if(all(S == 0)){
             Inf
         } else if(is.null(
                    tryCatch(INV <- solve(S),
                             error = function(e) NULL)
                )){
             Inf
         } else{
             sqrt(t(p1-p2) %*% INV %*% (p1-p2))
         }
    if(expand.levels) c(r, rep(NA, k)) else r
}
attr(c_cstd, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for surv variables | ##
     ## +----------------------------------------+ ##

##' various comparer functions for surv types
##'
##' @param x vector
##' @param glist a grouping list
##' @param w weight
##' @param cens.type type of censoring
##' @param ... this is to be able to tolerate unnecessary arguments
c_surv <- function(...) invisible(NULL)

##' @describeIn c_surv rate ratio, 2 groups only
##' @export
c_rr <- function(x, glist, w = NULL, cens.type = "right", ...){
    warn_if_wrong_glist_length(glist, 2)
    survcheck(x)
    if(is.null(w)) w <- rep(1, length(x))
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    if(cens.type == "right"){
        check_right(x)
        d_rate(x1, w = w1, cens.type = cens.type) /
            d_rate(x2, w = w2, cens.type = cens.type)
    } else {
        stop("no cens.type but 'right' has been implemented")
    }
}
attr(c_rr, "dtable") <- "comp"

##' @describeIn c_surv standardized difference between the rates of two surv
##'     variables (difference in mean divided by 'average sd'-isch)
##' @export
c_sstd <- function(x, glist, w = NULL, cens.type = "right", ...){
    warn_if_wrong_glist_length(glist, 2)
    survcheck(x)
    if(is.null(w)) w <- rep(1, length(x)/2)
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    n1 <- d_esum(x = x1, w = w1, cens.type = cens.type, ...)
    n2 <- d_esum(x = x2, w = w2, cens.type = cens.type, ...)
    t1 <- d_tsum(x = x1, w = w1, cens.type = cens.type, ...)
    t2 <- d_tsum(x = x1, w = w1, cens.type = cens.type, ...)
    (n1 / t1 - n2 / t2) / sqrt((n1 / t1 + n2 / t2) / 2)
}
attr(c_sstd, "dtable") <- "comp"
