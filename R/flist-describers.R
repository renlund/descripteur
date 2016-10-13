    ## +-----------------------------------------+ ##
    ## | describing functions for real variables | ##
    ## +-----------------------------------------+ ##

##' various describer functions for real types
##'
##' @param x vector
##' @param w weight
##' @param ... this is to be able to tolerate unnecessary arguments
d_real <- function(...) invisible(NULL)

##' @describeIn d_real number of non-missing elements
##' @export
d_n <- function(x, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    sum(w[!is.na(x)])
}
attr(d_n, "dtable") <- "desc"

##' @describeIn d_real number of  elements
##' @export
d_length <- function(x, ...) length(x)
attr(d_length, "dtable") <- "desc"

##' @describeIn d_real Count missing
##' @export
d_missing <- function(x, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    sum(w[is.na(x)])
}
attr(d_missing, "dtable") <- "desc"

##' @describeIn d_real sum of all elements
##' @export
d_sum <- function(x, w = NULL, ...){
    s <- if(is.null(w)){
             mean(x, na.rm = TRUE)
         } else {
             if(is.null(w)) w <- rep(1, length(x))
             stats::weighted.mean(x, w = w, na.rm = TRUE)
         }
    d_n(x) * s
}
attr(d_sum, "dtable") <- "desc"

##' @describeIn d_real mean
##' @export
d_mean <- function(x, w = NULL, ...){
    if(is.null(w)){
        mean(x, na.rm = TRUE)
    } else {
        stats::weighted.mean(x, w = w, na.rm = TRUE)
    }
}
attr(d_mean, "dtable") <- "desc"

##' @describeIn d_real standard deviation
##' @export
d_sd <- function(x, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    y <- x[!is.na(x)]
    w <- w[!is.na(x)]
    m <- stats::weighted.mean(x = y, w = w)
    ok <- w != 0
    N <- sum(ok)
    num <- sum(w*(y - m)^2)
    den <- sum(w)
    sqrt(num * N / (den * (N-1)))
}
attr(d_sd, "dtable") <- "desc"

##' @describeIn d_real median
##' @export
d_median <- function(x, w = NULL, ...){
    if(is.null(w)){
        stats::median(x, na.rm = TRUE)
    } else {
        w1 <- w[!is.na(x)]
        x1 <- x[!is.na(x)]
        w2 <- w1[order(x1)]
        x2 <- x1[order(x1)]
        s  <- sum(w2)
        cs <- cumsum(w2)
        is <- which(cs < s/2)
        i <- is[length(is)]
        if(cs[i+1] == s/2){
            (x2[i+1] + x2[i+2]) / 2
        } else {
            x2[i+1]
        }
    }
}
attr(d_median, "dtable") <- "desc"

##' @describeIn d_real minimum
##' @export
d_min = function(x, ...) min(x, na.rm = TRUE)
attr(d_min, "dtable") <- "desc"

##' @describeIn d_real maximum
##' @export
d_max = function(x, ...) max(x, na.rm = TRUE)
attr(d_max, "dtable") <- "desc"

##' @describeIn d_real inter quartile range
##' @export
d_IQR <- function(x, ...) stats::IQR(x, na.rm = TRUE)
attr(d_IQR, "dtable") <- "desc"

    ## +-----------------------------------------+ ##
    ## | describing functions for bnry variables | ##
    ## +-----------------------------------------+ ##

##' various describer functions for bnry types
##'
##' @param x vector
##' @param w weight
##' @param ... this is to be able to tolerate unnecessary arguments
d_bnry <- function(...) invisible(NULL)

## turn vector, if possible, into binary factor
make_bnry <- function(x){
    rev = FALSE ## let this be a setting in opts_desc ??
    if(is.factor(x)){
        if(length(levels(x)) == 2){
            if(rev) return(factor(x, levels = rev(levels(x))))
            if(!rev) return(x)
        } else {
            stop("trying to give binry stats on a non-binary variable")
        }
    }
    lev <- stats::na.omit(unique(x))
    if(rev) lev <- rev(lev)
    if(length(lev) != 2){
        stop("trying to give binry stats on a non-binary variable")
    }
    factor(x, levels = lev)
}

##' @describeIn d_bnry return the reference level of a bnry factor
##' @export
d_ref_level <- function(x, ...){
    y <- make_bnry(x)
    levels(y)[2]
}
attr(d_ref_level, "dtable") <- "meta"

##' @describeIn d_bnry number of occurences of the reference value
##' @export
d_bn <- function(x, w = NULL, ...){
    y <- make_bnry(x)
    if(is.null(w)) w <- rep(1, length(x))
    z <- ifelse(y==d_ref_level(y), 1L, 0L)
    sum(w[z==1], na.rm = TRUE)
}
attr(d_bn, "dtable") <- "desc"

##' @describeIn d_bnry proportion of occurences of the reference value
##' @export
d_bp <- function(x, w = NULL, ...){
    y <- make_bnry(x)
    if(is.null(w)) w <- rep(1, length(x))
    z <- ifelse(y==d_ref_level(y), 1L, 0L)
    stats::weighted.mean(x = z, w = w, na.rm = TRUE)
}
attr(d_bp, "dtable") <- "desc"

##' @describeIn d_bnry the odds of the reference level
##' @export
d_odds <- function(x, w = NULL, ...){
    y <- make_bnry(x)
    if(is.null(w)) w <- rep(1, length(y))
    tryCatch(d_bp(y, w = w)/(1-d_bp(y, w = w)), error = function(e) NA)
}
attr(d_odds, "dtable") <- "desc"

    ## +-----------------------------------------+ ##
    ## | describing functions for date variables | ##
    ## +-----------------------------------------+ ##


    ## +-----------------------------------------+ ##
    ## | describing functions for catg variables | ##
    ## +-----------------------------------------+ ##

##' various describer functions for catg types
##'
##' @param x vector
##' @param w weight
##' @param useNA show information for missing?
##' @param ... this is to be able to tolerate unnecessary arguments
d_catg <- function(...) invisible(NULL)

.missing_char <- "missing"

## make a catg variable
make_catg <- function(x){
    if(is.factor(x)){
        x
    } else {
        factor(x)
    }
}

## weighted proportion for categorical variables
weighted_p <- function(x, w = NULL){
    if(is.null(w)) w <- rep(1L, length(x))
    mm <- stats::model.matrix(~x)
    mm[,1] <- ifelse(rowSums(mm[,-1]) == 0, 1, 0)
    as.numeric(apply(X = mm, MARGIN = 2, FUN = stats::weighted.mean, w = w[!is.na(x)]))
}

## weighted table (percentage) for categorical variable
weighted_tab <- function(x, w = NULL){
    tryCatch(
        expr = {
    if(is.null(w)) w <- rep(1L, length(x))
    y <- as.character(x)
    lev <- if(is.factor(x)) levels(x) else unique(x)
    y[is.na(x)] <- "dOntnAmeyOurlEveltOtHis"
    u <- factor(y, levels = c(lev, "dOntnAmeyOurlEveltOtHis"))
    mm <- stats::model.matrix(~u)
    mm[,1] <- ifelse(rowSums(mm[,-1]) == 0, 1, 0)
    100*as.numeric(apply(X = mm, MARGIN = 2, FUN = stats::weighted.mean,
                              w = w))
    }, error = function(e)
        stop(paste0("descripteur internal function weighted_tab fails with error:\n", e))
    )
}

##' @describeIn d_catg the levels of a catg variable
##' @export
d_levels <- function(x, useNA = FALSE, w = NULL, ...){
    y <- make_catg(x)
    if(useNA) c(levels(y), .missing_char) else levels(y)
}
attr(d_levels, "dtable") <- "meta"

##' @describeIn d_catg percentage of each level (and missing possibly)
##' @export
d_percent <- function(x, useNA = FALSE, w = NULL){
    y <- make_catg(x)
    ta <- weighted_tab(x = y, w = w)
    r <- if(useNA) ta else ta[-length(ta)]
    r
}
attr(d_percent, "dtable") <- "desc"

##' @describeIn d_catg the estimated proportion of each level
##' @export
d_cp <- function(x, useNA = FALSE, w = NULL){
    y <- make_catg(x)
    t <- weighted_p(x = y, w = w)
    if(useNA) c(t, NA) else t
}
attr(d_cp, "dtable") <- "desc"

    ## +-----------------------------------------+ ##
    ## | describing functions for surv variables | ##
    ## +-----------------------------------------+ ##

##' various describer functions for surv types
##'
##' @param x vector
##' @param w weight
##' @param type what kind of censoring?
##' @param ... this is to be able to tolerate unnecessary arguments
d_surv <- function(...) invisible(NULL)

survcheck <- function(x){
    if(!"Surv" %in% class(x)) warning("object not of class 'Surv'")
    invisible(NULL)
}
check_right <- function(x){
    if(attr(x, "type") != "right") warning("object type not 'right'")
    invisible(NULL)
}
consurv <- function(x, type = "right"){
    survcheck(x)
    if(type == "right"){
        check_right(x)
        n <- length(x)
        data.frame(
            time = as.numeric(x)[1:(n/2)],
            event = as.numeric(x)[(n/2+1):n]
        )
    } else {
        stop("no type but 'right' has been implemented")
    }
}

##' @describeIn d_surv sum of follow up time
##' @export
d_tsum <- function(x, w = NULL, type = "right", ...){
    survcheck(x)
    if(is.null(w)) w <- rep(1L, length(x)/2)
    if(type == "right"){
        check_right(x)
        d <- consurv(x, type)
        d_sum(d$time, w)
    } else {
        stop("no type but 'right' has been implemented")
    }
}
attr(d_tsum, "dtable") <- "desc"

##' @describeIn d_surv sum of events
##' @export
d_esum <- function(x, w = NULL, type = "right", ...){
    survcheck(x)
    if(is.null(w)) w <- rep(1L, length(x)/2)
    if(type == "right"){
        check_right(x)
        d <- consurv(x, type)
        d_sum(d$event, w)
    } else {
        stop("no type but 'right' has been implemented")
    }
}
attr(d_esum, "dtable") <- "desc"

##' @describeIn d_surv rate (d_esum / d_tsum)
##' @export
d_rate <- function(x, w = NULL, type = "right", ...){
    survcheck(x)
    if(type == "right"){
        check_right(x)
        d_esum(x, w = w, type = type) / d_tsum(x, w = w, type = type)
    } else {
        stop("no type but 'right' has been implemented")
    }
}
attr(d_rate, "dtable") <- "desc"
