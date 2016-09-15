    ## +-----------------------------------------+ ##
    ## | describing functions for real variables | ##
    ## +-----------------------------------------+ ##

d_n <- function(x, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    sum(w)
}
attr(d_n, "dtable") <- "desc"
d_missing <- function(x, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    sum(w[is.na(x)])
}
attr(d_missing, "dtable") <- "desc"
d_mean <- function(x, w = NULL, ...){
    if(is.null(w)){
        mean(x, na.rm = TRUE)
    } else {
        stats::weighted.mean(x, w = w, na.rm = TRUE)
    }
}
attr(d_mean, "dtable") <- "desc"
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
d_median <- function(x, ...) stats::median(x, na.rm = TRUE)
attr(d_median, "dtable") <- "desc"
d_IQR <- function(x, ...) stats::IQR(x, na.rm = TRUE)
attr(d_IQR, "dtable") <- "desc"
d_sum <- function(x, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    s <- if(is.null(w)){
        mean(x, na.rm = TRUE)
    } else {
        stats::weighted.mean(x, w = w, na.rm = TRUE)
    }
    length(x) * s
}
attr(d_sum, "dtable") <- "desc"

    ## +-----------------------------------------+ ##
    ## | describing functions for bnry variables | ##
    ## +-----------------------------------------+ ##

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
d_ref_level <- function(x, ...){
    y <- make_bnry(x)
    levels(y)[2]
}
attr(d_ref_level, "dtable") <- "desc"
d_n.b <- function(x, w = NULL, ...){
    y <- make_bnry(x)
    if(is.null(w)) w <- rep(1, length(x))
    z <- ifelse(y==d_ref_level(y), 1L, 0L)
    sum(w[z==1], na.rm = TRUE)
}
attr(d_n.b, "dtable") <- "desc"
d_p.b <- function(x, w = NULL, ...){
    y <- make_bnry(x)
    if(is.null(w)) w <- rep(1, length(x))
    z <- ifelse(y==d_ref_level(y), 1L, 0L)
    stats::weighted.mean(x = z, w = w, na.rm = TRUE)
}
attr(d_p.b, "dtable") <- "desc"
d_odds <- function(x, w = NULL, ...){
    y <- make_bnry(x)
    if(is.null(w)) w <- rep(1, length(x))
    tryCatch(d_p.b(y, w = w)/(1-d_p.b(y, w = w)), error = function(e) NA)
}
attr(d_odds, "dtable") <- "desc"

    ## +-----------------------------------------+ ##
    ## | describing functions for date variables | ##
    ## +-----------------------------------------+ ##

d_min = function(x, ...) min(x, na.rm = TRUE)
attr(d_min, "dtable") <- "desc"
d_max = function(x, ...) max(x, na.rm = TRUE)
attr(d_max, "dtable") <- "desc"

    ## +-----------------------------------------+ ##
    ## | describing functions for catg variables | ##
    ## +-----------------------------------------+ ##

.missing_char <- "missing"

## -- helper functions --
make_catg <- function(x){
    if(is.factor(x)){
        x
    } else {
        factor(x)
    }
}
.weighted_p <- function(x, w = NULL){
    if(is.null(w)) w <- rep(1L, length(x))
    mm <- stats::model.matrix(~x)
    mm[,1] <- ifelse(rowSums(mm[,-1]) == 0, 1, 0)
    as.numeric(apply(X = mm, MARGIN = 2, FUN = stats::weighted.mean, w = w[!is.na(x)]))
}
.weighted_tab <- function(x, w = NULL){
    if(is.null(w)) w <- rep(1L, length(x))
    y <- as.character(x)
    lev <- if(is.factor(x)) levels(x) else unique(x)
    y[is.na(x)] <- "dOntnAmeyOurlEveltOtHis"
    u <- factor(y, levels = c(lev, "dOntnAmeyOurlEveltOtHis"))
    mm <- stats::model.matrix(~u)
    mm[,1] <- ifelse(rowSums(mm[,-1]) == 0, 1, 0)
    100*as.numeric(apply(X = mm, MARGIN = 2, FUN = stats::weighted.mean, w = w))
}
## -- list functions --
d_levels <- function(x, useNA = FALSE, w = NULL, ...){
    y <- make_catg(x)
    if(useNA) c(levels(y), .missing_char) else levels(y)
}
attr(d_levels, "dtable") <- "meta"
d_percent <- function(x, useNA = FALSE, w = NULL){
    ## if(!is.null(w)) message("percent function for catg does not use weights")
    y <- make_catg(x)
    ## t <- as.numeric(table(y, useNA = "always")) / length(y)
    t <- .weighted_tab(x = y, w = w)
    r <- if(useNA) t else t[-length(t)]
    r
}
attr(d_percent, "dtable") <- "desc"
d_p.c <- function(x, useNA = FALSE, w = NULL){
    y <- make_catg(x)
    ## t <- as.numeric(table(y)) / length(stats::na.omit(y))
    t <- .weighted_p(x = y, w = w)
    if(useNA) c(t, NA) else t
}
attr(d_p.c, "dtable") <- "desc"

    ## +-----------------------------------------+ ##
    ## | describing functions for surv variables | ##
    ## +-----------------------------------------+ ##

survcheck <- function(x){
    if(!"Surv" %in% class(x)) warning("object not of class 'Surv'")
    invisible(NULL)
}
rightcheck <- function(x){
    if(attr(x, "type") != "right") warning("object type not 'right'")
    invisible(NULL)
}
consurv <- function(x, type = "right"){
    survcheck(x)
    if(type == "right"){
        rightcheck(x)
        n <- length(x)
        data.frame(
            time = as.numeric(x)[1:(n/2)],
            event = as.numeric(x)[(n/2+1):n]
        )
    } else {
        stop("no type but 'right' has been implemented")
    }
}
d_tsum.s <- function(x, type = "right", w = NULL, ...){
    survcheck(x)
    if(is.null(w)) w <- rep(1L, length(x)/2)
    if(type == "right"){
        rightcheck(x)
        d <- consurv(x, type)
        d_sum(d$time, w)
    } else {
        stop("no type but 'right' has been implemented")
    }
}
attr(d_tsum.s, "dtable") <- "desc"
d_esum.s <- function(x, type = "right", w = NULL, ...){
    survcheck(x)
    if(is.null(w)) w <- rep(1L, length(x)/2)
    if(type == "right"){
        rightcheck(x)
        d <- consurv(x, type)
        d_sum(d$event, w)
    } else {
        stop("no type but 'right' has been implemented")
    }
}
attr(d_esum.s, "dtable") <- "desc"
d_rate.s <- function(x, type = "right", w = NULL, ...){
    survcheck(x)
    if(type == "right"){
        rightcheck(x)
        d_esum.s(x, type, w) / d_tsum.s(x, type, w)
    } else {
        stop("no type but 'right' has been implemented")
    }
}
attr(d_rate.s, "dtable") <- "desc"
