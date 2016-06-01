    ## +-----------------------------------------+ ##
    ## | describing functions for real variables | ##
    ## +-----------------------------------------+ ##

d.missing <- function(x, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    sum(w[is.na(x)])
}
d.mean <- function(x, w = NULL, ...){
    if(is.null(w)){
        mean(x, na.rm = TRUE)
    } else {
        weighted.mean(x, w = w, na.rm = TRUE)
    }
}
d.sd <- function(x, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    y <- x*w*length(w)/sum(w)
    sd(y, na.rm = TRUE)
}
d.median <- function(x, ...) median(x, na.rm = TRUE)
d.IQR <- function(x, ...) IQR(x, na.rm = TRUE)

dr_def <- list(
    "missing" = d.missing,
    "median" = d.median,
    "IQR" = d.IQR
)
attr(dr_def, "dtable") <- rep("desc", 3)

dr_sym <- list(
    "missing" = d.missing,
    "mean" = d.mean,
    "sd" = d.sd
)
attr(dr_sym, "dtable") <- rep("desc", 3)

    ## +-----------------------------------------+ ##
    ## | describing functions for bnry variables | ##
    ## +-----------------------------------------+ ##

d.bnryify <- function(x){
    rev = FALSE ## let this be a setting in opts_desc ??
    if(is.factor(x)){
        if(length(levels(x)) == 2){
            if(rev) return(factor(x, levels = rev(levels(x))))
            if(!rev) return(x)
        } else {
            stop("trying to give binry stats on a non-binary variable")
        }
    }
    lev <- na.omit(unique(x))
    if(rev) lev <- rev(lev)
    if(length(lev) != 2){
        stop("trying to give binry stats on a non-binary variable")
    }
    factor(x, levels = lev)
}
d.level <- function(x, ...){
    y <- d.bnryify(x)
    levels(y)[2]
}
d.p <- function(x, w = NULL, ...){
    y <- d.bnryify(x)
    if(is.null(w)) w <- rep(1, length(x))
    z <- ifelse(y==d.level(y), 1L, 0L)
    ##sum(y==d.level(y), na.rm = TRUE) / length(na.omit(y))
    mean(w*z, na.rm = TRUE)
}
d.odds <- function(x, w = NULL, ...){
    y <- d.bnryify(x)
    if(is.null(w)) w <- rep(1, length(x))
    tryCatch(d.p(y, w = w)/(1-d.p(y, w = w)), error = function(e) NA)
}

db_def <- list(
    "level" = d.level,
    "missing" = d.missing,
    "p" = d.p,
    "odds" = d.odds
)
attr(db_def, "dtable") <- c("meta", "desc", "desc", "desc")

    ## +-----------------------------------------+ ##
    ## | describing functions for date variables | ##
    ## +-----------------------------------------+ ##

d.min = function(x, ...) min(x, na.rm = TRUE)
d.max = function(x, ...) max(x, na.rm = TRUE)

dd_def <- list(
    "missing" = d.missing,
    "min" = d.min,
    "max" = d.max
)
attr(dd_def, "dtable") <- rep("desc", 3)

    ## +-----------------------------------------+ ##
    ## | describing functions for catg variables | ##
    ## +-----------------------------------------+ ##

.missing_char <- "missing"

## -- helper functions --
d.catgify <- function(x){
    if(is.factor(x)){
        x
    } else {
        factor(x)
    }
}
d.weighted_percent <- function(x, w = NULL){
    if(is.null(w)) w <- rep(1L, length(x))
    mm <- model.matrix(~x)
    mm[,1] <- ifelse(rowSums(mm[,-1]) == 0, 1, 0)
    as.numeric(colSums(w[!is.na(x)]*mm, na.rm = TRUE) / sum(w[!is.na(x)]))
}

## -- list functions --
d.levels <- function(x, useNA = TRUE, w = NULL, ...){
    y <- d.catgify(x)
    if(useNA) c(levels(y), .missing_char) else levels(y)
}
d.percent <- function(x, useNA = TRUE, w = NULL){
    y <- d.catgify(x)
    t <- as.numeric(table(y, useNA = "always")) / length(y)
    r <- if(useNA) t else t[-length(t)]
    100*r
}
d.tab <- function(x, useNA = TRUE, w = NULL){
    y <- d.catgify(x)
    ## t <- as.numeric(table(y)) / length(na.omit(y))
    t <- d.weighted_percent(x = y, w = w)
    if(useNA) c(t, NA) else t
}

dc_def <- list(
    "levels" = d.levels,
    "percent" = d.percent,
    "p" = d.tab
)
attr(dc_def, "dtable") <- c("meta", "desc", "desc")
