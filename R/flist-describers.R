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

##' @describeIn d_real Percent missing
##' @export
d_missing.perc <- function(x, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    100 * sum(w[is.na(x)]) / sum(w)
}
attr(d_missing.perc, "dtable") <- "desc"

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

##' @describeIn d_real first quartile
##' @export
d_Q1 <- function(x, ...) stats::quantile(x, na.rm = TRUE, probs = 0.25,
                                         names = FALSE)
attr(d_Q1, "dtable") <- "desc"

##' @describeIn d_real third quartile
##' @export
d_Q3 <- function(x, ...) stats::quantile(x, na.rm = TRUE, probs = 0.75,
                                         names = FALSE)
attr(d_Q3, "dtable") <- "desc"

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
            if(length(unique(stats::na.omit(x))) == 2){
                a <- "trying to give binary stats on a non-binary variable"
                b <- paste0(a, ".\n which will work since it only has",
                            " two unique values, but is a bit dodgy.")
                lev <- levels(factor(x))
                if(rev) return(factor(x, levels = rev(lev)))
                if(!rev) return(factor(x, levels = lev))
                warning(b)
            } else {
                stop(a)
            }
        }
    }
    lev <- sort(stats::na.omit(unique(x)))
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

##' various describing functions for date variables
##'
##' @param x vector of dates
##' @param ... this is to be able to tolerate unnecessary arguments
d_date <- function(...) invisible(NULL)

##' @describeIn d_date minimum
##' @export
d_dmin = function(x, ...) as.character(min(x, na.rm = TRUE))
attr(d_dmin, "dtable") <- "desc"

##' @describeIn d_date maximum
##' @export
d_dmax = function(x, ...) as.character(max(x, na.rm = TRUE))
attr(d_dmax, "dtable") <- "desc"


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

## weighted proportion for categorical variables - will never show statistic for
## missing even if there are missing
weighted_p <- function(x, w = NULL, count = FALSE){
    if(is.null(w)) w <- rep(1L, length(x))
    mm <- stats::model.matrix(~x)
    mm[,1] <- ifelse(rowSums(mm[, -1, drop = FALSE]) == 0, 1, 0)
    if(count){
        as.numeric(colSums(mm*w[!is.na(x)]))
    } else {
        as.numeric(apply(X = mm, MARGIN = 2, FUN = stats::weighted.mean,
                         w = w[!is.na(x)]))
    }
}

## weighted table (percentage) for categorical variable - will always show
## statistic for missing, even if there are no missing
weighted_tab <- function(x, w = NULL, count = FALSE){
    tryCatch(
        expr = {
            if(is.null(w)) w <- rep(1L, length(x))
            y <- as.character(x)
            lev <- if(is.factor(x)) levels(x) else unique(x)
            y[is.na(x)] <- "dOntnAmeyOurlEveltOtHis"
            u <- factor(y, levels = c(lev, "dOntnAmeyOurlEveltOtHis"))
            mm <- stats::model.matrix(~u)
            mm[,1] <- ifelse(rowSums(mm[, -1, drop = FALSE]) == 0, 1, 0)
            ## colnames(mm)[1] <- paste0("u", lev[1])
            if(count){
                as.numeric(colSums(mm*w))
            } else {
                100*as.numeric(apply(X = mm, MARGIN = 2, FUN = stats::weighted.mean,
                                     w = w))
            }
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
d_percent <- function(x, useNA = FALSE, w = NULL, ...){
    y <- make_catg(x)
    ta <- weighted_tab(x = y, w = w)
    r <- if(useNA) ta else ta[-length(ta)]
    r
}
attr(d_percent, "dtable") <- "desc"

##' @describeIn d_catg count of each level (and missing possibly)
##' @export
d_cn <- function(x, useNA = FALSE, w = NULL, ...){
    y <- make_catg(x)
    ta <- weighted_tab(x = y, w = w, count = TRUE)
    r <- if(useNA) ta else ta[-length(ta)]
    r
}
attr(d_cn, "dtable") <- "desc"

##' @describeIn d_catg the estimated proportion of each level
##' @export
d_cp <- function(x, useNA = FALSE, w = NULL, ...){
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
##' @param cens.type what kind of censoring?
##' @param ... this is to be able to tolerate unnecessary arguments
d_surv <- function(...) invisible(NULL)

survcheck <- function(x){
    if(!"Surv" %in% class(x)) warning("object not of class 'Surv'")
    invisible(NULL)
}
check_right <- function(x){
    if(attr(x, "type") != "right") warning("object cens.type not 'right'")
    invisible(NULL)
}
consurv <- function(x, cens.type = "right"){
    survcheck(x)
    if(cens.type == "right"){
        check_right(x)
        n <- length(x)
        data.frame(
            time = as.numeric(x)[1:(n/2)],
            event = as.numeric(x)[(n/2+1):n]
        )
    } else {
        stop("no cens.type but 'right' has been implemented")
    }
}

##' @describeIn d_surv sum of follow up time
##' @export
d_tsum <- function(x, w = NULL, cens.type = "right", ...){
    survcheck(x)
    if(is.null(w)) w <- rep(1L, length(x)/2)
    if(cens.type == "right"){
        check_right(x)
        d <- consurv(x, cens.type)
        d_sum(d$time, w)
    } else {
        stop("no cens.type but 'right' has been implemented")
    }
}
attr(d_tsum, "dtable") <- "desc"

##' @describeIn d_surv sum of events
##' @export
d_esum <- function(x, w = NULL, cens.type = "right", ...){
    survcheck(x)
    if(is.null(w)) w <- rep(1L, length(x)/2)
    if(cens.type == "right"){
        check_right(x)
        d <- consurv(x, cens.type)
        d_sum(d$event, w)
    } else {
        stop("no cens.type but 'right' has been implemented")
    }
}
attr(d_esum, "dtable") <- "desc"

##' @describeIn d_surv rate (d_esum / d_tsum)
##' @export
d_rate <- function(x, w = NULL, cens.type = "right", ...){
    survcheck(x)
    if(cens.type == "right"){
        check_right(x)
        d_esum(x, w = w, cens.type = cens.type) / d_tsum(x, w = w, cens.type = cens.type)
    } else {
        stop("no cens.type but 'right' has been implemented")
    }
}
attr(d_rate, "dtable") <- "desc"


##    ## +-----------------------------------+ ##
##    ## | compact-type describing functions | ##
##    ## +-----------------------------------+ ##
##
## ##' various describer functions for any type
## ##'
## ##' @param x variable
## ##' @param xname typically you need not supply this (automatic or internal)
## ##' @param type.guide a guide to determine type
## ##' @param median median style info for numerics? else mean style
## ##' @param show.NA display missing info?
## ##' @param ...
## d_any <- function(...) invisible(NULL)

## ##' @describeIn d_any a function to get a compact table of selected statistics
## ##' @export
## d_compact <- function(x, xname, type.guide, median = TRUE,
##                       show.NA = TRUE, ...){
##     if(is.null(type.guide)){
##         ## function to guess type ?
##         ## -->  type <- "real" ## or whatever
##         stop("'type.guide' needed")
##     }
##     if(missing(xname)) xname <- as.character(substitute(x))
##     dots <- list(...)
##     info <- if(!is.null(tmp <- dots[["only.give.information"]])){
##                 tmp
##             } else FALSE
##     type <- type.guide$type[type.guide$variable == xname]
##     NAn <- sum(is.na(x))
##     NAtxt <- if(NAn>0 & show.NA) paste0("[", NAn,"]") else NULL
##     if(type == "real"){
##         z <- as.numeric(x)
##         if(median){
##             if(!info){
##                 Q2 <- d_median(x = z)
##                 Q1 <- d_Q1(x = z)
##                 Q3 <- d_Q3(x = z)
##                 paste0(roundisch(Q2), " (", roundisch(Q1),"-",
##                        roundisch(Q3),")", NAtxt)
##             } else "Numeric variables: median(Q1-Q3)"
##         } else {
##             if(!info){
##                 m <- d_mean(x = z)
##                 sd <- d_sd(x = z)
##                 paste0(roundisch(m), " (", roundisch(sd), ")", NAtxt)
##             } else "Numeric variables: mean(sd)"
##         }
##     } else if(type == "bnry"){
##         if(!info){
##             z <- make_bnry(x)
##             n <- d_bn(x = z)
##             p <- d_bp(x = z)
##             paste0(n, " (",
##                    roundisch(100*p, t = 0.001, scientific = TRUE, digit2 = 2),
##                    "\\%)", NAtxt)
##             } else "Binary variables: count(percentage) for non-reference level only"
##     } else if(type == "catg"){
##         if(!info){
##             z <- make_catg(x)
##             n <- d_cn(x = z)
##             p <- d_cp(x = z)
##             paste0(n, " (",
##                    roundisch(100*p, t = 0.001, scientific = TRUE, digit2 = 2),
##                    "\\%)", NAtxt)
##             } else "Category variables: count(percentage)"
##     } else if(type == "date"){
##         if(!info){
##             z <- if(any(c("Date", "POSIXct") %in% class(x))){
##                      as.Date(x, origin = "1970-01-01")
##                  } else{
##                      x
##                  }
##             a <- as.character(d_min(z))
##             b <- as.character(d_max(z))
##             paste0(a, "/", b)
##         } else "Date variables: min/max"
##     } else if(type == "surv"){
##         NULL
##     }
## }
## attr(d_compact, "dtable") <- "desc"

## ##' @describeIn d_any is to give information on \code{d_compact}
## ##' @export
## d_compact_info <- function(x, xname, type.guide = NULL, median = TRUE,
##                            show.NA = TRUE, ...){
##     d_compact(x, xname, type.guide = NULL, median = TRUE,
##               show.NA = TRUE, ..., only.give.information = TRUE)
## }
## attr(d_compact_info, "dtable") <- "meta"


    ## +-----------------------------------+ ##
    ## | compact-type describing functions | ##
    ## +-----------------------------------+ ##

##' various functions for compact summary of variables
##'
##' @param x the data
##' @param xname name of variable
##' @param useNA display information on missing
##' @param ... arguments passed
dt_desc <- function(...) invisible(NULL)

NA_txt <- function(x) paste0("[", sum(is.na(x)),"]")

##' @describeIn dt_desc returns an empty string
##' @export
dt_empty_desc <- function(x, ...) NA
attr(dt_empty_desc, "dtable") <- "desc"

abbrev <- function(s, n = 31){
    if(n<3) stop("don't")
    foo <- function(x, n) paste0(substring(x, 1, n-3))
    s_copy <- s
    for(k in seq_along(s)){
        s_copy[k] <- if(nchar(s[k])>n){
                         paste0(substring(s[k], 1, n-3),"...")
                     } else {
                         s[k]
                     }
    }
    s_copy
}

abbrev2 <- function(a, b, n = 31, sep = ":"){
    n2 <- floor(n/2)
    an <- nchar(a)
    bn <- nchar(b)
    paste0(abbrev(a, max(n2, n-bn)),
           sep,
           abbrev(b, max(n2, n-an)))
}

latex_fix <- function(s){
    gsub("_", "\\_", s, fixed = TRUE)
}

if(FALSE){
    abbrev2(paste0(letters[1:20], collapse = ""),
            paste0(LETTERS[1:20], collapse = ""))
    abbrev2(paste0(letters[1:10], collapse = ""),
            paste0(LETTERS[1:25], collapse = ""))
    abbrev2(paste0(letters[1:25], collapse = ""),
            paste0(LETTERS[1:10], collapse = ""))
    abbrev2(paste0(letters[1:15], collapse = ""),
            paste0(LETTERS[1:15], collapse = ""))
    abbrev2(paste0(letters[1:20], collapse = ""),
            paste0(LETTERS[1:10], collapse = ""))
}

##' @describeIn dt_desc returns name
##' @export
dt_name <- function(x, xname = NULL, ...){
    if(is.null(xname)) xname <- as.character(substitute(x))
    xname <- latex_fix(xname)
    abbrev(xname)
}
attr(dt_name, "dtable") <- "meta"

##' @describeIn dt_desc returns name:ref for bnry
##' @export
dt_bname <- function(x, xname = NULL, ...){
    rl <- d_ref_level(x)
    if(is.null(xname)) xname <- as.character(substitute(x))
    xname <- latex_fix(xname)
    abbrev2(xname, rl, sep = ": ")
}
attr(dt_bname, "dtable") <- "meta"

##' @describeIn dt_desc returns name:ref for bnry
##' @export
dt_cname <- function(x, xname = NULL, ...){
    rl <- latex_fix(levels(make_catg(x)))
    n <- length(rl)
    if(is.null(xname)) xname <- as.character(substitute(x))
    xname <- latex_fix(xname)
    a <- abbrev2(xname, rl[1], sep = ": ")
    b <- paste0("\\quad: ", abbrev(rl[-1], 25))
    c(a, b)
}
attr(dt_cname, "dtable") <- "meta"

## ------------------------------------------------------------------------- ##
dt_Q_helper <- function(x, useNA, info){
    if(info){
        "Numeric variables: median(Q1-Q3)"
    } else {
        NAtxt <- if(useNA) NA_txt(x) else NULL
        Q2 <- d_median(x = x)
        Q1 <- d_Q1(x = x)
        Q3 <- d_Q3(x = x)
        paste0(roundisch(Q2), " (", roundisch(Q1),"-",
               roundisch(Q3),")", NAtxt)
    }
}

##' @describeIn dt_desc quantiles
##' @export
dt_Q <- function(x, useNA = FALSE, ...){
    dt_Q_helper(x, useNA = useNA, info = FALSE)
}
attr(dt_Q, "dtable") <- "desc"

##' @describeIn dt_desc info for \code{dt_Q}
##' @export
dt_Q.info <- function(x, ...){
    dt_Q_helper(x, info = TRUE)
}
attr(dt_Q.info, "dtable") <- "meta" ## "footnote" ?

## ------------------------------------------------------------------------- ##
dt_msd_helper <- function(x, useNA, info){
    if(info){
        "Numeric variables: mean(sd)"
    } else {
        NAtxt <- if(useNA) NA_txt(x) else NULL
        m <- d_mean(x = x)
        sd <- d_sd(x = x)
        paste0(roundisch(m), " (", roundisch(sd), ")", NAtxt)
    }
}

##' @describeIn dt_desc mean and standard deviation
##' @export
dt_msd <- function(x, useNA = FALSE, ...){
    dt_msd_helper(x, useNA = useNA, info = FALSE)
}
attr(dt_msd, "dtable") <- "desc"

##' @describeIn dt_desc info for \code{dt_msd}
##' @export
dt_msd.info <- function(x, ...){
    dt_msd_helper(x, info = TRUE)
}
attr(dt_msd.info, "dtable") <- "meta" ## "footnote" ?

## ------------------------------------------------------------------------- ##
dt_bcp_helper <- function(x, useNA, info, perc.sign = NULL){
    if(is.null(perc.sign)) perc.sign <- "\\%"
    if(info){
        "Category variables: count(percentage)"
    } else {
        NAtxt <- if(useNA) NA_txt(x) else NULL
        z <- make_bnry(x)
        n <- d_bn(x = z)
        p <- d_bp(x = z)
        paste0(n, " (",
               roundisch(100*p, t = 0.001, scientific = TRUE, digit2 = 2),
               perc.sign, ")", NAtxt)
    }
}

##' @describeIn dt_desc count and percentages (for bnry)
##' @export
dt_bcp <- function(x, useNA = FALSE, ...){
    dt_bcp_helper(x, useNA = useNA, info = FALSE)
}
attr(dt_bcp, "dtable") <- "desc"

##' @describeIn dt_desc info for \code{dt_bcp}
##' @export
dt_bcp.info <- function(x, ...){
    dt_bcp_helper(x, info = TRUE)
}
attr(dt_bcp.info, "dtable") <- "meta" ## "footnote" ?

## ------------------------------------------------------------------------- ##
dt_ccp_helper <- function(x, useNA, info, perc.sign = NULL){
    if(is.null(perc.sign)) perc.sign <- "\\%"
    if(info){
        "Category variables: count(percentage)"
    } else {
        NAtxt <- if(useNA) NA_txt(x) else NULL
        z <- make_catg(x)
        n <- d_cn(x = z)
        p <- d_cp(x = z)
        paste0(n, " (",
               roundisch(100*p, t = 0.001, scientific = TRUE, digit2 = 2),
               perc.sign, ")", c(NAtxt, rep("", length(n)-1)))
    }
}

##' @describeIn dt_desc count and percentages (for catg)
##' @export
dt_ccp <- function(x, useNA = FALSE, ...){
    dt_ccp_helper(x, useNA = useNA, info = FALSE)
}
attr(dt_ccp, "dtable") <- "desc"

##' @describeIn dt_desc info for \code{dt_ccp}
##' @export
dt_ccp.info <- function(x, ...){
    dt_ccp_helper(x, info = TRUE)
}
attr(dt_ccp.info, "dtable") <- "meta" ## "footnote" ?

## ------------------------------------------------------------------------- ##
dt_date_helper <- function(x, useNA, info){
    if(info){
        "Date variables: min/max"
    } else {
        NAtxt <- if(useNA) NA_txt(x) else NULL
        a <- as.character(d_min(x))
        b <- as.character(d_max(x))
        paste0(a, "/", b, NAtxt)
    }
}

##' @describeIn dt_desc first and last date
##' @export
dt_date <- function(x, useNA = FALSE, ...){
    dt_date_helper(x, useNA = useNA, info = FALSE)
}
attr(dt_date, "dtable") <- "desc"

##' @describeIn dt_desc info for \code{dt_date}
##' @export
dt_date.info <- function(x, ...){
    dt_date_helper(x, info = TRUE)
}
attr(dt_date.info, "dtable") <- "meta" ## "footnote" ?

## ------------------------------------------------------------------------- ##
dt_rate_helper <- function(x, useNA, info, ...){
    if(info){
        "Event data: rate of events"
    } else {
        NAtxt <- if(useNA) paste0(" ", NA_txt(x)) else NULL
        r <- d_rate(x, ...)
        paste0(roundisch(r, t = 0.001, scientific = TRUE), NAtxt)
    }
}

##' @describeIn dt_desc rate of events
##' @export
dt_rate <- function(x, useNA = FALSE, ...){
    dt_rate_helper(x, useNA = useNA, info = FALSE)
}
attr(dt_rate, "dtable") <- "desc"

##' @describeIn dt_desc info for \code{dt_rate}
##' @export
dt_rate.info <- function(x, ...){
    dt_rate_helper(x, info = TRUE)
}
attr(dt_rate.info, "dtable") <- "meta" ## "footnote" ?

## ------------------------------------------------------------------------- ##
dt_event_helper <- function(x, useNA, info, ...){
    if(info){
        "Event data: number of events"
    } else {
        NAtxt <- if(useNA) paste0(" ", NA_txt(x)) else NULL
        paste(d_esum(x, ...), NAtxt)
    }
}

##' @describeIn dt_desc rate of events
##' @export
dt_event <- function(x, useNA = FALSE, ...){
    dt_event_helper(x, useNA = useNA, info = FALSE)
}
attr(dt_event, "dtable") <- "desc"

##' @describeIn dt_desc info for \code{dt_rate}
##' @export
dt_event.info <- function(x, ...){
    dt_event_helper(x, info = TRUE)
}
attr(dt_event.info, "dtable") <- "meta" ## "footnote" ?
