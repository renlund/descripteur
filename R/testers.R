     ## +----------------------------------------+ ##
     ## | testing functions for real variables   | ##
     ## +----------------------------------------+ ##

##' various testing functions for real types
##'
##' @param x vector
##' @param glist a grouping list
##' @param ... this is to be able to tolerate unnecessary arguments
t_real <- function(...) invisible(NULL)


##' @describeIn t_real t.test p-value, 2 groups only (could be made with weights
##'     but not implemented yet)
##' @export
t_t.test.p <- function(x, glist, ...){
    warn_if_weight_not_used(...)
    warn_if_wrong_glist_length(glist, 2)
    tryCatch(stats::t.test(x = x[glist[[1]]], y = x[glist[[2]]])$p.value,
             error = function(e) NA_real_)
}
attr(t_t.test.p, "dtable") <- "test"

##' @describeIn t_real wilcoxon test p.value
##' @export
t_wilcox.p <- function(x, glist, ...){
    warn_if_weight_not_used(...)
    warn_if_wrong_glist_length(glist, 2)
    tryCatch(stats::wilcox.test(x = x[glist[[1]]], y = x[glist[[2]]],
                                exact = FALSE)$p.value,
             error = function(e) NA_real_)
}
attr(t_wilcox.p, "dtable") <- "test"

##' @describeIn t_real kruskal-wallis p-value, any number of groups
##' @export
t_kruskal.p <- function(x, glist, ...){
    warn_if_weight_not_used(...)
    g <- factor(factorize_glist(glist))
    tryCatch(stats::kruskal.test(x = x, g = g)$p.value,
             error = function(e) NA_real_)
}
attr(t_kruskal.p, "dtable") <- "test"

##' @describeIn t_real ANOVA test p-value, any number of groups
##' @export
t_anova.p <- function(x, glist, ...){
    warn_if_weight_not_used(...)
    fg <- factorize_glist(glist)
    a <- stats::anova(stats::lm(x~fg))
    a[["Pr(>F)"]][1]
}
attr(t_anova.p, "dtable") <- "test"

     ## +----------------------------------------+ ##
     ## | testing functions for bnry variables   | ##
     ## +----------------------------------------+ ##

##' various testing functions for bnry types
##'
##' @param x vector
##' @param glist a grouping list
##' @param ... this is to be able to tolerate unnecessary arguments
t_bnry <- function(...) invisible(NULL)


##' @describeIn t_bnry fisher test p-value
##' @export
t_fisher.p <- function(x, glist, ...){
    warn_if_weight_not_used(...)
    ## warn_if_wrong_glist_length(glist, 2)
    ## x1 <- x[glist[[1]]]
    ## x2 <- x[glist[[2]]]
    ## x <- c(x1, x2)
    ## g <- rep(0:1, c(length(x1), length(x2)))
    g <- factorize_glist(glist)
    tryCatch(stats::fisher.test(x = x, y = g, simulate.p.value = TRUE)$p.value,
             error = function(e) NA_real_)
}
attr(t_fisher.p, "dtable") <- "test"

##' @describeIn t_bnry chisquare test p-value
##' @export
t_chisq.p <- function(x, glist, ...){
    warn_if_weight_not_used(...)
    ## warn_if_wrong_glist_length(glist, 2)
    ## x1 <- x[glist[[1]]]
    ## x2 <- x[glist[[2]]]
    ## x <- c(x1, x2)
    ## g <- rep(0:1, c(length(x1), length(x2)))
    g <- factorize_glist(glist)
    ##stats::chisq.test(x = x, y = g)$p.value
    tryCatch(expr = stats::chisq.test(x = x, y = g)$p.value,
             error = function(e) NA_real_)
}
attr(t_chisq.p, "dtable") <- "test"


     ## +----------------------------------------+ ##
     ## | testing functions for catg variables   | ##
     ## +----------------------------------------+ ##

##' various testing functions for catg types
##'
##' @param x vector
##' @param glist a grouping list
##' @param ... this is to be able to tolerate unnecessary arguments
##' @seealso \code{\link{t_fisher.p}} and \code{\link{t_chisq.p}} which work for
##'     catg variables as well
t_catg <- function(...) invisible(NULL)

##' @describeIn t_catg chisquare test p-value
##' @export
t_cchisq.p <- function(x, glist, ...){
    warn_if_weight_not_used(...)
    y <- make_catg(x)
    n <- length(levels(y)) - 1
    ## if(n == 1) n <- 0
    c(t_chisq.p(x = x, glist = glist, ...), rep(NA, n))
}
attr(t_cchisq.p, "dtable") <- "test"

if(FALSE){
    x = sample(letters[1:2], 100, TRUE)
    tmp <- sample(c(TRUE, FALSE), 100, TRUE)
    glist = list('foo' = tmp, 'bar' = !tmp)
}

     ## +----------------------------------------+ ##
     ## | testing functions for date variables   | ##
     ## +----------------------------------------+ ##

##' various testing functions for date types
##'
##' @param x vector
##' @param glist a grouping list
##' @param ... this is to be able to tolerate unnecessary arguments
t_date <- function(...) invisible(NULL)

##' @describeIn t_date empty test
##' @export
t_date_empty <- function(x, glist, ...) NA
attr(t_date_empty, "dtable") <- "test"

     ## +----------------------------------------+ ##
     ## | testing functions for surv variables   | ##
     ## +----------------------------------------+ ##

##' various testing functions for surv types
##'
##' @param x vector
##' @param glist a grouping list
##' @param ... this is to be able to tolerate unnecessary arguments
t_surv <- function(...) invisible(NULL)

##' @describeIn t_surv empty test
##' @export
t_surv_empty <- function(x, glist, ...) NA
attr(t_surv_empty, "dtable") <- "test"

    ## +----------------------------------+ ##
    ## | compact-type testing functions   | ##
    ## +----------------------------------+ ##

##' various functions for compact summary of variables
##'
##' @param x the data
##' @param glist grouping
##' @param ... to be able to tolerate unnecessary arguments
##' @export
dt_comp <- function(...) invisible(NULL)

##' @describeIn dt_comp returns an empty string
##' @export
dt_empty_comp <- function(x, ...) NA
attr(dt_empty_comp, "dtable") <- "test"

##' @describeIn dt_comp returns an empty string
##' @export
dt_empty_meta <- function(x, ...) NA
attr(dt_empty_meta, "dtable") <- "meta"


## ----------------------------------------------------------------------------
## the cumbersome setup below of having a 'helper' function either return the
## value of interest OR the information about what it has returned is only
## necessary if it is not always the same thing being returned (because if it
## always the same you can just use e.g.
##     dt_wilcox.p = t_wilcox.p, and
##     dt_wilcox.p.info = function(x, glist, ...) "Wilcox"
## (instead) since the current setup of 'apply_flist' only allows one value of
## being captured from each function in the function list (well at least one
## variable, strictly speaking you could return a vector of the value and the
## information, but that seems highly untidy). This would be one reason to allow
## several variables to be recorded from each function (but I think there would
## be other complications with that approach).
dt_wilcox.p_helper <- function(x, glist, info, ...){
    if(info){
        "Wilcoxon"
    } else {
        t_wilcox.p(x = x, glist = glist, ...)
    }
}

##' @describeIn dt_comp wilcoxon test p-value
##' @export
dt_wilcox.p <- function(x, glist, ...){
    dt_wilcox.p_helper(x = x, glist = glist, info = FALSE, ...)
}
attr(dt_wilcox.p, "dtable") <- "test"

##' @describeIn dt_comp info on wilcoxon test p-value
##' @export
dt_wilcox.p.info <- function(x, glist, ...){
    dt_wilcox.p_helper(x = x, glist = glist, info = TRUE)
}
attr(dt_wilcox.p.info, "dtable") <- "meta"

## ----------------------------------------------------------------------------
dt_kruskal.p_helper <- function(x, glist, info, ...){
    if(info){
        "Kruskal-Wallis"
    } else {
        t_kruskal.p(x = x, glist = glist, ...)
    }
}

##' @describeIn dt_comp kruskal-wallis test p-value
##' @export
dt_kruskal.p <- function(x, glist, ...){
    dt_kruskal.p_helper(x = x, glist = glist, info = FALSE, ...)
}
attr(dt_kruskal.p, "dtable") <- "test"

##' @describeIn dt_comp info on kruskal-wallis test p-value
##' @export
dt_kruskal.p.info <- function(x, glist, ...){
    dt_kruskal.p_helper(x = x, glist = glist, info = TRUE)
}
attr(dt_kruskal.p.info, "dtable") <- "meta"

## ----------------------------------------------------------------------------
dt_fisher.p_helper <- function(x, glist, info, ...){
    y <- make_catg(x)
    n <- length(levels(y)) - 1
    if(n == 1) n <- 0
    if(info){
        "Fisher"
    } else {
        ## t_fisher.p(x = x, glist = glist, ...)
        c(t_fisher.p(x = x, glist = glist, ...), rep(NA, n))
    }
}

##' @describeIn dt_comp fishers exact test p-value
##' @export
dt_fisher.p <- function(x, glist, ...){
    dt_fisher.p_helper(x = x, glist = glist, info = FALSE, ...)
}
attr(dt_fisher.p, "dtable") <- "test"

##' @describeIn dt_comp info on fishers exact test p-value
##' @export
dt_fisher.p.info <- function(x, glist, ...){
    dt_fisher.p_helper(x = x, glist = glist, info = TRUE)
}
attr(dt_fisher.p.info, "dtable") <- "meta"

## ----------------------------------------------------------------------------
dt_chisq.p_helper <- function(x, glist, info, ...){
    y <- make_catg(x)
    n <- length(levels(y)) - 1
    if(n == 1) n <- 0
    if(info){
        "Chi-square"
    } else {
        ## t_chisq.p(x = x, glist = glist, ...)
        c(t_chisq.p(x = x, glist = glist, ...), rep(NA, n))
    }
}

##' @describeIn dt_comp chisquare test p-value
##' @export
dt_chisq.p <- function(x, glist, ...){
    dt_chisq.p_helper(x = x, glist = glist, info = FALSE, ...)
}
attr(dt_chisq.p, "dtable") <- "test"

##' @describeIn dt_comp info on chisquare test p-value
##' @export
dt_chisq.p.info <- function(x, glist, ...){
    dt_chisq.p_helper(x = x, glist = glist, info = TRUE)
}
attr(dt_chisq.p.info, "dtable") <- "meta"
