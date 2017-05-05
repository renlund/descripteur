##' extract attributes
##'
##' extract tidy version of attributes from a dtable
##' @param dt a dtable object
##' @param perc use percentages instead of counts
##' @param perc.sign what kind of percentage sign, if any
##' @export
dtable_attr <- function(dt, perc = FALSE, perc.sign = "%"){
    a <- attributes(dt)
    n <- a$size
    ccn <- a$cc
    w <- a$weight
    u <- a$units
    R <- data.frame(
        measure = c(
            if(!is.null(n))   "size"   else NULL,
            if(!is.null(ccn)) "cc"     else NULL,
            if(!is.null(w))   "weight" else NULL,
            if(!is.null(u))   "units"  else NULL
        ),
        total = c(n, if(perc) paste0(roundisch(100*ccn/n), perc.sign) else ccn, w, u),
        stringsAsFactors = FALSE
    )
    fnc <- function(a, b, p){
        r <- if(is.null(a)){
            NULL
        } else {
            if(p) paste0(roundisch(100*a/b), perc.sign) else a
        }
        stats::setNames(r, names(a))
    }
    if(!is.null(a$glist_size)){
             size =   fnc(a$glist_size, n, FALSE)
             cc =     fnc(a$glist_cc, a$glist_size, perc)
             weight = fnc(a$glist_weight, w, perc)
             units =  fnc(a$glist_units, u, FALSE)
             tmp <- rbind(size, cc, weight, units, stringsAsFactors = FALSE)
             Q <- as.data.frame(tmp, stringsAsFactors = FALSE)
             Q$measure <- rownames(tmp)
             rownames(Q) <- NULL
             merge(R, Q, sort = FALSE)
    } else {
        R
    }
}

## round numbers, for numbers >1 round to 1 decimal, else
## use 1 significant digit when >t, else "<t"
round_helper <- function(x, t = 0.1, scientific = FALSE,
                         digit1 = 1, digit2 = 1){
    if(is.null(x)) return(NULL)
    if(length(x)==0) return(NULL)
    if(length(x) != 1) stop("want length 1 vector")
    if(is.na(x)) return("")
    if(x==0) return(0)
    if(abs(x)>=1) return(round(x, digit1))
    if(abs(x)>=t) return(signif(x, digit2))
    if(scientific){
        format(x, digits = digit2, scientific = TRUE)
    } else {
        paste0("<", gsub("^0", "", t))
    }
}

## - # round_helper for vectors
roundisch <- function(x, ...){
    n <- length(x)
    R <- rep(NA_character_, n)
    for(i in 1:n) R[i] <- round_helper(x[i], ...)
    R
}
