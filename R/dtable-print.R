##' print dtable object
##'
##' prints the data.frame part as well as the dtable attributes
##'     if they are sane
##' @title print dtable
##' @param x dtable object
##' @param ... arguments passed
##' @export
print.dtable <- function(x, ...){
    cat("## data.frame:\n")
    print(as.data.frame(x), ...)
    a <- dattr(x)
    if(!is.null(a) & length(a) == length(x)){
        co <- utils::capture.output(print(as.data.frame(x), ...))
        co.n <- max(nchar(co))
        w <- options("width")$width
        cat(paste0(rep("-", min(co.n, w)), collapse = ""), sep = "")
        cat("\n## dtable attributes:\n")
        r <- as.list(NULL)
        for(k in 1:length(a)) r[paste0("v", k)] <- a[k]
        r <- as.data.frame(r)
        names(r) <- names(x)
        print(r)
        co2 <- utils::capture.output(print(r))
        co2.n <- max(nchar(co2))
    } else {
        cat("\n## dtable attributes are not ok\n")
        co2.n <- 29
    }
    cat(paste0(rep("-", min(co2.n, w)), collapse = ""), sep = "")
    cat("\n## selected attributes:\n")
    print(dtable_attr(x))
    invisible(NULL)
}

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

# - # turn dtable_attr into text
attr2text <- function(dt, perc = FALSE, perc.sign = "%",
                      attr = c("size", "cc", "weight", "units", "info"),
                      sep = ". ", vector = FALSE){
    da <- dtable_attr(dt, perc = perc, perc.sign = perc.sign)
    gr <- setdiff(names(da), c("measure", "total"))
    n <- length(gr)
    foo <- function(m, g, text){
        if(!m %in% attr) return(NULL)
        x <- subset(da, da$measure == m)
        if(nrow(x)==0) return(NULL)
        a <- x$total
        b <- if(g) as.character(x[1, 3:(2+n)]) else NULL
        c <- if(g){
                 paste0(" (", paste0(paste0(gr, ":", b), collapse = ", "), ")")
             } else NULL
        paste0(text, " ", a, c)
    }
    r <- c(
        foo(m = "size", g = n>0, text = "Rows:"),
        foo("cc", n>0, "Complete Cases:"),
        foo("weight", n>0, "Weight:"),
        foo("units", n>0, "Units:"),
        if("info" %in% attr) attr(dt, "info") else NULL
    )
    if(vector) return(r)
    s <- c(rep(sep, length.out = max(length(r)-1, 0)), "")
    R <- paste(r, s, sep = "", collapse = "")
    if(R == "") NULL else R
}

if(FALSE){

    devtools::load_all()
    df <- dtable_data_example(n = 111)
    g <- dtable_guide(df, unit.id = "id")
    dt <- dtable(df, "real", glist = "b1", guide = g, w = "vikt")
    attr(dt, "info") <- "Some interesting detail."
    dtable_attr(dt)
    dtable_attr(dt, perc = T)
    str(dtable_attr(dt, perc = T))
    attr2text(dt)
    attr2text(dt, perc = T)
    attr2text(dt, sep = ". ")
    cat(attr2text(dt, sep = "\n"))
    attr2text(dt, sep = c(". ", "... "))
    attr2text(dt, vector = TRUE)
    attr2text(dt, attr = c("size", "info"), sep = ". ")
    attr2text(dt, attr = NULL)

    attr2text(dt)
    attr2text(dt, perc = T, perc.sign = "%")
    dtable_attr(dt, perc = FALSE)
    dtable_attr(dt, perc = TRUE)

    dtable_latex(dt)

}
