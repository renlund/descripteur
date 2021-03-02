##' get attributes as text
##'
##' extract attributes of dtable to a string (or character vector)
##' @param dt a dtable
##' @param perc use percentages
##' @param perc.sign percentage sign
##' @param lessthan 'strictly less than' sign
##' @param attr which attributes to extract
##' @param sep separator between attributes for output
##' @param vector return vector instead of a single string
##' @param rm.if.all exclude info on units if all unique
##' @export
attr2text <- function(dt, perc = FALSE, perc.sign = "%", lessthan = "<",
                      attr = c("size", "cc", "weight", "units", "info"),
                      sep = ". ", vector = FALSE, rm.if.all = FALSE){
    if(nrow(dt) == 0) return("")
    da <- dtable_attr(dt, perc = perc, perc.sign = perc.sign,
                      lessthan = lessthan)
    gr <- setdiff(names(da), c("measure", "total"))
    n <- length(gr)
    foo <- function(m, g, text){
        if(!m %in% attr) return(NULL)
        N <- subset(da, da$measure == "size")$total
        x <- subset(da, da$measure == m)
        if(nrow(x)==0) return(NULL)
        a <- x$total
        if(m == "cc" & a == paste0("100", perc.sign)){
            if(rm.if.all) NULL else paste(text, a)
        } else if(m == "units" & a == N){
            if(rm.if.all) NULL else paste("All units unique")
        } else {
            b <- if(g) as.character(x[1, 3:(2+n)]) else NULL
            c <- if(g){
                     paste0(" (", paste0(paste0(gr, ":", b),
                                         collapse = ", "), ")")
                 } else NULL
            paste0(text, " ", a, c)
        }
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


other2text <- function(dt, constant = TRUE, ignored = FALSE,
                       unknown = FALSE, rmus = TRUE){
    RU <- function(x, L) if(L) gsub("_", "\\_", x, fixed = TRUE) else x
    oth <- attributes(dt)$other
    R <- NULL
    if(!is.null(co <- oth$constant) & constant){
        x <- paste0("Constant variables: ",
                    paste0(RU(co, rmus), collapse = ", "),
                    ".")
        R <- c(R, x)
    }
    if(!is.null(ig <- oth$ignored) & ignored){
        x <- paste0("Ignored variables: ",
                    paste0(RU(ig, rmus), collapse = ", "),
                    ".")
        R <- c(R, x)
    }
    if(!is.null(un <- oth$unknown) & unknown){
        x <- paste0("Unknown variables: ",
                    paste0(RU(un, rmus), collapse = ", "),
                    ".")
        R <- c(R, x)
    }
    R
}
