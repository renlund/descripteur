##' set 'dtable'
##'
##' @description shortcut and sanity check for \code{attr(x,
##'     "dtable")}
##'
##' @title set 'dtable' attribute
##' @param x object
##' @export

dattr <- function(x){
    a <- attr(x, "dtable")
    n <- length(x)
    if(length(a) != n) warning("'dtable' attributes has wrong length")
    a
}

##' get 'dtable'
##'
##' shortcut and sanity check for \code{"attr<-"(x, "dtable", value)}
##'
##' @title change 'dtable' attribute
##' @param x object
##' @param value 'dtable' value
##' @export

"dattr<-" <- function(x, value){
    n <- length(x)
    if(length(value) != n) warning("'dtable' attributes has wrong length")
    "attr<-"(x, "dtable", value)
}

