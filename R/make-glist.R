##' make a grouping list
##'
##' description tables with grouping requires a list of indexes to
##'     group on. Often this should be equivalent to categorization
##'     according to some grouping variable. If so, the 'glist' needed
##'     can be created by either specifying that variable or the name
##'     of that variable in data source 'ref'.
##' @title make glist
##' @param x object
##' @param ref reference
##' @export

make_glist <- function(x, ref = NULL){
    if(!is.null(ref)){
        if(is.data.frame(ref)){
            if(is.character(x)){
                x <- ref[[x]]
            } else {
                if(length(x) != nrow(ref)) stop("nah1")
            }
        } else {
            if(length(x) != length(ref)) stop("nah2")
        }
    }
    y <- as.factor(x)
    if(length(levels)>100) stop("nah3")
    g <- as.list(NULL)
    for(k in levels(y)){
        g[[k]] <- y == k
    }
    g
}
