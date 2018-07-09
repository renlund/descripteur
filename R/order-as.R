##' ordering of vector
##'
##' order a given vector, which may contain duplicates, according to the
##'   wanted order given by some other vector
##' @param given the vector that needs ordering
##' @param wanted the wanted order
##' @param incl.unordered should given elements not in wanted be kept?
##' @return an index vector
##' @export
order_as <- function(given, wanted, incl.unordered = TRUE){
    want <- wanted[wanted %in% given]
    foo <- function(X) {
        n <- nrow(X)
        X$nr <- if(n==1) "" else 1:n
        X$attention  <- if(n==1) 0 else c(rep(0, n-1), 1)
        X$edited <- paste0(X$given, X$nr)
        X
    }
    df <- data.frame(given = given, stringsAsFactors = FALSE)
    spl <- lapply(split(df, f = df$given), foo)
    dc <- unsplit(spl, f = df$given)
    rownames(dc) <- NULL
    sdc <- subset(dc, attention == 1)
    lw <- as.list(want)
    names(lw) <- want
    for(k in seq_along(sdc$given)){ ## k = 1
        K <- as.character(sdc$given[k])
        n <- sdc$nr[k]
        lw[[K]] <- sprintf(paste0(lw[[K]], "%s"), 1:n)
    }
    W <- unlist(lw)
    G <- dc$edited
    indx <- match(W, G)
    rest <- setdiff(1:length(given), indx)
    if(incl.unordered){
        c(indx, rest)
    } else {
        indx
    }
}
