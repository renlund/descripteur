##' create function list
##'
##' create a function list by specifying the names of functions,
##'     primarily searching in the descripteur namespace but also in
##'     other packages
##' @title flist
##' @param x vector of function names
##' @param dattr the dtable attribute
##' @export
flist <- function(x, dattr= NULL){
    l <- as.list(NULL)
    da <- rep(NA, length(x))
    atr <- if(is.null(dattr)) "desc" else dattr
    for(k in seq_along(x)){
        xk <- NULL
        l[[k]] <- tryCatch(xk <- get(x[k],
                               envir = getNamespace("descripteur"),
                               inherits = TRUE),
                           error = function(e) function(...)
                               "unknown fnc")
        da[k] <- if(is.null(tmp <- attr(xk, "dtable"))) atr else tmp
    }
    names(l) <- if(is.null(names(x))){
        x
    } else {
        ifelse(names(x) == "", x, names(x))
    }
    dattr(l) <- da
    l
}

## unknow_fnc <- function(e) function(...){
##     "unknown fnc"
## }

if(FALSE){

    (fl <- flist(x = c("foo" = "d.OR", "bar" = "mean")))
    (fl <- flist(x = c("foo" = "d.OR", "qwerty" = "woot", "mean" = "mean")))
    (fl <- flist(x = c("foo" = "d.OR", "qwerty" = "woot", "mean")))

}
