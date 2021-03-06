##' create function list
##'
##' create a function list by specifying the names of functions,
##'     primarily searching in the descripteur namespace but also in
##'     other packages
##' @title flist
##' @param x vector of function names
##' @param dattr the dtable attribute
##' @param local if \code{TRUE}, look for function first locally, then in
##'     'descripteur'
##' @export
flist <- function(x, dattr= NULL, local = FALSE){
    l <- as.list(NULL)
    da <- rep(NA, length(x))
    atr <- if(is.null(dattr)) "desc" else dattr
    for(k in seq_along(x)){
        xk <- NULL
        l[[k]] <- tryCatch(xk <- get(x[k],
                                     envir = if(local){
                                                 as.environment(-1)
                                             } else {
                                                 getNamespace("descripteur")
                                             },
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

##' create flists
##'
##' flists is a list of one or more flist, one for each type wanted (and named
##' thereafter)
##' @param real logical or an flist
##' @param bnry logical or an flist
##' @param catg logical or an flist
##' @param date logical or an flist
##' @param surv logical or an flist
##' @param names row names wanted
##' @param thing character; 'desc', 'comp' or 'test'
##' @export
flists <- function(real = FALSE, bnry = FALSE, catg = FALSE,
                   date = FALSE, surv = FALSE, names = NULL,
                   thing = "desc"){
    NAMES <- names
    if(!thing %in% c("desc", "comp", "test")) stop("nah1")
    types <- c("real", "bnry", "catg", "date", "surv")
    foo <- function(x){
        ## tmp <- get(x, envir = environment(), inherits  = FALSE)
        tmp <- get(x, pos = -1, inherits  = TRUE) ## ... is this right??
        is.null(tmp) || is.na(tmp)
    }
    if(any(unlist(lapply(types, foo)))) stop("nah2") ## check for NULLs or NAs
    freal <- fbnry <- fcatg <- fdate <- fsurv <- NULL ## to pass R CMD CHECK
    bar <- function(x){
        ## tmp <- get(x, envir = environment(), inherits = FALSE)
        tmp <- get(x, pos = -1, inherits  = TRUE) ## ... is this right??
        if(is.logical(tmp)){
            if(tmp){
                if(thing == "desc"){
                    desc_get(paste0("describe_", x, "_compact"))
                } else if(thing == "comp") {
                    desc_get(paste0("compare_",  x, "_compact"))
                } else if(thing == "test"){
                    desc_get(paste0("test_",  x, "_compact"))
                }
            } else NULL
        } else tmp
    }
    L <- as.list(NULL)
    dummy <- 0
    for(K in types){ ## K = types[1]
        tmp <- bar(K)
        if(!is.null(tmp)){
            if(is.null(names) && dummy == 0){
                NAMES <- names(tmp)
                dummy <- 1
            } else {
                tryCatch(names(tmp) <- NAMES,
                         error = function(e){
                             stop(paste0("somethings wrong, perhaps names do",
                                         " not match flist lengthwise?"))
                         })
            }
            L[[K]] <- tmp
        }
    }
    L
}

# - # easier way to get the defaults
flists_default <- function(types, names = NULL, thing = "desc"){
    ## types <- intersect(types, dtable_types()) ## not needed
    flists(
        real = "real" %in% types,
        bnry = "bnry" %in% types,
        catg = "catg" %in% types,
        date = "date" %in% types,
        surv = "surv" %in% types,
        names = names, thing = thing
    )
}
