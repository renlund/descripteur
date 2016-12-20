##' rbind dtables
##'
##' wrapper for \code{rbind}
##' @title rbind for dtables
##' @param x object 1
##' @param y object 2
##' @export
dtable_rbind <- function(x, y){
    if(is.null(x) | is.null(y)){
        r <- if(is.null(x)) return(y) else return(x)
    }
    a <- dattr(x)
    b <- dattr(y)
    if(!all(a == b)) warning("dtable attributes do not match")
    r <- rbind(as.data.frame(x), as.data.frame(y))
    ## names(r) <- names(x)
    class(r) <- c("dtable", "data.frame")
    dattr(r) <- a
    r
}

##' cbind dtables
##'
##' wrapper for \code{cbind} which keeps dtable attributes sane
##' @title cbind for dtables
##' @param x object 1
##' @param y object 2
##' @param groups add meta info to the groups
##' @export
dtable_cbind <- function(x, y, groups = NULL){
    mx <- dtable_order(x)
    my <- dtable_order(y)
    a <- attr(mx, "dtable")
    b <- attr(my, "dtable")
    if(!is.null(groups)){
        if(length(groups) != 2) groups <- c("", groups[1])
        a <- ifelse(a == "meta", a, paste0(a,
                    if(groups[1] == "") "" else ":", groups[1]))
        b <- ifelse(b == "meta", b, paste0(b, ":", groups[2]))
    }
    if(is.null(mx)){
        dattr(my) <- b
        return(my)
    }
    n_a <- names(mx)[a == "meta"]
    n_b <- names(my)[b == "meta"]
    ut <- n_b[n_b %in% n_a]
    if(all(mx$variable == my$variable & nrow(mx) == nrow(my))){
        tmp <- setdiff(names(my), ut)
        y_mod <- dtable_prune(my, rm = ut)
        r <- cbind(as.data.frame(mx), as.data.frame(y_mod))
    } else {
        message("Something doesn't quite add up. I'll try to fix it - but please check the results.")
        ax <- ay <- FALSE
        if(nrow(mx) >= nrow(y)) ax <- TRUE else ay <- TRUE
        r <- merge(as.data.frame(mx), as.data.frame(my), by = ut,
                   all.x = ax, all.y = ay)
        names(r) <- sub("\\.(x|y)$", "", names(r))
    }
    attr(r, "dtable") <- c(a, stats::na.omit(ifelse(names(my) %in% ut, NA,
                                             b)))
    class(r) <- c("dtable", class(r))
    dtable_order(r)
}

##' order dtables according to meta info
##'
##' place meta info to the left
##' @title order dtable
##' @param x object
##' @export
dtable_order <- function(x){
    a <- attr(x, "dtable")
    i <- c(which(a == "meta"), which(a != "meta"))
    r <- x[,i]
    attr(r, "dtable") <- a[i]
    names(r) <- names(x)[i]
    r
}

##' prune dtable
##'
##' remove columns by name or index
##' @title prune dtable
##' @param x object
##' @param rm index or variable name to remove
##' @param keep index or variable name to keep
##' @export
dtable_prune <- function(x, rm = NULL, keep = NULL){
    if(is.null(rm) & is.null(keep)) return(x)
    if(!is.null(rm) & !is.null(keep)){
        warning("It does not like to remove AND keep.\nIt will only remove.")
        keep <- NULL
    }
    d <- dattr(x)
    old_attr <- attributes(x)
    if(!is.null(rm)){
        if(is.character(rm)){
            rm <- which(names(x) %in% rm)
        }
    } else {
        if(is.character(keep)){
            rm <- which(!names(x) %in% keep)
        } else {
            rm <- setdiff(1:ncol(x), keep)
        }
    }
    r <- x[,-rm, drop = FALSE]
    names(r) <- names(x)[-rm]
    dattr(r) <- dattr(x)[-rm]
    attributes(r) <- concatenate_attributes(r, old_attr)
    r
}

##' turn dtable into data.frame
##'
##' this will just change the class attribute
##' @title dtable to data.frame
##' @param x dtable object
##' @param ... arguments passed
##' @export
as.data.frame.dtable <- function(x, ...){
    class(x) <- "data.frame"
    x
}

## -- helper fnc
concatenate_attributes <- function(x, a){
    haz <- attributes(x)
    add <- setdiff(names(a), names(haz))
    c(haz, a[add])
}
