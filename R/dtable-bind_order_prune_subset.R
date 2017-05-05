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
##' @param keep index or variable name to keep (specify this or 'rm' and not both)
##' @param info store the discarded information in attributes?
##' @param info.attr name of attribute to store discarded info (if \code{info = TRUE})
##' @param info.unique store only unique info (if \code{info = TRUE})
##' @param split.unique if \code{unique.info = TRUE}, also split into indivual
##'     sentences before determining uniqueness?
##' @export
dtable_prune <- function(x, rm = NULL, keep = NULL, info = FALSE,
                         info.attr = "info", info.unique = TRUE,
                         split.unique = TRUE){
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
            rm <- setdiff(seq_along(x), keep)
        }
    }
    if(info){
        infot <- unlist(lapply(x[,rm], identity))
        if(info.unique){
            if(split.unique){
                tmp <- unlist(strsplit(as.character(infot), split = ".", fixed = TRUE))
                infot <- gsub("(^ )|( $)", "", tmp)
            }
            infot <- unique(infot)
        }
    }
    r <- x[,-rm, drop = FALSE]
    names(r) <- names(x)[-rm]
    dattr(r) <- dattr(x)[-rm]
    attributes(r) <- concatenate_attributes(r, old_attr)
    if(info) attr(r, info.attr) <- c(attr(r, info.attr), infot)
    r
}

#-#' concatenate attributes
#-#' @param x atributes
#-#' @param a adders
concatenate_attributes <- function(x, a){
    haz <- attributes(x)
    add <- setdiff(names(a), names(haz))
    c(haz, a[add])
}

##' subset a dtable
##'
##' select rows in a dtable
##' @param x a dtable
##' @param ... arguments passed to \code{subset}
##' @param all.attr keep more than just the essential attributes?
##' @export
dtable_subset <- function(x, ..., all.attr = FALSE){
    xA <- attributes(x)
    if(!all.attr) xA <- clear_most_attr(xA)
    dots <- list(...)
    if(!is.null(dots$select)){
        warning("no selection!")
        dots$select <- NULL
    }
    dots$x <- as.data.frame(x)
    dots$drop <- FALSE
    r <- do.call(base::subset, dots)
    xA$row.names <- rownames(r)
    attributes(r) <- xA
    r
}

#-#' clear selected (most) attributes
#-#' @param attr attributes
clear_most_attr <- function(attr){
    keep <- c("names", "row.names", "dtable", "class")
    attr[keep]
}
