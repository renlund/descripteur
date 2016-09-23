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
##' @title rbind for dtables
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
    if(all(mx$variable == my$variable)){
        tmp <- setdiff(names(my), ut)
        ## y_mod <- subset(as.data.frame(my), TRUE, select = tmp)
        y_mod <- dtable_prune(my, rm = ut)
        r <- cbind(as.data.frame(mx), as.data.frame(y_mod))
    } else {
        message("why is 'variable' off? I'll try to fix it")
        ut2 <- setdiff(ut, "variable")
        y_mod <- subset(my, TRUE,
                        select = setdiff(names(my), ut2))
        r <- merge(mx, y_mod, by = "variable")
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

##' print dtable object
##'
##' prints the data.frame part as well as the dtable attributes
##'     if they are sane
##' @title print dtable
##' @param x dtable object
##' @param ... arguments passed
##' @export

print.dtable <- function(x, ...){
    cat("dtable object with data.frame:\n")
    print(as.data.frame(x), ...)
    a <- dattr(x)
    if(!is.null(a) & length(a) == length(x)){
        cat("\nwith dtable attributes:\n")
        r <- as.list(NULL)
        for(k in 1:length(a)) r[paste0("v", k)] <- a[k]
        r <- as.data.frame(r)
        names(r) <- names(x)
        print(r)
    } else {
        cat("\nwithout proper dtable attributes.\n")
    }
    invisible(NULL)
}

if(FALSE){

    x <- data.frame(variable = c("foo", "bar"),
                    mush = c("baz", "quuz"),
                    gee = c(1,2))
    attr(x, "dtable")  <- c("meta", "meta", "desc")
    y <- data.frame(variable = c("foo", "bar"),
                    gee = c(3,4),
                    mush = c("baz", "quuz"))
    attr(y, "dtable")  <- c("meta", "desc", "meta")
    (r <- dtable_cbind(x, y, groups = c("hoy", "jupp")))
    attr(r, "dtable")

    z <- data.frame(gee = c(5,6),
                    variable = c("foo", "bar"),
                    mush = c("baz", "quuz"))
    attr(z, "dtable")  <- c("desc", "meta", "meta")
    (r2 <- dtable_cbind(x = r, y = z, groups = c("bazooka")))
    attr(r2, "dtable")

    u <- data.frame(fruuu = c(7,8),
                    variable = c("foo", "bar"))
    attr(u, "dtable")  <- c("comp", "meta")
    (r3 <- dtable_cbind(x = r2, y = u))
    attr(r3, "dtable")

    (x <- apply_flist(1:10, flist = dr_def))
    attr(x, "dtable")
    (y <- apply_flist(2:10, flist = dr_sym))
    attr(y, "dtable")
    (r <- dtable_cbind(x, y))

    tmp1 <- structure(list(variable = structure(1L, .Label = "r1", class = "factor"),
    missing = 15, median = 19, IQR = 5), .Names = c("variable",
"missing", "median", "IQR"), row.names = c(NA, -1L), class = c("dtable",
"data.frame"), dtable = c("meta", "desc", "desc", "desc"))

    tmp2 <- structure(list(variable = structure(1L, .Label = "r1", class = "factor"),
    missing = 16, median = 20.5, IQR = 6), .Names = c("variable",
"missing", "median", "IQR"), row.names = c(NA, -1L), class = c("dtable",
"data.frame"), dtable = c("meta", "desc", "desc", "desc"))

    (tmp <- dtable_cbind(x = tmp1, y = tmp2, groups = letters[1:2]))
    attr(tmp, "dtable")

    x <- data.frame(variable = c("foo", "bar"),
                    mush = c("baz", "quuz"),
                    gee = c(1,2))
    attr(x, "dtable")  <- c("meta", "meta", "desc")
    y <- data.frame(variable = c("foo", "bar"),
                    gee = c(3,4),
                    mush = c("baz", "quuz"))
    attr(y, "dtable")  <- c("meta", "desc", "meta")
    (r <- dtable_cbind(x, y, groups = c("hoy", "jupp")))
    attr(r, "dtable")

    z <- data.frame(gee = c(5,6),
                    variable = c("foo", "bar"),
                    mush = c("baz", "quuz"))
    attr(z, "dtable")  <- c("desc", "meta", "meta")
    (r2 <- dtable_cbind(x = r, y = z, groups = c("bazooka")))
    attr(r2, "dtable")

    u <- data.frame(fruuu = c(7,8),
                    variable = c("foo", "bar"))
    attr(u, "dtable")  <- c("comp", "meta")
    (r3 <- dtable_cbind(x = r2, y = u))
    attr(r3, "dtable")

    (x <- apply_flist(1:10, flist = dr_def))
    attr(x, "dtable")
    (y <- apply_flist(2:10, flist = dr_sym))
    attr(y, "dtable")
    (r <- dtable_cbind(x, y))

    tmp1 <- structure(list(variable = structure(1L, .Label = "r1", class = "factor"),
    missing = 15, median = 19, IQR = 5), .Names = c("variable",
"missing", "median", "IQR"), row.names = c(NA, -1L), class = c("dtable",
"data.frame"), dtable = c("meta", "desc", "desc", "desc"))

    tmp2 <- structure(list(variable = structure(1L, .Label = "r1", class = "factor"),
    missing = 16, median = 20.5, IQR = 6), .Names = c("variable",
"missing", "median", "IQR"), row.names = c(NA, -1L), class = c("dtable",
"data.frame"), dtable = c("meta", "desc", "desc", "desc"))

    (tmp <- dtable_cbind(x = tmp1, y = tmp2, groups = letters[1:2]))
    attr(tmp, "dtable")

}
