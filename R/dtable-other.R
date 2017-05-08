##' print method for dtable_other objects
##'
##' dtable_other objects are objects with an additional attribute 'dtable_other'
##'     which will be printed alongside the inherited print method
##' @param x the object
##' @param ... for compatibility
##' @param sep if multiple dtable_other attributes, how to separate them
##' @export
print.dtable_other <- function(x, ..., sep = "\n"){
    dtable_other <- attr(x, "dtable_other")
    class(x) <- setdiff(class(x), "dtable_other")
    co <- utils::capture.output(print(x))
    co.n <- max(nchar(co))
    w <- options("width")$width
    print(x)
    cat(paste0(rep("-", min(co.n, w)), collapse = ""))
    cat("\n", paste0(dtable_other, collapse = sep), "\n", sep = "")
    invisible(NULL)
}

##' latex method for dtable_other objects
##'
##' dtable_other objects are objects with an additional attribute 'dtable_other'
##'     which will be printed alongside the inherited latex method
##' @param x object
##' @param file \code{Hmisc::latex}
##' @param where for \code{Hmisc::latex}
##' @param rowname for \code{Hmisc::latex}
##' @param sep if multiple dtable_other attributes, how to separate them
##' @param ... arguments passed to \code{Hmisc::latex}
##' @export
latex.dtable_other <- function(x, file = "", where = "htb",
                               rowname = NULL, sep = "\\\\", ...){
    dtable_other <- attr(x, "dtable_other")
    ib <- paste0(dtable_other, collapse = sep)
    class(x) <- setdiff(class(x), "dtable_other")
    Hmisc::latex(object = x, file = file, where = where,
                 rowname = rowname, insert.bottom = ib, ...)
}
