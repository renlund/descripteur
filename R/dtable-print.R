##' print dtable object
##'
##' prints the data.frame part as well as the dtable attributes
##'     if they are sane
##' @title print dtable
##' @param x dtable object
##' @param ... arguments passed
##' @export
print.dtable <- function(x, ...){
    cat("## data.frame:\n")
    print(as.data.frame(x), ...)
    a <- dattr(x)
    if(!is.null(a) & length(a) == length(x)){
        co <- utils::capture.output(print(as.data.frame(x), ...))
        co.n <- max(nchar(co))
        w <- options("width")$width
        cat(paste0(rep("-", min(co.n, w)), collapse = ""), sep = "")
        cat("\n## dtable attributes:\n")
        r <- as.list(NULL)
        for(k in seq_along(a)) r[paste0("v", k)] <- a[k]
        r <- as.data.frame(r)
        names(r) <- names(x)
        print(r)
        co2 <- utils::capture.output(print(r))
        co2.n <- max(nchar(co2))
    } else {
        cat("\n## dtable attributes are not ok\n")
        co2.n <- 29
    }
    cat(paste0(rep("-", min(co2.n, w)), collapse = ""), sep = "")
    cat("\n## selected attributes:\n")
    print(dtable_attr(x))
    if(!is.null(oth <- attr(x, "other"))){
        co3 <- utils::capture.output(print(dtable_attr(x)))
        co3.n <- max(nchar(co3))
        cat(paste0(rep("-", min(co3.n, w)), collapse = ""), sep = "")
        cat("\n## other types:\n  ")
        tmp <- lapply(oth, paste0, collapse = ", ")
        cat(paste0(paste0(names(tmp), ": ", tmp), collapse = "\n  "), "\n")
    }
    invisible(NULL)
}
