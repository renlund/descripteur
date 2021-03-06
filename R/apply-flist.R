##' apply function list
##'
##' apply the functions in flist to the variable 'x'
##' @title apply flist
##' @param x variable
##' @param flist list of functions
##' @param ... arguments passed to the functions in flist
##' @param xname name of variable
##' @export
apply_flist <- function(x, flist, ..., xname = NULL){
    r <- as.list(NULL)
    r$variable <- if(!is.null(xname)){
        xname
    } else {
        paste0(as.character(substitute(x)), collapse = "")
    }
    dots <- list(...)
    dots$x <- x
    dots$xname <- r$variable
    for(k in names(flist)){ ## k = names(flist)[1]
        r[[k]] <- if(is.function(fnc <- flist[[k]])){
                      ## fnc(x, ...)
                      do.call(what = fnc, args = dots)
                  } else {
                      tryCatch(as.character(flist[[k]]),
                               error = function(e) "*anomalie*")
                  }
    }
    r <- as.data.frame(r, stringsAsFactors = FALSE)
    class(r) <- c("dtable", "data.frame")
    attr(r, "dtable") <- c("meta", dattr(flist))
    r
}
