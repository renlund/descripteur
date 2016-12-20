##' @describeIn dtable create a list of dtable, one for each type wanted
##' @param types a vector of the types wanted
##' @export
dtable_list <- function(data, types, ...){
    x <- list(...)
    if(!is.null(x$desc.flist) | !is.null(x$comp.flist)){
        stop(paste0("dtable_list will only take 'desc.flist' and 'comp.flist'",
                    " arguments from the defaults (which can be set with",
                    " the opts_desc$get-function)"))
    }
    R <- as.list(NULL)
    for(type in types){
        R[[type]] <- dtable(data, type = type, ...)
    }
    R
}

##' @describeIn dtable_latex create multiple latex tables from a dtable list
##' @param dtl a list of dtables
##' @param label labels for the tables
##' @param caption captions for the tables
##' @param use.sprintf if TRUE then labels and captions should be uniform in
##'     text apart from the specification of type, where a '\%s' should be.
##' @export
dtable_list_latex <- function(dtl, ..., label, caption, use.sprintf = TRUE){
    if(!is.list(dtl)) stop("'dtl' needs to be a list (of dtables)")
    types <- names(dtl)
    n_types <- length(dtl)
    types_code <- c("bnry", "real", "catg", "surv", "date")
    if(!all(types %in% types_code)) stop("there are unknown types in 'dtl'")
    if(use.sprintf){
        key1 <- stats::setNames(c("binary", "continuous", "categorical", "event",
                           "date"), types_code)
        foo <- function(s){ ## Capitalize key1
            paste0(toupper(substring(s,1,1)), substring(s,2,nchar(s)))
        }
        key2 <- unlist(lapply(key1, foo))
        ## use capitalization if %s is at the beginning of a sentence
        ## lab_key <- if(grepl("(^%s)|(\\. %s)", label))   key2 else key1 ## not needed
        cap_key <- if(grepl("(^%s)|(\\. %s)", caption)) key2 else key1
        ## label <- sprintf(lab, lab_key[types]) ## not needed
        label <- sprintf(label, types)
        caption <- sprintf(caption, cap_key[types])
    }
    if(!is.null(caption) & !is.null(label)){
        if(length(caption) != n_types) caption <- rep(caption, length = n_types)
        if(length(label) != n_types) label <- rep(label, length = n_types)
    }
    for(k in seq_along(types)){
        dtable_latex(dt = dtl[[k]], label = label[k], caption = caption[k], ...)
    }
    invisible(NULL)
}
