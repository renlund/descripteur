##' create footnotes from table variables
##'
##' make \code{unique(variable)} a part of attribute 'info' and, if wanted,
##'     place a footnote on another variable
##' @param dt a dtable
##' @param text the variables to add to 'info'
##' @param footnote the variables to add to 'info' and the variables to map upon,
##'     or so it is thought to behave if ever implemented
##' @export
footnote <- function(dt, text = NULL, footnote = NULL){
                     ## format = FALSE,
                     ## format.param = as.list(NULL)){
    if(is.null(text) & is.null(footnote)){
        message("no action has been performed on 'dt'")
        return(dt)
    }
    old.info <- attr(dt, "info")
    ## ## formating only relevant when mapping of symbols occur
    ## if(length(format.param)>0) format <- TRUE
    ## if(format) dt <- do.call(dtable_format,
    ##                         c(dt = list(dt), format_fixer(format.param)))
    if(!is.null(text)){
        tmp <- sort_out_text_index_and_names(text, names(dt))
        n.text <- tmp$n
        i.text <- tmp$i
        text_info <- unique(unlist(lapply(dt[,i.text], identity)))
    }
    if(!is.null(footnote)){
        ## -----  NOT YET ------ ##
        stop("the 'footnote' argument of the footnote function not yet implemented")
        tmp <- sort_out_footnote_index_and_names(footnote, names(dt))
        n.fn <- tmp$n
        i.fn <- tmp$i
        n.fn_names <- tmp$n2
        i.fn_names <- tmp$i2
        fn_info <- unlist(lapply(dt[,i.fn]))
        ## here we need to map values of each column to distinct latex symbols
        ## which needs to be pasted onto the other columns
    }
    attr(dt, "info") <- c(old.info, text_info)
    dtable_prune(dt, rm = i.text)
}


sort_out_text_index_and_names <- function(text, names_dt){
    if(is.character(text)){
        if(!all(text %in% names_dt)){
            stop("not all 'text' in names('dt')")
        }
        n.text <- text
        i.text <- which(names_dt %in% text)
    } else {
        n.text <- names_dt[text]
        i.text <- text
    }
    list(n = n.text, i = i.text)
}

sort_out_footnote_index_and_names <- function(fn, names_dt){
    n.fn_name <- names(footnote)
    if(!all(n.fn_name %in% names_dt)){
        i.fn_names <- tryCatch(as.numeric(n.fn_name),
                               warning = function(w){
                                   stop("bad names of 'footnote'")
                               })
        if(max(i.fn_names)>ncol(dt)) stop("bad index names of 'footnote'")
        n.fn_names <- names_dt[i.fn_names]
    } else {
        i.fn_names <- which(names_dt %in% n.fn_names)
    }
    if(is.character(footnote)){
        if(!all(footnote %in% names_dt)){
            stop("not all 'footnote' in names('dt')")
        }
        n.fn <- footnote
        i.fn <- which(names_dt == footnote)
    } else {
        n.fn <- names_dt[footnote]
        i.fn <- footnote
    }
    list(i = i.fn, n = n.fn, i2 = i.fn_names, n2 = n.fn_names)
}
