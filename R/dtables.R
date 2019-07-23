##' dtable for multiple types
##'
##' concatenate dtables for multiple types into a single dtable
##' @param data the data set
##' @param guide a dtable guide
##' @param ... arguments passed to dtable
##' @param types types wanted
##' @param desc.flists flists for description
##' @param comp.flists flists for comparison
##' @param test.flists flists for testing
##' @export
dtables <- function(data, guide = NULL, ..., types = NULL,
                    desc.flists = NULL, comp.flists = NULL, test.flists = NULL){
    ok_types <- descripteur_desc_types() ## c("real", "bnry", "catg", "date", "surv")
    if(is.null(guide)) guide <- dtable_guide(data)
    if(is.null(types)) types <- intersect_if_notnull(names(desc.flists),
                                intersect_if_notnull(names(comp.flists),
                                                     names(test.flists)))
    if(is.null(types)) types <- ok_types
    types <- intersect(types, unique(guide$type))
    if(is.null(desc.flists)) desc.flists <- flists_default(types = types,
                                                          thing = "desc")
    if(is.null(comp.flists)) comp.flists <- flists_default(types = types,
                                                          thing = "comp")
    if(is.null(test.flists)) test.flists <- flists_default(types = types,
                                                           thing = "test")
    if(!all(types %in% ok_types)){
        wot <- paste0(setdiff(types, ok_types), collapse = ", ")
        stop(paste0("some types specified are unknow: ", wot, "."))
    }
    R <- NULL
    dots <- list(...)
    for(TYP in types){ ## TYP = types[1] ## TYP = "catg"
        if(!TYP %in% guide$type) next
        ## cat("typ:", TYP, "\n")
        tmp <- do.call(dtable, args = c(list(data = data, type = TYP,
                                             guide = guide,
                                             desc.flist = desc.flists[[TYP]],
                                             comp.flist = comp.flists[[TYP]],
                                             test.flist = test.flists[[TYP]]),
                                        dots))
        if(nrow(tmp) == 0) next
        suppressWarnings(R <- if(is.null(R)){
            tmp
        } else {
            dtable_rbind(R, tmp)
        })
    }
    mod_guide <- subset(guide, guide$type %in% c(types, "unit.id"))
    mod_guide$type <- "real" ## this choice should not matter
    META <- dtable(data, type = "real",
                   desc = FALSE, comp = FALSE, test = FALSE,
                   guide = mod_guide, glist = dots$glist)
    aM <- attributes(META)
    transf <- setdiff(names(aM), c("names", "row.names", "class", "dc_param"))
    for(K in transf) attr(R, K) <- attr(META, K)
    R
}

intersect_if_notnull <- function(a, b){
    if(is.null(a) & is.null(b)){
        NULL
    } else if(is.null(a)){
        b
    } else if(is.null(b)){
        a
    } else {
        intersect(a, b)
    }
}

##' standard dtables to latex operations
##'
##' helper function for common transformation of ungrouped dtables object into
##'     latex output; remove column 'variable' and put 'info' in info attribute
##' @param dt an object from \code{dtables}
##' @param format argument passed to \code{dtable_latex}
##' @param ... arguments passed to \code{dtable_latex}
##' @export
dtables2latex_ungrouped_helper <- function(dt, format = TRUE, ...){
    a1 <- dtable_prune(x = dt, rm = "variable")
    a2 <- dtable_prune(x = a1, rm = "info", info = TRUE)
    dtable_latex(a2, format = format, ...)
}

##' @describeIn dtables2latex_ungrouped_helper helper function for common
##'     transformation of grouped dtables object into; in addition to
##'     \code{dtables2latex_ungrouped_helper}, also put column 'pinfo' as a
##'     footnote to column 'p' (and format)
##' @export
dtables2latex_grouped_helper <- function(dt, format = TRUE, ...){
    a1 <- dtable_prune(x = dt, rm = "variable")
    a2 <- dtable_prune(x = a1, rm = "info", info = TRUE)
    a3 <- if("pinfo" %in% names(dt)){
              dtable_fnote(dt = a2, info = "pinfo", fn.var = "p",
                           format = format)
          } else {
              dtable_format(a2)
          }
    dtable_latex(a3, ...)
}

##' for exporting dtables (experimental)
##'
##' make a dtable ready to write to file
##' @param dt a dtables
##' @param rm columns to remove
##' @param reps vector of replacements, where the name replaces the entry,
##'     e.g. \code{c('foo' = 'bar')} will replace all 'bar' with 'foo'
##' @param format use \code{dtable_format}?
##' @export
dtables2file_helper <- function(dt, rm = NULL, reps = NULL, format = FALSE){
    if(is.null(reps)) reps <- c("   " = "\\quad:", "%" = "\\%")
    if(is.null(rm)) rm <- intersect(names(dt), c("variable", "pinfo", "p", "info"))
    a1 <- dtable_prune(dt, rm = rm)
    if(format) a1 <- dtable_format(a1)
    for(i in seq_along(reps)){
        a1[] <- lapply(a1[], FUN = function(x) gsub(reps[i], names(reps)[i], x=x, fixed = TRUE))
    }
    ## a1[] <- lapply(a1[], FUN = function(x) gsub("\\quad:", "   ", x=x, fixed = TRUE))
    ## a1[] <- lapply(a1[], FUN = function(x) gsub("\\%", "%", x=x, fixed = TRUE))
    a1
}

##' @describeIn dtables2file_helper quick peek at dtable (in particular dtables)
##' @export
peek <- function(dt){
    dtables2file_helper(dt, format = TRUE)
}
