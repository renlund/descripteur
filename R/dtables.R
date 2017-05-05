##' dtable for multiple types
##'
##' concatenate dtables for mutiple types into a single dtable
##' @param data the data set
##' @param types types wanted
##' @param desc.flists flists for description
##' @param comp.flists flists for comparison
##' @param guide a dtable guide
##' @param ... arguments passed to dtable
##' @export
dtables <- function(data, types = NULL, desc.flists = NULL,
                    comp.flists = NULL, guide = NULL, ...){
    ok_types <- c("real", "bnry", "catg", "date", "surv")
    if(is.null(guide)) guide <- dtable_guide(data)
    if(is.null(types)) types <- intersect_if_notnull(names(desc.flists),
                                                     names(comp.flists))
    if(is.null(types)) types <- ok_types
    types <- intersect(types, unique(guide$type))
    if(is.null(desc.flists)) desc.flists <- flists_default(types = types,
                                                          thing = "desc")
    if(is.null(comp.flists)) comp.flists <- flists_default(types = types,
                                                          thing = "comp")
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
                                             comp.flist = comp.flists[[TYP]]),
                                        dots))
        if(nrow(tmp) == 0) next
        suppressWarnings(R <- if(is.null(R)) expr={
            tmp
        } else {
            dtable_rbind(R, tmp)
        })
    }
    mod_guide <- subset(guide, guide$type %in% c(types, "unit.id"))
    mod_guide$type <- "real" ## this choice should not matter
    META <- dtable(data, type = "real", desc = FALSE, comp = FALSE,
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
