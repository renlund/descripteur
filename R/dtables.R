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
    message("dtables function is still experimental\nHigly likely to change!\n")
    ok_types <- c("real", "bnry", "catg", "date", "surv")
    if(is.null(types)) types <- ok_types
    if(is.null(desc.flists)) desc.flists <- desc_get("desc_compact")
    if(is.null(comp.flists)) comp.flists <- desc_get("comp_compact")
    d.f <- names(desc.flists)
    c.f <- names(comp.flists)
    dc.f <- union(d.f, c.f)
    all_types <- union(types, dc.f)
    if(!all(all_types %in% ok_types)){
        wot <- paste0(setdiff(all_types, ok_types), collapse = ", ")
        stop(paste0("some types specified are unknow: ", wot, "."))
    }
    if(is.null(all_types)) return(as.data.frame(NULL))
    defl <- if(!setequal(d.f, all_types)){
                NAMES <- names(desc.flists[[1]])
                tmp <- flists_default(types = setdiff(all_types, d.f))
                c(desc.flists, tmp)
            } else desc.flists
    cofl <- if(!setequal(c.f, all_types)){
                NAMES <- names(comp.flists[[1]])
                tmp <- flists_default(types = setdiff(all_types, c.f))
                c(comp.flists, tmp)
            } else comp.flists
    if(TRUE){ ## checks
        d.n <- unlist(lapply(defl, length))
        c.n <- unlist(lapply(cofl, length))
        test <- all(names(defl) %in% ok_types) &
            all(names(cofl) %in% ok_types) &
            all(d.n == max(d.n)) &
            all(c.n == max(c.n))
        if(!test) stop("...somethings wrong")
    }
    ## n <- length(all_types)
    if(is.null(guide)) guide <- dtable_guide(data)
    R <- NULL
    for(TYP in all_types){ ## TYP = all_types[1]
        tmp <- dtable(data, type = TYP, guide = guide,
                      desc.flist = flist(unlist(defl[[TYP]])),
                      comp.flist = flist(unlist(cofl[[TYP]])), ...)
        suppressWarnings(R <- if(is.null(R)) {
                 tmp
             } else {
                 dtable_rbind(R, tmp)
             })
    }
    mod_guide <- subset(guide, guide$type %in% all_types)
    mod_guide$type <- "real" ## this choice should not matter
    META <- dtable(data, type = "real", desc = FALSE, comp = FALSE, ...)
    aM <- attributes(META)
    transf <- setdiff(names(aM), c("names", "row.names", "class"))
    for(K in transf) attr(R, K) <- attr(META, K)
    R
}
