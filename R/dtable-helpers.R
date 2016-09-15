## function for setting up sane 'comp' and 'desc' defaults
dc_param <- function(desc = NULL, comp = NULL, glist = NULL){
    desc.style <- NULL
    comp.style <- NULL
    if(is.null(desc)) desc <- TRUE
    if(is.character(desc)){
        if(!desc %in% c("each", "first")){
            warning("desc misspecified, we'll fix")
        } else {
            desc.style <- desc
        }
        desc <- TRUE
    }
    if(is.character(comp)){
        if(!comp %in% c("overall", "across", "adjacent")){
            warning("comp misspecified, we'll fix")
        } else {
            comp.style <- comp
        }
        comp <- TRUE
    }
    if(is.null(glist)){
        if(is.null(comp)) comp <- FALSE
        if(comp){
            warning("cannot compare without a glist")
            comp <- FALSE
        }
        if(!desc){
            warning("strange to not want to describe now...")
        }
    } else {
        if(is.null(comp) | is.logical(comp)){
            if(is.null(comp)) comp <- TRUE
            if(is.null(comp.style)) comp.style <- "overall"
            if(!comp) comp.style <- NULL
        }
        if(desc){
            desc.style <- if(comp){
                if(comp.style == "overall") "each" else "first"
            } else {
                "each"
            }
        }
    }
    list("desc" = desc,
         "desc.style" = desc.style,
         "comp" = comp,
         "comp.style" = comp.style)
}

if(FALSE){
    dc_param()
    dc_param(desc = T, glist = 1)
    dc_param(glist=1)
    dc_param(comp = "across", glist = 1)
    dc_param(comp = "adjacent", glist = 1)

    r <- c()
    for(k in 1:3){
        r <- c(r, k)
        if(TRUE) break
    }
    r

}

