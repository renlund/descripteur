##' create descriptive table
##'
##' create a description of variables of a given type in your
##'     data. see the vignette for examples
##' @title create descriptive table
##' @param data a \code{data.frame} or such object
##' @param type what kind of variables to you want to describe? 'real', 'bnry',
##'     'cat', 'date' and 'surv' are available
##' @param guide point to guide or get the default
##' @param desc if to describe variables
##' @param desc.flist list of describers, i.e. describing functions
##' @param comp if, and how, to compare variables (requires a glist)
##' @param comp.flist list of comparers, i.e. comparing functions
##' @param glist grouping list, if wanted. This can either be a list of logical
##'     vectore equal in length to the numbers of rows (i.e. logical indices), in
##'     which case overlapping groups can be made, or the name of a variable in
##'     the data frame (in which case that variable will be removed from output)
##'     or just any variable in the global workspace
##' @param w weights, if wanted
##' @param useNA how to handle \code{NA} (requires that functions in
##'     flist has this argument)
##' @param ... arguments passed
##' @export
dtable <- function(data, type = NULL, guide = NULL,
                   desc = NULL, desc.flist = NULL,
                   comp = NULL, comp.flist = NULL,
                   glist = NULL, w = NULL, useNA = "no", ...){
    if(!useNA %in% c("ifany", "always", "no")){
        message("wrong useNA specification (set to 'no')")
        useNA <- "no"
    }
    if(is.null(guide)) guide <- dtable_guide(data = data)
    if(is.logical(guide)){
        guide_val <- guide
        guide <- dtable_guide(data = data)
        if(!guide_val){
            filter <- !guide$type %in% c("row.id", "unit.id")
            guide$type[filter] <- "real" ## this particular value should not matter
        }
    }
    ## you can skip 'type' but if the guide contains several different types
    ## this will raise a warning
    if(is.null(type)){
            tmp <- setdiff(guide$type, c("id.row", "id.unit"))
            type <- tmp[1]
            if(length(unique(tmp)) != 1){
                warning(paste0("no type given, has been set to ", type, "."))
            }
    }
    if(!type %in% c("real","bnry", "date", "catg", "surv")){
        stop("type not supported")
    }
    P <- dc_param(desc = desc, comp = comp, glist = glist)
    glist.variable <- NULL
    if(!is.null(glist)){
        if(is.character(glist)){
            glist.variable <- data[[glist]]
            guide <- guide[guide$variable != glist,]
            glist <- make_glist(x = glist, ref = data)
        }
        if(!is.list(glist)){
            glist.variable <- glist
            glist <- tryCatch(make_glist(glist, ref = data[[1]]),
                              error = function(e)
                                  stop("cannot make glist from this glist-argument"))
        }
        if(length(glist) == 1) stop("only 1 subgroup defined by glist")
    }
    if(!is.null(w)){
        if(is.character(w)){
            w = data[[w]]
            if(is.null(w))
                warning("weighting variable does not exist in data")
        }
        if(length(w) != nrow(data))
            stop("bad weighting (want same length as data)")
        if(any(is.na(w))){
            warning("weight has NA:s")
        }
        if(any(w<0)){
            warning("weight has negative elements")
        }
    }
    gvar <- guide[guide$type == type,]
    if(!P$desc & !P$comp) {
        R <- as.data.frame(NULL)
    } else {
        d_fnc <- if(!is.null(desc.flist)){
                     desc.flist
                 } else {
                     opts_desc$get(paste0("describe_", type))
                 }
        dattr_d_fnc <- attr(d_fnc, "dtable")
        if(length(dattr_d_fnc) != length(d_fnc)){
            warning("dattr for describers is off, reset to 'desc'")
            attr(d_fnc, "dtable") <- rep("desc", length(d_fnc))
        }
        c_fnc <- if(!is.null(comp.flist)){
                     comp.flist
                 } else {
                     opts_desc$get(paste0("compare_", type))
                 }
        dattr_c_fnc <- attr(c_fnc, "dtable")
        if(length(dattr_c_fnc) != length(c_fnc)){
            warning("dattr for comparers is off, reset to 'comp'")
            attr(c_fnc, "dtable") <- rep("comp", length(c_fnc))
        }
        R1 <- NULL
        R2 <- NULL
        has_na <- any(gvar$has_missing)
        use_na <- if(useNA != "ifany") useNA == "always" else has_na
        if(P$desc){
            for(g in gvar$variable){ ## g <- gvar$variable[2]
                ## x <- data[[g]]
                x <- if(type %in% c("bnry", "catg")){
                         factor(data[[g]], levels = attr(guide, "levels")[[g]])
                     } else {
                         data[[g]]
                     }
                lab <- gvar$label[gvar$variable == g][1]
                R0 <- NULL
                if(is.null(glist)){
                    R0 <- apply_flist(x = x, flist = d_fnc, w = w,
                                      useNA = use_na, xname = lab, ...)
                } else {
                    for(k in seq_along(glist)){ ## k = 1
                        tmp <- apply_flist(x = x[glist[[k]]],
                                           flist = d_fnc,
                                           useNA = use_na,
                                           w = w[glist[[k]]],
                                           xname = lab, ...)
                        R0 <- dtable_cbind(x = R0, y = tmp,
                                           groups = names(glist)[k])
                        if(P$desc.style == "first") break
                    }
                }
                R1 <- if(is.null(R1)) R0 else dtable_rbind(R1, R0)
            }
        }
        if(P$comp){
            for(g in gvar$variable){ ## g = gvar$variable[1]
                lab <- gvar$label[gvar$variable == g][1]
                x <- if(type %in% c("bnry", "catg")){
                         factor(data[[g]], levels = attr(guide, "levels")[[g]])
                     } else {
                         data[[g]]
                     }
                if(P$comp.style == "overall"){
                    R0 <- apply_flist(x = x, flist = c_fnc, useNA = use_na,
                                      glist = glist, w = w, xname = lab, ...)
                } else {
                    R0 <- NULL
                    for(k in 2:length(glist)){ ## k = 2
                        ref.index <- if(P$comp.style == "across") 1 else k-1
                        tmp <- apply_flist(x = x,
                                           glist = glist[c(ref.index,k)],
                                           flist = c_fnc,
                                           useNA = use_na,
                                           xname = lab, ...)
                        R0 <- dtable_cbind(R0, tmp,
                                           groups = names(glist)[k])
                    }
                }
                R2 <- dtable_rbind(R2, R0)
            }
        }
        R <- dtable_order(if(is.null(R1)  | is.null(R2)){
                              if(!is.null(R1)) R1 else R2
                          } else {
                              dtable_cbind(R1, R2)
                          })
    }
    attr(R, "size") <- nrow(data)
    variables <- guide$variable[guide$type == type]
    attr(R, "cc") <- sum(stats::complete.cases(data[,variables]))
    if(!is.null(w)) attr(R, "weight") <- sum(w)
    if(!is.null(row_id <- attr(guide, "row.id"))){
        ## This currently does not do much
    }
    if(!is.null(unit_id <- attr(guide, "unit.id"))){
        tmp_f <- function(x) length(unique(stats::na.omit(x)))
        attr(R, "units") <- tmp_f(data[[unit_id]])
    }
    if(!is.null(glist)){
        attr(R, "glist_size") <- unlist(lapply(glist, sum, na.rm = TRUE))
        if(!is.null(w)){
            attr(R, "glist_weight") <-
                unlist(lapply(glist, function(x) sum(w[x], na.rm =TRUE)))
        }
        if(!is.null(unit_id)){
            tmp_g <- function(x) tmp_f(data[[unit_id]][x])
            attr(R, "glist_units") <- unlist(lapply(glist, tmp_g))
        }
        tmp_fnc <- function(x, Y = data[,variables,drop = FALSE]) sum(stats::complete.cases(Y[x,]))
        attr(R, "glist_cc") <- unlist(lapply(glist, tmp_fnc))
    }
    attr(R, "dc_param") <- P
    R
}

## function for setting up sane 'comp' and 'desc' defaults
dc_param <- function(desc = NULL, comp = NULL, glist = NULL){
    if(is.null(desc)) desc <- TRUE
    if(is.null(comp)) comp <- if(is.null(glist)) FALSE else TRUE
    if(is.character(desc)){
        if(!desc %in% c("each", "first")){
            stop("if character, desc should be 'each' or 'first'")
        }
        desc.style <- desc
        desc <- TRUE
    } else {
        desc.style <- NA_character_
    }
    if(is.character(comp)){
        if(!comp %in% c("overall", "across", "adjacent")){
            stop("if character, comp should be 'overall', 'across' or 'adjacent'")
        }
        comp.style <- comp
        comp <- TRUE
    } else {
        comp.style <- NA_character_
    }
    if(comp){
        if(is.null(glist)){
            warning("comp set, but no glist?")
            comp <- FALSE
            comp.style <- NA_character_
        } else if(is.na(comp.style)){
            comp.style <- "overall"
        }
    }
    if(desc){
        if(is.null(glist)){
            if(is.na(desc.style)) desc.style <- "first"
        } else {
            if(comp){
                desc.style <- if(comp.style == "overall"){
                                  "each"
                              } else {
                                  "first"
                              }
            } else {
                desc.style <- "each"
            }
        }
    }
    list("desc" = desc,
         "desc.style" = desc.style,
         "comp" = comp,
         "comp.style" = comp.style)
}


###############################################################################

if(FALSE){ ## TESTS, some of which are also in tests


}
