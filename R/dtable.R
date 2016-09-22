##' create descriptive table
##'
##' create a description of variables of a given type in your
##'     data. this functions should have a vignette...
##' @title create descriptive table
##' @param data a \code{data.frame} or such object
##' @param type what kind of variables to you want to describe?
##' @param guide point to guide or get the default
##' @param desc if to decribe variables
##' @param desc.flist list of describers, i.e. describing functions
##' @param comp if, and how to, compare variables (requires a glist)
##' @param comp.flist list of comparers, i.e. comparing functions
##' @param glist grouping list, if wanted
##' @param w weights, if wanted
##' @param useNA how to handle \code{NA} (requires that functions in
##'     flist has this argument)
##' @param ... arguments passed
##' @export
dtable <- function(data, type, guide = NULL,
                   desc = NULL, desc.flist = NULL,
                   comp = NULL, comp.flist = NULL,
                   glist = NULL, w = NULL, useNA = "no", ...){
    if(!type %in% c("real","bnry", "date", "catg", "surv")){
        stop("type not supported")
    }
    if(!useNA %in% c("ifany", "always", "no")){
        message("wrong useNA specification (set to 'no')")
        useNA <- "no"
    }
    P <- dc_param(desc = desc, comp = comp, glist = glist)
    if(is.null(glist)){
        ## if(is.null(comp)) comp <- FALSE
        ## if(comp) message("comparisons require a glist")
        ## comp <- FALSE
    } else {
        if(is.character(glist)) glist <- make_glist(x = glist, ref = data)
        if(!is.list(glist)){
            glist <- tryCatch(make_glist(glist, ref = data[[1]]),
                              error = function(e)
                                  stop("cannot make glist from this glist-argument"))
        }
        if(length(glist) == 1) stop("only 1 subgroup defined by glist")
        ## if(length(glist) >  1 & is.null(P$comp)) comp <- TRUE
    }
    if(!is.null(w)){
        if(is.character(w)){
            w = data[[w]]
            if(is.null(w))
                warning("weighting variable does not exist in data")
        }
        if(length(w) != nrow(data))
            stop("bad weighting")
        if(any(is.na(w))){
            warning("weight has NA:s")
        }
    }
    if(!P$desc & !P$comp) return(NULL)
    if(is.null(guide)) guide <- dtable_guide(data = data)
    gvar <- guide[guide$type == type,]
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
        for(g in gvar$variable){ ## g <- gvar$variable[1]
            x <- if(type %in% c("bnry", "catg")){
                factor(data[[g]], levels = attr(guide, "levels")[[g]])
            } else {
                data[[g]]
            }
            R0 <- NULL
            if(is.null(glist)){
                R0 <- apply_flist(x = x, flist = d_fnc, w = w,
                                  useNA = use_na, xname = g)##, ...)
            } else {
                for(k in seq_along(glist)){ ## k = 1
                    tmp <- apply_flist(x = x[glist[[k]]],
                                       flist = d_fnc,
                                       useNA = use_na,
                                       w = w[glist[[k]]],
                                       xname = g)##, ...)
                    R0 <- cbind_dtable(x = R0, y = tmp,
                                       groups = names(glist)[k])
                    if(P$desc.style == "first") break
                }
            }
            R1 <- if(is.null(R1)) R0 else rbind_dtable(R1, R0)
        }
    }
    if(P$comp){
        for(g in gvar$variable){ ## g = gvar$variable[1]
            x <- if(type %in% c("bnry", "catg")){
                factor(data[[g]], levels = attr(guide, "levels")[[g]])
            } else {
                data[[g]]
            }
            if(P$comp.style == "overall"){
                ## R2 <- rbind_dtable(R2,
                ##                    apply_flist(x = x, flist = c_fnc,
                ##                       glist = glist, w = w, xname =
                ##                                                 g))##,...))
                R0 <- apply_flist(x = x, flist = c_fnc, useNA = use_na, ## <----------
                                      glist = glist, w = w, xname = g) ##,...)
            } else {
                R0 <- NULL
                for(k in 2:length(glist)){ ## k = 2
                    ref.index <- if(P$comp.style == "across") 1 else k-1
                    tmp <- apply_flist(x = x, glist = glist[c(ref.index,k)],
                                       flist = c_fnc, useNA = use_na,
                                       xname = g, ...)
                    R0 <- cbind_dtable(R0, tmp,
                                       groups = names(glist)[k])
                }
            }
            R2 <- rbind_dtable(R2, R0)
        }
    }
    R <- meta_order_dtable(if(is.null(R1)  | is.null(R2)){
        if(!is.null(R1)) R1 else R2
    } else {
        cbind_dtable(R1, R2)
    })
    if(!is.null(glist)){
        attr(R, "glist_size") <- unlist(lapply(glist, sum, na.rm = TRUE))
        if(!is.null(w)){
            attr(R, "glist_weight") <-
                unlist(lapply(glist, function(x) sum(w[x], na.rm =TRUE)))
        }
    }
    ## R
    attr(R, "dc_param") <- P
    R
}

if(FALSE){ ## TESTS, some of which are also in tests

    n <- 100
    set.seed(20160216)
    df <- data.frame(
        id = paste0("id", 1001:(1000 + n)),
        r1 = round(rnorm(n, 20, 5)),
        r2 = round(rexp(n, 1/20)),
        c1 = sample(letters[1:5], size = n, replace = TRUE),
        c2 = factor(sample(LETTERS[5:3], size = n, replace = TRUE)),
        b1 = sample(LETTERS[6:7], size = n, replace = TRUE, prob = 2:3),
        b2 = rbinom(n, 1, 0.1),
        b3 = sample(c("No", "Yes"), size = n, replace = TRUE, prob = 1:2),
        b4 = sample(c(TRUE, FALSE), size = n, replace = TRUE),
        d1 = as.Date("2000-01-01") + rpois(n, 365),
        d2 = as.Date(floor(rexp(n, 1/3650)), origin = "1975-01-01"),
        s1 = survival::Surv(time = rnorm(n, 50, 7), event = rbinom(n, 1, 0.1)),
        s2 = survival::Surv(time = rexp(n, 1/40), event = rbinom(n, 1, 0.2)),
        stringsAsFactors = FALSE
    )
    misser <- function(x, m = length(x)){
        p <- floor(runif(1, min = 1, max = m/10))
        x[sample(1:n, size = p, replace = FALSE)] <- NA
        x
    }
    df[2:length(df)] <- lapply(df[2:length(df)], misser)
    df
    (dtb <- dtable_guide(df))
    gl <- make_glist("b1", ref = df)
    gl3 <- list(
        "abacus" = sample(c(T,F), size = n, replace =T),
        "quuz" = sample(c(T,F), size = n, replace =T),
        "k__7" = sample(c(T,F), size = n, replace =T)
    )
    vikt <- rpois(n, 1.5) + 1

    dtable(data = df, type = "real", guide = dtb)
    dtable(data = df, type = "real", guide = dtb, glist = gl)
    dtable(data = df, type = "real", guide = dtb, glist = gl3, comp = F)
    dtable(data = df, type = "real", guide = dtb, glist = gl, comp = FALSE)
    dtable(data = df, type = "real", guide = dtb, glist = gl, desc = F)
    dtable(data = df, type = "real", guide = dtb, glist = gl3, desc = F,
           comp = "across")
    dtable(data = df, type = "real", guide = dtb, glist = gl3, desc = F,
           comp = "adjacent")

    ## dtable(data = df, type = "real", guide = dtb)
    ## dtable(data = df, type = "real", guide = dtb, glist = gl)
    ## dtable(data = df, type = "real", guide = dtb, glist = gl3, comp = F)
    ## dtable(data = df, type = "real", guide = dtb, glist = gl, comp = FALSE)
    ## dtable(data = df, type = "real", guide = dtb, glist = gl, desc = F)

    dtable(data = df, type = "real", guide = dtb, glist = gl, w = vikt)
    dtable(data = df, type = "real", guide = dtb, glist = gl3, w = vikt)

    dtable(data = df, type = "bnry", guide = dtb)
    dtable(data = df, type = "bnry", guide = dtb, glist = gl)
    dtable(data = df, type = "bnry", guide = dtb, glist = gl3, comp = F)
    dtable(data = df, type = "bnry", guide = dtb, glist = gl, desc =F)

    dtable(data = df, type = "bnry", guide = dtb, glist = gl, w = vikt)
    dtable(data = df, type = "bnry", guide = dtb, glist = gl3, w = vikt)

    dtable(data = df, type = "catg", guide = dtb)
    dtable(data = df, type = "catg", guide = dtb, useNA = "no")

    dtable(data = df, type = "catg", guide = dtb, glist = gl)
    dtable(data = df, type = "catg", guide = dtb, glist = gl3, comp = F)
    dtable(data = df, type = "catg", guide = dtb, glist = gl, desc = F)

    dtable(data = df, type = "catg", guide = dtb, glist = gl, w = vikt)

    dtable(data = df, type = "date", guide = dtb)
    dtable(data = df, type = "date", guide = dtb, glist = gl)
    dtable(data = df, type = "date", guide = dtb, glist = gl3, comp = F)

    dtable(data = df, type = "surv", guide = dtb)
    dtable(data = df, type = "surv", guide = dtb, glist = gl)
    dtable(data = df, type = "surv", guide = dtb, glist = gl3,
           comp = "across")


    fix <- function(s){
        x <- eval(parse(text=s))
        text <- capture.output(dput(x))
        cat(paste0("expect_equal(\n    ",
               s,
               ",\n    ",
               gsub("),", "),\n             ", paste0(text, collapse=""), fixed = TRUE),
               "\n)\n"))
    }


{  ## run and copy-paste this segment into tests
    fix("dtable(data = df, type = 'real', guide = dtb)")
    fix("dtable(data = df, type = 'real', guide = dtb, glist = gl)")
    fix("dtable(data = df, type = 'real', guide = dtb, glist = gl3, comp = F)")
    fix("dtable(data = df, type = 'real', guide = dtb, glist = gl, comp = FALSE)")
    fix("dtable(data = df, type = 'real', guide = dtb, glist = gl, desc = F)")
    fix("dtable(data = df, type = 'bnry', guide = dtb)")
    fix("dtable(data = df, type = 'bnry', guide = dtb, glist = gl)")
    fix("dtable(data = df, type = 'bnry', guide = dtb, glist = gl3, comp = F)")
    fix("dtable(data = df, type = 'bnry', guide = dtb, glist = gl, desc =F)")
    fix("dtable(data = df, type = 'catg', guide = dtb)")
    fix("dtable(data = df, type = 'catg', guide = dtb, useNA = 'no')")
    fix("dtable(data = df, type = 'catg', guide = dtb, glist = gl)")
    fix("dtable(data = df, type = 'catg', guide = dtb, glist = gl3, comp = F)")
    fix("dtable(data = df, type = 'catg', guide = dtb, glist = gl, desc = F)")
    fix("dtable(data = df, type = 'date', guide = dtb)")
    fix("dtable(data = df, type = 'date', guide = dtb, glist = gl)")
    fix("dtable(data = df, type = 'date', guide = dtb, glist = gl3, comp = F)")
}

    set.seed(20080608)
    n <- 100
    tr = rep(LETTERS[1:2], each = n)
    ev = rbinom(2*n, 1, ifelse(tr == "A", 0.2, 0.1))
    meas = rnorm(2*n, ifelse(tr=="A", 10, 8), ifelse(tr=="A", 3, 2))
    vikt = rpois(2*n, lambda = ifelse(tr=="A", 2, 1))
    df <- data.frame(tr=tr,ev=ev,vikt=vikt,meas=meas)
    dtg <- dtable_guide(df, elim.set = c("vikt", "tr"))
    rm(tr,ev,meas,vikt)

    (dtb <- dtable(df, "bnry", dtg, glist = "tr"))
    attributes(dtb)
    p1 <- dtb[1,4]; p2 <- dtb[1,7]
    (o1 <- p1/(1-p1))
    (o2 <- p2/(1-p2))
    o1/o2

    (dtbw <- dtable(data = df, type = "bnry", guide = dtg, glist = "tr", w = "vikt"))
    attributes(dtbw)
    p1 <- dtbw[1,4]; p2 <- dtbw[1,7]
    (o1 <- p1/(1-p1))
    (o2 <- p2/(1-p2))
    o1/o2

}
