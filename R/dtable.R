##' create descriptive table
##'
##' create a description of variables of a given type in your
##'     data. this functions should have a vignette...
##' @title create descriptive table
##' @param data a \code{data.frame} or such object
##' @param type what kind of variables to you want to describe?
##' @param guide point to guide or get the default
##' @param desc want to decribe variables?
##' @param comp want to apply a flist of comparers? (requires a glist)
##' @param glist grouping list, if wanted
##' @param useNA how to handle \code{NA} (requires that functions in
##'     flist has this argument)
##' @param ... arguments passed
##' @export

dtable <- function(data, type, guide = NULL, desc = TRUE, comp = NULL,
                   glist = NULL, useNA = "ifany", ...){
    if(!type %in% c("real","bnry", "date","catg")){
        stop("type not supported")
    }
    if(!useNA %in% c("ifany", "always", "no")){
        message("wrong useNA specification")
        useNA <- "ifany"
    }
    if(is.null(glist)){
        if(is.null(comp)) comp <- FALSE
        if(comp) message("comparisons require a glist")
        comp <- FALSE
    } else {
        if(is.character(glist)) glist <- make_glist(x = glist, ref = data)
        if(!is.list(glist)){
            glist <- tryCatch(make_glist(glist, ref = data[[1]]),
                              error = function(e)
                                  stop("cannot make glist from this glist-argument"))
        }
        if(length(glist) == 1) stop("only 1 subgroup defined by glist")
        if(length(glist) >  1 & is.null(comp)) comp <- TRUE
    }
    if(!desc & !comp) return(NULL)
    if(is.null(guide)) guide <- dtable_guide(data = data)
    gvar <- guide[guide$type == type,]
    d_fnc <- opts_desc$get(paste0("describe_", type))
    c_fnc <- opts_desc$get(paste0("compare_", type))
    R1 <- NULL
    R2 <- NULL
    if(desc){
        has_na <- any(gvar$has_missing)
        use_na <- if(useNA != "ifany") useNA == "always" else has_na
        for(g in gvar$variable){ ## g <- gvar$variable[1]
            x <- if(type %in% c("bnry", "catg")){
                factor(data[[g]], levels = attr(guide, "levels")[[g]])
            } else {
                data[[g]]
            }
            R0 <- NULL
            if(is.null(glist)){
                R0 <- apply_flist(x = x, flist = d_fnc,
                                  xname = g, useNA = use_na, ...)
            } else {
                for(k in seq_along(glist)){ ## k = 2
                    tmp <- apply_flist(x = x[glist[[k]]],
                                            flist = d_fnc, xname = g, ...)
                    R0 <- cbind_dtable(x = R0, y = tmp, groups = names(glist)[k])
                }
            }
            R1 <- if(is.null(R1)) R0 else rbind_dtable(R1, R0)
        }
    }
    if(comp){
        for(g in gvar$variable){ ## g <- gvar$variable[1]
            x <- if(type %in% c("bnry", "catg")){
                factor(data[[g]], levels = attr(guide, "levels")[[g]])
            } else {
                data[[g]]
            }
            R2 <- rbind_dtable(R2,
                               apply_flist(x = x, flist = c_fnc,
                                    glist = glist, xname = g, ...))
        }
    }
    if(is.null(R1)  | is.null(R2)){
        if(!is.null(R1)) R1 else R2
    } else {
        cbind_dtable(R1, R2)
    }
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
        stringsAsFactors = FALSE
    )
    misser <- function(x, m = length(x)){
        p <- floor(runif(1, min = 1, max = m/2))
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

    dtable(data = df, type = "real", guide = dtb)
    dtable(data = df, type = "real", guide = dtb, glist = gl)
    dtable(data = df, type = "real", guide = dtb, glist = gl3, comp = F)
    dtable(data = df, type = "real", guide = dtb, glist = gl, comp = FALSE)
    dtable(data = df, type = "real", guide = dtb, glist = gl, desc = F)

    dtable(data = df, type = "bnry", guide = dtb)
    dtable(data = df, type = "bnry", guide = dtb, glist = gl)
    dtable(data = df, type = "bnry", guide = dtb, glist = gl3, comp = F)
    dtable(data = df, type = "bnry", guide = dtb, glist = gl, desc =F)

    dtable(data = df, type = "catg", guide = dtb)
    dtable(data = df, type = "catg", guide = dtb, useNA = "no")
    dtable(data = df, type = "catg", guide = dtb, glist = gl)
    dtable(data = df, type = "catg", guide = dtb, glist = gl3, comp = F)
    dtable(data = df, type = "catg", guide = dtb, glist = gl, desc = F)

    dtable(data = df, type = "date", guide = dtb)
    dtable(data = df, type = "date", guide = dtb, glist = gl)
    dtable(data = df, type = "date", guide = dtb, glist = gl3, comp = F)

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

}
