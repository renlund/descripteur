
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## *                 describing functions                          * ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## +-----------------------------------------+ ##
## | describing functions for real variables | ##
## +-----------------------------------------+ ##

d.missing <- function(x, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    ## if(useNA) sum(w[is.na(x)]) else NULL "" ## bad idea, maybe
    sum(w[is.na(x)])
}
d.mean <- function(x, w = NULL, ...){
    if(is.null(w)){
        mean(x, na.rm = TRUE)
    } else {
        weighted.mean(x, w = w, na.rm = TRUE)
    }
}
d.sd <- function(x, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    y <- x*w*length(w)/sum(w)
    sd(y, na.rm = TRUE)
}
d.median <- function(x, ...) median(x, na.rm = TRUE)
d.IQR <- function(x, ...) IQR(x, na.rm = TRUE)

dr_def <- list(
    "missing" = d.missing,
    "median" = d.median,
    "IQR" = d.IQR
)
attr(dr_def, "dtable") <- rep("desc", 3)

dr_sym <- list(
    "missing" = d.missing,
    "mean" = d.mean,
    "sd" = d.sd
)
attr(dr_sym, "dtable") <- rep("desc", 3)

## +-----------------------------------------+ ##
## | describing functions for bnry variables | ##
## +-----------------------------------------+ ##

d.bnryify <- function(x){
    rev = FALSE ## let this be a setting in opts_desc ??
    if(is.factor(x)){
        if(length(levels(x)) == 2){
            if(rev) return(factor(x, levels = rev(levels(x))))
            if(!rev) return(x)
        } else {
            stop("trying to give binry stats on a non-binary variable")
        }
    }
    lev <- na.omit(unique(x))
    if(rev) lev <- rev(lev)
    if(length(lev) != 2){
        stop("trying to give binry stats on a non-binary variable")
    }
    factor(x, levels = lev)
}
d.level <- function(x, ...){
    y <- d.bnryify(x)
    levels(y)[2]
}
d.p <- function(x, w = NULL, ...){
    y <- d.bnryify(x)
    if(is.null(w)) w <- rep(1, length(x))
    z <- ifelse(y==d.level(y), 1L, 0L)
    ##sum(y==d.level(y), na.rm = TRUE) / length(na.omit(y))
    mean(w*z, na.rm = TRUE)
}
d.odds <- function(x, w = NULL, ...){
    y <- d.bnryify(x)
    if(is.null(w)) w <- rep(1, length(x))
    tryCatch(d.p(y, w = w)/(1-d.p(y, w = w)), error = function(e) NA)
}

db_def <- list(
    "level" = d.level,
    "missing" = d.missing,
    "p" = d.p,
    "odds" = d.odds
)
attr(db_def, "dtable") <- c("meta", "desc", "desc", "desc")

## +-----------------------------------------+ ##
## | describing functions for date variables | ##
## +-----------------------------------------+ ##

d.min = function(x, ...) min(x, na.rm = TRUE)
d.max = function(x, ...) max(x, na.rm = TRUE)

dd_def <- list(
    "missing" = d.missing,
    "min" = d.min,
    "max" = d.max
)
attr(dd_def, "dtable") <- rep("desc", 3)

## +-----------------------------------------+ ##
## | describing functions for catg variables | ##
## +-----------------------------------------+ ##

.missing_char <- "missing"

## -- helper functions --
d.catgify <- function(x){
    if(is.factor(x)){
        x
    } else {
        factor(x)
    }
}
d.weighted_percent <- function(x, w = NULL){
    if(is.null(w)) w <- rep(1L, length(x))
    mm <- model.matrix(~x)
    mm[,1] <- ifelse(rowSums(mm[,-1]) == 0, 1, 0)
    as.numeric(colSums(w[!is.na(x)]*mm, na.rm = TRUE) / sum(w[!is.na(x)]))
}

## -- list functions --
d.levels <- function(x, useNA = TRUE, w = NULL, ...){
    y <- d.catgify(x)
    if(useNA) c(levels(y), .missing_char) else levels(y)
}
d.percent <- function(x, useNA = TRUE, w = NULL){
    y <- d.catgify(x)
    t <- as.numeric(table(y, useNA = "always")) / length(y)
    r <- if(useNA) t else t[-length(t)]
    100*r
}
d.tab <- function(x, useNA = TRUE, w = NULL){
    y <- d.catgify(x)
    ## t <- as.numeric(table(y)) / length(na.omit(y))
    t <- d.weighted_percent(x = y, w = w)
    if(useNA) c(t, NA) else t
}

dc_def <- list(
    "levels" = d.levels,
    "percent" = d.percent,
    "p" = d.tab
)
attr(dc_def, "dtable") <- c("meta", "desc", "desc")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## *                 comparing functions                           * ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## +----------------------------------------+ ##
## | comparing functions for real variables | ##
## +----------------------------------------+ ##

d.shift <- function(x, glist, ...){
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]

    (median(x1, na.rm = TRUE) - median(x2, na.rm = TRUE)) /
        IQR(x1, na.rm = TRUE)
}

cr_def <- list(
    "shift" = d.shift
)
attr(cr_def, "dtable") <- "comp"

d.std.diff <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    x1 <- x[glist[[1]]]*w*length(w)/sum(w)
    x2 <- x[glist[[2]]]*w*length(w)/sum(w)
    (mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE)) /
        sd(x1, na.rm = TRUE)
}

cr_sym <- list(
    "std.diff" = d.std.diff
)
attr(cr_sym, "dtable") <- "comp"

## +----------------------------------------+ ##
## | comparing functions for bnry variables | ##
## +----------------------------------------+ ##

d.RR <- function(x, glist, w = NULL, ...){
    d.p(x[glist[[1]]], w = w) / d.p(x[glist[[1]]], w = w)
}
d.OR <- function(x, glist, w = NULL, ...){
    d.odds(x[glist[[1]]], w = w) / d.odds(x[glist[[2]]], w = w)
}

cb_def <- list(
    ## "RR" = d.RR,
    "OR" = d.OR
)
attr(cb_def, "dtable") <- "comp"

## +----------------------------------------+ ##
## | comparing functions for date variables | ##
## +----------------------------------------+ ##

d.overlap <- function(x, glist, ...){
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    a <- max(min(x1, na.rm = TRUE), min(x2, na.rm = TRUE))
    b <- min(max(x1, na.rm = TRUE), max(x2, na.rm = TRUE))
    sum(x >= a & x <= b, na.rm = TRUE) ## / sum(!is.na(x))
}

cd_def <- list(
    "n.overlap" = d.overlap
)
attr(cd_def, "dtable") <- "comp"

## +----------------------------------------+ ##
## | comparing functions for catg variables | ##
## +----------------------------------------+ ##

d.cc_diff <- function(x, glist, useNA = FALSE, w = NULL, ...){
    p1 <- d.tab(x[glist[[1]]], w = w)
    p2 <- d.tab(x[glist[[2]]], w = w)
    dp <- p1 - p2
    if(useNA) c(dp, NA) else dp
}

d.cc_OR <- function(x, glist, useNA = FALSE, w = NULL, ...){
    p1 <- d.tab(x[glist[[1]]], w = w)
    p2 <- d.tab(x[glist[[2]]], w = w)
    OR <- (p1 / (1-p1)) / (p2 / (1-p2))
    if(useNA) c(OR, NA) else OR
}

cc_def <- list(
    "levels" = d.levels,
    "diff" = d.cc_diff,
    "OR" = d.cc_OR
)
attr(cc_def, "dtable") <- c("meta", "comp", "comp")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## *                     other functions                           * ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## +--------------------------------------+ ##
## | make a grouping list from a variable | ##
## +--------------------------------------+ ##

make_glist <- function(x, ref = NULL){
    if(!is.null(ref)){
        if(is.data.frame(ref)){
            if(is.character(x)){
                x <- ref[[x]]
            } else {
                if(length(x) != nrow(ref)) stop("nah1")
            }
        } else {
            if(length(x) != length(ref)) stop("nah2")
        }
    }
    y <- as.factor(x)
    if(length(levels)>100) stop("nah3")
    g <- as.list(NULL)
    for(k in levels(y)){
        g[[k]] <- y == k
    }
    g
}


## +--------------------------------------+ ##
## | function to apply the function lists | ##
## +--------------------------------------+ ##

flist_attr_to_dtable <- function(flist){
    n <- length(flist)
    A <- attr(flist, "dtable")
    r <- c("meta", rep("desc", n))
    if(is.null(A)){
        r
    } else {
        if(length(A) != n){
            message("weird dtable attributes")
            r
        }
        c("meta", A)
    }
}

apply_flist <- function(x, flist, ..., xname = NULL){
    r <- as.list(NULL)
    r$variable <- if(!is.null(xname)){
        xname
    } else {
        paste0(as.character(substitute(x)), collapse = "")
    }
    for(k in names(flist)){ ## k = names(flist)[1]
        r[[k]] <- if(is.function(fnc <- flist[[k]])){
            fnc(x, ...)
        } else {
            tryCatch(as.character(flist[[k]]), error = function(e) "*anomalie*")
        }
    }
    r <- as.data.frame(r)
    class(r) <- c("dtable", "data.frame")
    attr(r, "dtable") <- flist_attr_to_dtable(flist)
    r
}

meta_order_dtable <- function(x){
    a <- attr(x, "dtable")
    i <- c(which(a == "meta"), which(a != "meta"))
    r <- x[,i]
    attr(r, "dtable") <- a[i]
    names(r) <- names(x)[i]
    r
}

prune_dtable <- function(x, rm = NULL){
    if(is.null(rm)) return(x)
    d <- dattr(x)
    if(is.character(rm)){
        rm <- which(names(x) == rm)
    }
    r <- x[,-rm]
    names(r) <- names(x)[-rm]
    dattr(r) <- dattr(x)[-rm]
    r
}


if(FALSE){

    x <- data.frame(variable = c("foo", "bar"),
                    mush = c("baz", "quuz"),
                    gee = c(1,2))
    attr(x, "dtable")  <- c("meta", "desc", "meta")
    (r <- meta_order_dtable(x))
    attr(r, "dtable")

}

dattr <- function(x) attr(x, "dtable")
"dattr<-" <- function(x, value) "attr<-"(x, "dtable", value)

rbind_dtable <- function(x, y){
    r <- rbind(x, y)
    class(r) <- c("dtable", "data.frame")
    r
}

cbind_dtable <- function(x, y, groups = NULL){
    x <- meta_order_dtable(x)
    y <- meta_order_dtable(y)
    a <- attr(x, "dtable")
    b <- attr(y, "dtable")
    if(!is.null(groups)){
        if(length(groups) != 2) groups <- c("", groups[1])
        a <- ifelse(a == "meta", a, paste0(a,
                    if(groups == "") "" else ":", groups[1]))
        b <- ifelse(b == "meta", b, paste0(b, ":", groups[2]))
    }
    if(is.null(x)){
        dattr(y) <- b
        return(y)
    }
    n_a <- names(x)[a == "meta"]
    n_b <- names(y)[b == "meta"]
    ut <- n_b[n_b %in% n_a]
    if(all(x$variable == y$variable)){
        y_mod <- subset(y, TRUE,
                        select = setdiff(names(y), ut))
        r <- cbind(x, y_mod)
    } else {
        message("why is 'variable' off? I'll try to fix it")
        ut2 <- setdiff(ut, "variable")
        y_mod <- subset(y, TRUE,
                        select = setdiff(names(y), ut2))
        r <- merge(x, y_mod, by = "variable")
    }
    attr(r, "dtable") <- c(a, na.omit(ifelse(names(y) %in% n_b, NA, b)))
    r
}

if(FALSE){

    x <- data.frame(variable = c("foo", "bar"),
                    mush = c("baz", "quuz"),
                    gee = c(1,2))
    attr(x, "dtable")  <- c("meta", "meta", "desc")
    y <- data.frame(variable = c("foo", "bar"),
                    gee = c(3,4),
                    mush = c("baz", "quuz"))
    attr(y, "dtable")  <- c("meta", "desc", "meta")
    (r <- cbind_dtable(x, y, groups = c("hoy", "jupp")))
    attr(r, "dtable")

    z <- data.frame(gee = c(5,6),
                    variable = c("foo", "bar"),
                    mush = c("baz", "quuz"))
    attr(z, "dtable")  <- c("desc", "meta", "meta")
    (r2 <- cbind_dtable(x = r, y = z, groups = c("bazooka")))
    attr(r2, "dtable")

    u <- data.frame(fruuu = c(7,8),
                    variable = c("foo", "bar"))
    attr(u, "dtable")  <- c("comp", "meta")
    (r3 <- cbind_dtable(x = r2, y = u))
    attr(r3, "dtable")

    (x <- apply_flist(1:10, flist = dr_def))
    attr(x, "dtable")
    (y <- apply_flist(2:10, flist = dr_sym))
    attr(y, "dtable")
    (r <- cbind_dtable(x, y))

    tmp1 <- structure(list(variable = structure(1L, .Label = "r1", class = "factor"),
    missing = 15, median = 19, IQR = 5), .Names = c("variable",
"missing", "median", "IQR"), row.names = c(NA, -1L), class = c("dtable",
"data.frame"), dtable = c("meta", "desc", "desc", "desc"))

    tmp2 <- structure(list(variable = structure(1L, .Label = "r1", class = "factor"),
    missing = 16, median = 20.5, IQR = 6), .Names = c("variable",
"missing", "median", "IQR"), row.names = c(NA, -1L), class = c("dtable",
"data.frame"), dtable = c("meta", "desc", "desc", "desc"))

    (tmp <- cbind_dtable(x = tmp1, y = tmp2, groups = letters[1:2]))
    attr(tmp, "dtable")

}


## +--------------------------------------+ ##
## |               TESTS !                | ##
## +--------------------------------------+ ##

if(FALSE){

    xr <- rnorm(10)
    xr[1] <- NA
    apply_flist(x = xr, flist = dr_def)
    apply_flist(x = xr, flist = dr_def, xname = "foO")
    apply_flist(x = xr, flist = dr_sym)
    apply_flist(x = xr, flist = dr_sym, w = (1:10)/10)

    xb <- rbinom(100, 1, 0.3)
    xb[c(3,89)] <- NA
    d.bnryify(xb)
    apply_flist(x = xb, flist = db_def)
    apply_flist(x = xb, flist = db_def, w = ifelse(xb == 1, 2, 1))

    xb <- sample(c(T, F), size = 100, replace = TRUE)
    xb[c(71,18)] <- NA
    d.bnryify(xb)
    apply_flist(x = xb, flist = db_def)

    xb <- sample(c("T", "F"), size = 100, replace = TRUE)
    xb[c(71,18)] <- NA
    d.bnryify(xb)
    apply_flist(x = xb, flist = db_def)

    xb <- factor(xb, levels = c("F", "T"))
    d.bnryify(xb)
    apply_flist(x = xb, flist = db_def)

    xc <- sample(letters[1:3], size = 100, replace = TRUE)
    xc[c(1,6,9,14,67)] <- NA
    apply_flist(x = xc, flist = dc_def)
    xc <- factor(xc, levels = letters[3:1])
    apply_flist(x = xc, flist = dc_def)
    apply_flist(x = xc, flist = dc_def, useNA = T)
    apply_flist(x = xc, flist = dc_def, w = 100:1)
    gl <- list("a" = rep(c(T, F),100), "b" = rep(c(F, T),100))
    apply_flist(x = xc, flist = cc_def, glist = gl)


    xd <- as.Date("2010-01-01") + round(rnorm(100, 0, 100))
    apply_flist(x = xd, flist = dd_def)
    apply_flist(x = xd[gl[[1]]], flist = dd_def)
    apply_flist(x = xd[gl[[2]]], flist = dd_def)
    apply_flist(x = xd, flist = cd_def, glist = gl)

    n <- 6
    f <- sample(letters[1:2], n, T)
    u <- rbinom(n, 1, 0.5)
    make_glist(f)
    make_glist(f, ref = u)
    tryCatch(make_glist(f, ref = c(u, 1)),
             error = function(e) "error, like expected")
    df <- data.frame(f, u)
    make_glist("f", ref = df)
    make_glist("u", ref = df)

    xr <- rnorm(n)
    apply_flist(xr, flist = cr_def, glist = make_glist(f))
    apply_flist(xr, flist = cr_sym, glist = make_glist(f))
}

## +--------------------------------------+ ##
## |          main function               | ##
## +--------------------------------------+ ##

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
                    ## R0 <- if(is.null(R0)){
                    ##     tmp
                    ## } else {
                    ##     cbind_dtable(x = R0, y = tmp, groups = names(glist)[k])
                    ## }
                    R0 <- cbind_dtable(x = R0, y = tmp, groups = names(glist)[k])
                }
            }
            R1 <- if(is.null(R1)) R0 else rbind_dtable(R1, R0)
        }
    }
    if(comp){
        for(g in gvar$variable){
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


## +--------------------------------------+ ##
## |               TESTS !                | ##
## +--------------------------------------+ ##


if(FALSE){

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

    (tmp <- dtable(data = df, type = "real", guide = dtb))
    attr(tmp, "dtable")
    (tmp <- dtable(data = df, type = "real", guide = dtb, useNA = "no"))
    attr(tmp, "dtable")
    (tmp <- dtable(data = df, type = "real", guide = dtb, glist = gl))
    attr(tmp, "dtable")
    (tmp <- dtable(data = df, type = "real", guide = dtb, glist = gl, comp = FALSE))
    attr(tmp, "dtable")
    (tmp <- dtable(data = df, type = "real", guide = dtb, glist = gl,
           desc = FALSE, comp = TRUE))
    attr(tmp, "dtable")

    (tmp <- dtable(data = df, type = "bnry", guide = dtb))
    attr(tmp, "dtable")
    (tmp <- dtable(data = df, type = "bnry", guide = dtb, useNA = "no"))
    attr(tmp, "dtable")
    (tmp <- dtable(data = df, type = "bnry", guide = dtb, glist = gl))
    attr(tmp, "dtable")

    (tmp <- dtable(data = df, type = "catg", guide = dtb))
    attr(tmp, "dtable")
    (tmp <- dtable(data = df, type = "catg", guide = dtb, useNA = "no"))
    attr(tmp, "dtable")
    (tmp <- dtable(data = df, type = "catg", guide = dtb, glist = gl))
    attr(tmp, "dtable")

    (tmp <- dtable(data = df, type = "date", guide = dtb))
    attr(tmp, "dtable")
    (tmp <- dtable(data = df, type = "date", guide = dtb, useNA = "no"))
    attr(tmp, "dtable")
    (tmp <- dtable(data = df, type = "date", guide = dtb, glist = gl))
    attr(tmp, "dtable")


}
