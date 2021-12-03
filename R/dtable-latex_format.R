##' latexify a dtable
##'
##' process dtable into latex code via \code{Hmisc::latex}
##' @param dt a dtable
##' @param bling shall we compose suitable table bling from the
##'     attributes? (default \code{TRUE})
##' @param bling.param list of parameters sent to \code{attr2text}, if non-empty \code{bling}
##'     will be set to TRUE
##' @param file (default empty string) passed to \code{Hmisc::latex}
##' @param where (default "htb") passed to \code{Hmisc::latex}
##' @param rowname (default \code{NULL}) passed to \code{Hmisc::latex}
##' @param grey should some lines be gray to improve readability? This
##'     requires that '\\usepackage[table]\{xcolor\}' be added to the
##'     preamble. If \code{TRUE} then every other line is grey, but this can
##'     also be specifed as a variable name or a given vector, see the vignette
##'     'describe-data' for details.
##' @param ... passed to \code{Hmisc::latex}
##' @param no.below shortcut to remove the bling that is places below the table
##'     (hence will only do something if \code{bling = TRUE})
##' @param guide a guide to provide labels, minimally a data frame with
##'     variables 'variable' and 'label'
##' @param format use \code{dtable_format}?
##' @param format.param list of parameters to pass to
##'     \code{dtable_format} (if given will set \code{format = TRUE}).
##' @export
dtable_latex <- function(dt, bling = TRUE, bling.param = as.list(NULL),
                         file = "", where = "htb", rowname = NULL,
                         grey = NULL,
                         ...,
                         no.below = FALSE,
                         guide = NULL,
                         format = FALSE,
                         format.param = as.list(NULL)){
    if(length(bling.param) > 0) bling <- TRUE
    if(length(format.param) > 0) format <- TRUE
    ## if(format) dt <- dtable_format(dt, param = format.param)
    if(format){
        dt <- do.call(dtable_format,
                      c('dt' = list(dt), format.param))
    }
    x <- as.data.frame.dtable(dt)
    if("variable" %in% names(x) & !is.null(guide)){
        lab <- stats::setNames(guide$label, guide$variable)
        x$variable <- lab[x$variable]
    }
    rnTC <- get_grey(grey, x)
    if(bling){
        A <- attributes(dt)
        d <- A$dtable
        d1 <- gsub("(meta)|(desc:*)", "", d)
        d2 <- gsub("comp", "Comparison", d1)
        d3 <- gsub("test", "Test", d2)
        r <- rle(d3)
        if(all(d3 == "")){
            r <- NULL ## nullify the cgroup and n.cgroup args of Hmisc::latex
        }
        text <- if(!no.below){
                    paste0("{\\small\\begin{center}\\emph{",
                           do.call(attr2text, c(dt = list(dt),
                                                bling_fixer(bling.param))),
                           "}\\end{center}}")
                } else NULL
        Hmisc::latex(object = x, file = file, where = where,
                     rowname = rowname, cgroup = r$values,
                     rownamesTexCmd = rnTC,
                     n.cgroup = r$lengths, insert.bottom = text, ...)
    } else {
        Hmisc::latex(object = x, file = file, where = where,
                     rowname = rowname, rownamesTexCmd = rnTC, ...)
    }
}

## - # this sets up the bling defaults for dtable_latex
bling_fixer <- function(x = as.list(NULL)){
    if(is.null(perc <- x$perc)) perc <- TRUE
    if(is.null(perc.sign <- x$perc.sign)) perc.sign <- "\\%"
    if(is.null(lessthan <- x$lessthan)) lessthan <- "$<$"
    if(is.null(attr <- x$attr)) attr <- c("size", "cc", "weight",
                                          "units", "info")
    if(is.null(units.name <- x$units.name)) units.name <- "units"
    if(is.null(sep <- x$sep)) sep <- ". "
    if(is.null(rm.if.all <- x$rm.if.all)) rm.if.all <- FALSE
    vector <- FALSE
    list(perc = perc, perc.sign = perc.sign, lessthan = lessthan,
         attr = attr, units.name, sep = sep, vector = FALSE)
}

## - ##' determine sequence of colors
get_grey <- function(grey = NULL, x = NULL){
    color_vec <- c("","rowcolor[gray]{.9}")
    if(is.null(grey)){
        NULL
    } else if(is.logical(grey)){
        if(grey){
            rep(color_vec, length.out = nrow(x))
        } else NULL
    } else if(is.character(grey) & length(grey) == 1){
        grey_var <- x[[grey]]
        if(is.null(grey_var)){
            warning(paste0("string '", grey,
                           "' passed to 'grey' is not",
                           "recognised as a column"))
            NULL
        } else {
            if(class(grey_var) == "factor") grey_var <- as.character(grey_var)
            ns <- rle(grey_var)$lengths
            rep(rep(color_vec, length.out = length(ns)), ns)
        }
    } else {
        if(length(grey) == nrow(x)){
            if(class(grey) == "factor") grey <- as.character(grey)
            ns <- rle(grey)$lengths
            rep(rep(color_vec, length.out = length(ns)), ns)
        } else {
            warning(paste0("argument passed to 'grey' not equal in ",
                           "length to the rows of object"))
            NULL
        }
    }
}

##' numerical formating
##'
##' numerical formating (mainly for \code{dtable_format})
##' @param x vector of numeric values
##' @param dg rounding digits for 'great' (abs>1) numbers
##' @param ds significance digits for 'small' numbers (abs<1)
##' @param maybe.p logical; is x possibly p-values?
##' @param p.bound numeric; if p-values, numbers below this will be '<p.bound'
##' @param miss character string to replace missing values
##' @param some.scientific logical; possibly write some numbers in scientific notation?
##' @param low lower threshold for when to resort to scientific notation
##' @param high upper threshold for when to resort to scientific notation
##' @param ... to be able to tolerate argument spamming
##' @param verbose logical; tell med things?
##' @export
dformat_num <- function(x, dg = 1, ds = 2,
                           maybe.p = TRUE, p.bound = 0.0001,
                           miss = "", some.scientific = TRUE,
                        low = 1e-8, high = 1e8, ..., verbose = TRUE){
    args <- list(...)
    ## START for backwards compatability
    ## --- these are arguments used previously
    tmpf <- function(x) if(!is.null(x) & is.numeric(x)) x else FALSE
    if(tmp <- tmpf(args$bh)) dg <- tmp
    if(tmp <- tmpf(args$bl)) ds <- tmp
    if(tmp <- tmpf(args$p_b)) p.bound <- tmp
    old.args <- c("hfnc", "lfnc", "rfnc", "peq0", "b")
    if(any(names(args) %in% old.args) & verbose){
        w <- paste0("Some format arguments (", paste0(old.args, collapse = ", "),
                    ") are out of use, and some are being recycled (dg, ds, p_b).",
                    "(Set verbose = FALSE to hide this message)")
        message(w)
    }
    if(ds < 1){
        ds <- 1
        if(verbose) message("ds found to be <1, changed to =1")
    }
    if(dg < 0){
        dg <- 0
        if(verbose) message("dg found to be <0, changed to =0")
    }
    ## END
    org.x <- x
    ret <- rep(NA_character_, length(x))
    inf <- is.infinite(x)
    x[is.infinite(x)] <- NA
    absx <- abs(x)
    ## if x is all integer set rounding parameter to 0
    ## if(all(absx == as.integer(absx))) dg <- 0
    if(abs(sum(absx-round(absx), na.rm = TRUE)) < 1e-12) dg <- 0
    ## initially assume we do not have p-values
    is.p <- FALSE
    ## but be willing to change your mind if maybe.p = TRUE
    if(maybe.p){
        a <- min(x, na.rm = TRUE)
        b <- max(x, na.rm = TRUE)
        if(a >= 0 & b <= 1) is.p <- TRUE
    }
    nas <- is.na(x)
    ## index for 'great' numbers
    g <- !nas & absx >= 1
    ## index for 'tiny' numbers
    t <- !nas & absx < 1e-3
    ## index for 'small' numbers
    s <- !nas & !g & !t
    ## deal with these kinds of numbers in different ways
    ret[g] <- if(dg == 0){
                  as.character(x[g])
              } else {
                  sprintf(paste0("%#.", dg, "f"), x[g])
              }
    ret[s] <- sprintf(paste0("%#.", ds, "g"), x[s])
    for(i in seq_along(x[t])){
        u <- format(x[t][i], digits = ds, scientific = FALSE)
        if(grepl(pattern = "\\.0*[1-9]$", x = u)){
            u <- paste0(u, "0")
        }
        ret[t][i] <- u
    }
    if(some.scientific){
        i <- !nas & absx != 0 & (absx >= high | absx <= low)
        ret[i] <- format(x[i], digits = ds, scientific = TRUE)
    }
    if(is.p){
        ret[x < p.bound] <- paste0("<", sprintf("%#.1g", p.bound))
    }
    ret[nas] <- miss
    ret[inf] <- as.character(org.x[inf])
    ret
}

##' text formating
##'
##' text formating (mainly for \code{dtable_format})
##' @param x vector of character values
##' @param tmax maximum number of chars
##' @param rep.key key of replacements, i.e. named vector of the form
##'       'new value' = 'old value'
##' @param add.key logical; add given key.values to default ones?
##' @param ... to be able to tolerate argument spamming
##' @param verbose logical; tell me things?
##' @export
dformat_text <- function(x, tmax = 30,
                         rep.key = NULL, add.key = TRUE,
                         ..., verbose = TRUE){
    args <- list(...)
    key <- c("\\\\_" = "_",
             "\\\\ldots" = "\\.\\.\\.",
             "$\\\\geq$" = ">=",
             "$\\\\leq$" = "<=")
    if(is.null(rep.key)){
        rep.key <- key
    } else if(add.key){
        rep.key <- c(rep.key, key)
    }
    ## START for backwards compatability
    old.args <- c("repus", "repwidth")
    if(any(names(args) %in% old.args) & verbose){
        w <- paste0("Some format arguments (", paste0(old.args, collapse = ", "),
                    ") are out of use.",
                    "(Set verbose = FALSE to hide this message)")
        message(w)
    }
    foo <- function(z){
        ifelse(nchar(z)>tmax + 2,
               paste0(substring(z, 1, tmax), "..."),
               z)
    }
    ret <- unlist(lapply(x, foo))
    for(i in seq_along(rep.key)){
        ret <- gsub(pattern = rep.key[i],
                    replacement = names(rep.key)[i],
                    x = ret)
    }
    ret
}


##' formating
##'
##' formatting of dtables and data.frames
##' @param dt a dtable or such
##' @param ... arguments passed to \code{dformat_num} and \code{dformat_text}
##' @export
dtable_format <- function(dt, ...){
    dots <- list(...) ## dots = as.list(NULL)
    da <- dattr(dt)
    DT <- as.data.frame(dt)
    attr(DT, "dattr") <- NULL
    ## things with dattr 'test' should be formatted as p-values
    pi <- which(da %in% "test")
    classy <- unlist(lapply(DT, class))
    indx <- which(classy %in% c("numeric", "integer"))
    ps <- intersect(pi, indx)
    if(length(ps) > 0){
        pdots <- dots
        pdots[['maybe.p']] <- TRUE
        Arg <- c(list("X" = DT[,ps, drop = FALSE],
                      "FUN" = dformat_num),
                 pdots)
        DT[ps] <- do.call(what = lapply, args = Arg)
        indx <- setdiff(indx, ps)
    }
    Arg <- c(list('X' = DT[, indx, drop = FALSE],
                  'FUN' = dformat_num),
             dots)
    DT[indx] <- do.call(what = lapply, args = Arg)
    ## now everything else can be formatted as text
    Arg <- c(list('X' = DT,
                  'FUN' = dformat_text),
             dots)
    DT[] <- do.call(what = lapply, args = Arg)
    attributes(DT) <- attributes(dt)
    DT
}


## - # this sets up the format defaults for dtable_latex
format_fixer <- function(x = as.list(NULL)){
    if(is.null(b    <- x$b))    b <- 1 ## boundary
    ## for numbers all > boundary
    if(is.null(bh   <- x$bh))   bh <- 1 ## digits arguments for hfnc
    if(is.null(hfnc <- x$hfnc)) hfnc <- base::round ## format fnc
    ## for numbers all < boundary
    if(is.null(bl   <- x$bl))   bl <- 2 ## digits argument for lfnc
    if(is.null(lfnc <- x$lfnc)) lfnc <- NULL ## base::signif ## format fnc
    if(is.null(br   <- x$br))   br <- 2 ## digits argument for rfnc
    if(is.null(rfnc <- x$rfnc)) rfnc <- base::round ## format fnc
    if(is.null(p_b  <- x$p_b))  p_b <- 0.0001 ## threshold for p-values
    if(is.null(peq0 <- x$peq0)) peq0 <- TRUE ## can p be zero?
    if(is.null(tmax <- x$tmax)) tmax <- 30 ## max chars to print for text
    if(is.null(repus <- x$repus)) repus <- TRUE ## replace '_' with '\\_'?
    if(is.null(repwith <- x$repwith)) repwith <- "\\_"
    list(b = b, bh = bh, hfnc = hfnc, bl = bl, lfnc = lfnc, br = br,
         rfnc = rfnc, p_b = p_b, peq0 = peq0, tmax = tmax,
         repus = repus, repwith = repwith)
}

##' format a dtable
##'
##' overall, low-precision formatting of dtable objects - quick and
##'     dirty way of getting something ok (hopefully), fast. could be
##'     developed...
##' @param dt a dtable
##' @param b boundary, if numbers are consistently higher they are handled by
##'     'bh' and 'hfnc', if consistently lower by 'bl' and 'lfnc', and otherwise
##'     by 'br' and 'rfnc'
##' @param bh digits argument for \code{hfnc}
##' @param hfnc format function for above boundary numbers
##' @param bl digits argument for \code{lfnc}
##' @param lfnc format function for below boundary numbers? But current
##'     implementation might override this, I can't figure out what is best here
##' @param br digits argument for \code{rfnc}
##' @param rfnc format function for other numbers
##' @param p_b threshold for how small p values to show. Any variable which is
##'     between 0 and 1 is considered a 'p-value' here, which need not be the
##'     case.
##' @param peq0 even if we abbreviate small 'p-values' should we explicitly put
##'     = "0" if it is equal to zero?
##' @param tmax how many characters to print for a character vector
##' @param repus should we replace "_" with 'repwith' in character variables? (If not
##'     LaTeX might fail.)
##' @param repwith that which to replace underscore with, default "\\_"
##' @export
dtable_format_old <- function(dt, b = 1,
                          bh = 1, hfnc = base::round,
                          bl = 2, lfnc = NULL, ##lfnc = base::signif,
                          br = 2, rfnc = base::signif,
                          p_b = 0.0001, peq0 = TRUE,
                          tmax = 30, repus = TRUE,
                          repwith = "\\_"){
    if(is.null(lfnc)){
        lfnc <- function(x, digits){
            sprintf(paste0("%#.", digits, "g"), x)
        }
    }
    ## format numeric part
    n <- ncol(dt)
    R <- as.data.frame(dt)
    classy <- unlist(lapply(R, function(x) class(x)[1]))
    datum <- which(classy %in% c("Date", "POSIXlt", "POSIXct"))
    R[datum] <- lapply(R[datum], as.character)
    num <- which(classy == "numeric")
    l <- unlist(lapply(R[num], function(x) min(abs(x[is.finite(x)]),
                                               na.rm = TRUE)))
    h <- unlist(lapply(R[num], function(x) max(abs(x[is.finite(x)]),
                                               na.rm = TRUE)))
    p0 <- unlist(lapply(R[num],
                        function(x) min(x[is.finite(x)], na.rm = TRUE) >= 0 &
                                    max(x[is.finite(x)], na.rm = TRUE) <= 1))
    ## p <- if(!is.null(p0)) which(p0) else NULL
    p <- if(!is.null(p0)) num[which(p0)] else NULL
    il <- num[which(l>b)]
    R[il] <- lapply(R[il], hfnc, digits = bh)
    ih <- setdiff(num[which(h<=b)], p) ## num[which(h<=b)]
    ## not_zero <- function(x) ifelse(x < p_b & x > 0, paste0("<", p_b), x)
    ## maybe_zero <- function(x) ifelse(x < p_b & x >= 0, paste0("<", p_b), x)
    ## zero <- function(x, not) if(not) not_zero(x) else maybe_zero(x)
    ## R[num[p]] <- lapply(R[num[p]], zero, not = peq0)
    small.fixer <- function(x, fnc = lfnc, digits, reps = TRUE, peq0=peq0){
        izero  <- which(x == 0)
        ismall <- which(x < p_b)
        isna <- which(is.na(x))
        y <- lfnc(x, digits = digits) ## sprintf(paste0("%#.", digits, "g"), x)
        if(reps) y[ismall] <- paste0("<", p_b)
        if(peq0) y[izero] <- 0
        y[isna] <- NA
        y
    }
    ## R[ih] <- lapply(R[ih], lfnc, digits = bl)
    R[ih] <- lapply(R[ih], FUN = small.fixer, digits = bl,
                    peq0 = FALSE, reps = FALSE)
    R[p] <- lapply(R[p], FUN = small.fixer, digits = bl,
                        peq0 = peq0, reps = TRUE)
    i_rest <- setdiff(num, c(il, ih, p))
    R[i_rest] <- lapply(R[i_rest], FUN = rfnc, digits = br)
    ## R[i_rest] <- lapply(R[i_rest], format, drop0trailing = TRUE)
    ## format character
    ## chr <- which(classy == "character")
    chr <- which(classy %in% c("character", "factor"))
    foo <- function(x){
        if("factor" %in% class(x)) x <- as.character(x)
        ifelse(nchar(x)>tmax + 2,
               paste0(substring(x, 1, tmax), "..."),
               x)
    }
    R[chr] <- lapply(R[chr], foo)
    if(repus){
        bar <- function(x) gsub("_", repwith, x, fixed = TRUE)
        R[chr] <- lapply(R[chr], bar)
    }
    attributes(R) <- attributes(dt)
    R
}
