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
                      c('dt' = list(dt), format_fixer(format.param)))
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
        r <- rle(d2)
        if(all(d2 == "")){
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
    if(is.null(sep <- x$sep)) sep <- ". "
    if(is.null(rm.if.all <- x$rm.if.all)) rm.if.all <- FALSE
    vector <- FALSE
    list(perc = perc, perc.sign = perc.sign, lessthan = lessthan,
         attr = attr, sep = sep, vector = FALSE)
}

## - # this sets up the format defaults for dtable_latex
format_fixer <- function(x = as.list(NULL)){
    if(is.null(b    <- x$b))    b <- 1 ## boundary
    ## for numbers all > boundary
    if(is.null(bh   <- x$bh))   bh <- 1 ## digits arguments for hfnc
    if(is.null(hfnc <- x$hfnc)) hfnc <- base::round ## format fnc
    ## for numbers all < boundary
    if(is.null(bl   <- x$bl))   bl <- 2 ## digits argument for lfnc
    if(is.null(lfnc <- x$lfnc)) lfnc <- base::signif ## format fnc
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

## format_fixer2 <- function(x = as.list(NULL)){
##     if(is.null(b    <- x$b))    b <- 1 ## boundary
##     ## for numbers all > boundary
##     if(is.null(dh   <- x$dh))   dh <- 1 ## digits arguments for h-values
##     ## for numbers all < boundary
##     if(is.null(dl   <- x$dl))   dl <- 2 ## digits argument for l-values
##     if(is.null(dp   <- x$dp))   dp <- 2 ## digits argument for p-values
##     if(is.null(dr   <- x$dr))   dr <- 2 ## digits argument for r-values
##     if(is.null(p_b  <- x$p_b))  p_b <- 0.0001 ## threshold for p-values
##     if(is.null(peq0 <- x$peq0)) peq0 <- TRUE ## can p be zero?
##     if(is.null(tmax <- x$tmax)) tmax <- 30 ## max chars to print for text
##     if(is.null(repus <- x$repus)) repus <- TRUE ## replace '_' with '\\_'?
##     if(is.null(repwith <- x$repwith)) repwith <- "\\_"
##     list(b = b, dh = dh, dl = dl, dr = dr,
##          p_b = p_b, peq0 = peq0, tmax = tmax,
##          repus = repus, repwith = repwith)
## }

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
            ns <- rle(grey_var)$lengths
            rep(rep(color_vec, length.out = length(ns)), ns)
        }
    } else {
        if(length(grey) == nrow(x)){
            ns <- rle(grey)$lengths
            rep(rep(color_vec, length.out = length(ns)), ns)
        } else {
            warning(paste0("argument passed to 'grey' not equal in ",
                           "length to the rows of object"))
            NULL
        }
    }
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
dtable_format <- function(dt, b = 1,
                          bh = 1, hfnc = base::round,
                          bl = 2, lfnc = base::signif,
                          br = 2, rfnc = base::signif,
                          p_b = 0.0001, peq0 = TRUE,
                          tmax = 30, repus = TRUE,
                          repwith = "\\_"){
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
    p <- if(!is.null(p0)) which(p0) else NULL
    il <- num[which(l>b)]
    R[il] <- lapply(R[il], hfnc, digits = bh)
    ih <- setdiff(num[which(h<=b)], p) ## num[which(h<=b)]
    ## not_zero <- function(x) ifelse(x < p_b & x > 0, paste0("<", p_b), x)
    ## maybe_zero <- function(x) ifelse(x < p_b & x >= 0, paste0("<", p_b), x)
    ## zero <- function(x, not) if(not) not_zero(x) else maybe_zero(x)
    ## R[num[p]] <- lapply(R[num[p]], zero, not = peq0)
    small.fixer <- function(x, digits, peq0, reps = TRUE){
        izero  <- which(x == 0)
        ismall <- which(x < p_b)
        y <- sprintf(paste0("%#.", digits, "g"), x)
        if(reps) y[ismall] <- paste0("<", p_b)
        if(peq0) y[izero] <- "0"
        y
    }
    ## R[ih] <- lapply(R[ih], lfnc, digits = bl)
    R[ih] <- lapply(R[ih], small.fixer, digits = bl,
                    peq0 = FALSE, reps = FALSE)
    R[num[p]] <- lapply(R[num[p]], small.fixer, digits = bl,
                        peq0 = peq0, reps = TRUE)
    i_rest <- setdiff(num, c(il, ih, p))
    R[i_rest] <- lapply(R[i_rest], rfnc, br)
    R[i_rest] <- lapply(R[i_rest], format, drop0trailing = TRUE)
    ## format character
    chr <- which(classy == "character")
    foo <- function(x){
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


## dtable_format2 <- function(dt, b = 1, dh = 1, dl = 2, dr = 2, dp = 2,
##                            p_b = 0.0001, peq0 = TRUE, tmax = 30,
##                            repus = TRUE, repwith = "\\_"){
##     ## format numeric part
##     n <- ncol(dt)
##     R <- as.data.frame(dt)
##     classy <- unlist(lapply(R, function(x) class(x)[1]))
##     datum <- which(classy %in% c("Date", "POSIXlt", "POSIXct"))
##     R[datum] <- lapply(R[datum], as.character)
##     num <- which(classy == "numeric")
##     l <- unlist(lapply(R[num], function(x) min(abs(x[is.finite(x)]),
##                                                na.rm = TRUE)))
##     h <- unlist(lapply(R[num], function(x) max(abs(x[is.finite(x)]),
##                                                na.rm = TRUE)))
##     p0 <- unlist(lapply(R[num],
##                         function(x) min(x[is.finite(x)], na.rm = TRUE) >= 0 &
##                                     max(x[is.finite(x)], na.rm = TRUE) <= 1))
##     p <- if(!is.null(p0)) which(p0) else NULL
##     il <- num[which(l>b)]
##     hfnc <- function(x, digits) sprintf(paste0("%#.", digits, "f"), x)
##     R[il] <- lapply(R[il], hfnc, digits = dh)
##     ih <- setdiff(num[which(h<=b)], p)
##     lfnc <- function(x, digits) sprintf(paste0("%#.", digits, "g"), x)
##     R[ih] <- lapply(R[ih], lfnc, digits = dl)
##     p.fixer <- function(x, digits){
##         izero  <- which(x == 0)
##         ismall <- which(x < p_b)
##         y <- lfnc(x, digits = digits)
##         y[ismall] <- paste0("<", p_b)
##         if(peq0) y[izero] <- "0"
##         y
##     }
##     R[num[p]] <- lapply(R[num[p]], p.fixer, digits = dp)
##     i_rest <- setdiff(num, c(il, ih, p))
##     R[i_rest] <- lapply(R[i_rest], lfnc, digits = dr)
##     ## format character
##     chr <- which(classy %in% c("character", "factor"))
##     foo <- function(x){
##         y <- as.character(x)
##         ifelse(nchar(y)>tmax + 2,
##                paste0(substring(y, 1, tmax), "..."),
##                y)
##     }
##     R[chr] <- lapply(R[chr], foo)
##     if(repus){
##         bar <- function(x) gsub("_", repwith, x, fixed = TRUE)
##         R[chr] <- lapply(R[chr], bar)
##     }
##     attributes(R) <- attributes(dt)
##     R
## }

if(FALSE){

    df <- data.frame(
        p = c(0,  0.1, 0.99,  0.029, 0.0000345),
        h = c(1.01,  2.1, 130,   3.456, 1000),
        l = c(0, -.21, 0.1,  -.99,   -.000456),
        m = c(0, -2.1, 0.1,   1.3,   456),
        d = as.Date("2012-01-01") + floor(seq(0,10000,len = 5)),
        t = proh::mumbo(5, 4)
    )

    dtable_format(df, peq0 = TRUE)
    dtable_format(df, peq0 = FALSE)

    ## dt = df
    ## b = 1
    ## hfnc = base::round
    ## lfnc = base::signif
    ## rfnc = base::round
    ## bh = 1
    ## bl = 2
    ## br = 2
    ## bp = 2
    ## p_b = 0.0001
    ## peq0 = TRUE
    ## tmax = 30
    ## repus = TRUE
    ## repwith = "\\_"

    dtable_format2(df, peq0 = TRUE)
    dtable_format2(df, peq0 = FALSE)


    ## test <- c(100.1, 10, 1.123, 0, 0.1, 0.11, 0.0001234)

    sprintf("%.2g", test)
    sprintf("%#.2g", test)
    sprintf("%#5.2g", test)

    sprintf("%.2f", test)
    sprintf("%#.2f", test)
    sprintf("%#5.2f", test)

    sprintf("%.2g", df$l)
    sprintf("%#.2g", df$l)
    sprintf("%#5.2g", df$l)

    sprintf("%.2f", df$l)
    sprintf("%#.2f", df$l)
    sprintf("%#5.2f", df$l)

    sprintf("%#.2e", df$l)
    sprintf("%.2e", df$l)

}
