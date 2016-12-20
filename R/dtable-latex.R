##' latexify a dtable
##'
##' process dtable into latex code via \code{Hmisc::latex}
##' @param dt a dtable
##' @param bling shall we compose suitable table bling from the
##'     attributes? (default \code{TRUE})
##' @param bling.param list of parameters sent to attr2text, if non-empty bling
##'     will be set to TRUE
##' @param file (default empty string) passed to \code{Hmisc::latex}
##' @param where (default "htb") passed to \code{Hmisc::latex}
##' @param rowname (default \code{NULL}) passed to \code{Hmisc::latex}
##' @param ... passed to \code{Hmisc::latex}
##' @param guide a guide to provide labels, minimally a data frame with
##'     variables 'variable' and 'label'
##' @param format use \code{dtable_format}?
##' @param format.param list of parameters to pass to
##'     \code{dtable_format} (if given will set \code{format = TRUE}).
##' @export
dtable_latex <- function(dt, bling = TRUE, bling.param = as.list(NULL),
                         file = "", where = "htb", rowname = NULL,
                         ...,
                         guide = NULL,
                         format = FALSE,
                         format.param = as.list(NULL)){
    if(length(bling.param) > 0) bling <- TRUE
    if(length(format.param) > 0) format <- TRUE
    ## if(format) dt <- dtable_format(dt, param = format.param)
    if(format){
        dt <- do.call(dtable_format,
                      c(dt = list(dt), format_fixer(format.param)))
    }
    x <- as.data.frame.dtable(dt)
    if("variable" %in% names(x) & !is.null(guide)){
        lab <- stats::setNames(guide$label, guide$variable)
        x$variable <- lab[x$variable]
    }
    A <- attributes(dt)
    d <- A$dtable
    d1 <- gsub("(meta)|(desc:*)", "", d)
    d2 <- gsub("comp", "Comparison", d1)
    r <- rle(d2)
    if(all(d2 == "")){
        r <- NULL ## nullify the cgroup and n.cgroup args of Hmisc::latex
    }
    text <- paste0("{\\small\\begin{center}\\emph{",
                   do.call(attr2text, c(dt = list(dt), bling_fixer(bling.param))),
                   "}\\end{center}}")
    if(bling){
        Hmisc::latex(object = x, file = file, where = where,
                     rowname = rowname, cgroup = r$values,
                     n.cgroup = r$lengths, insert.bottom = text, ...)
    } else {
        Hmisc::latex(object = x, file = file, where = where,
                     rowname = rowname, ...)
    }
}

## - # this sets up the bling defaults for dtable_latex
bling_fixer <- function(x = as.list(NULL)){
    if(is.null(perc <- x$perc)) perc <- TRUE
    if(is.null(perc.sign <- x$perc.sign)) perc.sign <- "\\%"
    if(is.null(attr <- x$attr)) attr <- c("size", "cc", "weight", "units", "info")
    if(is.null(sep <- x$sep)) sep <- ". "
    if(is.null(rm.if.all <- x$rm.if.all)) rm.if.all <- FALSE
    vector <- FALSE
    list(perc = perc, perc.sign = perc.sign, attr = attr, sep = sep, vector = FALSE)
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
    list(b = b, bh = bh, hfnc = hfnc, bl = bl, lfnc = lfnc, p_b,
         peq0 = peq0, tmax = tmax, repus = repus)
}

##' format a dtable
##'
##' overall, low-precision formatting of dtable objects - quick and
##'     dirty way of getting something ok (hopefully), fast. could be
##'     developed... its weird argument structure (all parameters
##'     gathered in a list) is due to it being
##'     considered most useful when called from other functions.
##' @param dt a dtable
##' @param b boundary, if numbers are consistenly higher they are handled by
##'     'bh' and 'hfnc', if consistently lower by 'bl' and 'lfnc', and otherwise
##'     by 'br' and 'rfnc'
##' @param bh digits argument for \code{hfnc}
##' @param hfnc format function for above boundary numbers
##' @param bl digits argument for \code{lfnc}
##' @param lfnc format function for below boundary numbers
##' @param br digits argument for \code{rfnc}
##' @param rfnc format function for other numbers
##' @param p_b threshold for how small p values to show. Any variable which is
##'     between 0 and 1 is considered a 'p-value' here, which need not be the
##'     case.
##' @param peq0 even if we abbreviate small 'p-values' should we explicitly put
##'     = "0" if it is equal to zero?
##' @param tmax how many characters to print for a character vector
##' @param repus should we replace "_" with "\\_" in charcter variables? (If not
##'     LaTeX might fail.)
##' @export
dtable_format <- function(dt, b = 1,
                          bh = 1, hfnc = base::round,
                          bl = 2, lfnc = base::signif,
                          br = 2, rfnc = base::round,
                          p_b = 0.0001, peq0 = TRUE,
                          tmax = 30, repus = TRUE){
    ## format numeric part
    n <- ncol(dt)
    R <- as.data.frame(dt)
    classy <- unlist(lapply(R, function(x) class(x)[1]))
    datum <- which(classy %in% c("Date", "POSIXlt", "POSIXct"))
    R[datum] <- lapply(R[datum], as.character)
    num <- which(classy == "numeric")
    l <- unlist(lapply(R[num], function(x) min(abs(x), na.rm = TRUE)))
    h <- unlist(lapply(R[num], function(x) max(abs(x), na.rm = TRUE)))
    p0 <- unlist(lapply(R[num], function(x) min(x, na.rm = TRUE) >= 0 &
                                            max(x, na.rm = TRUE) <= 1))
    p <- if(!is.null(p0)) which(p0) else NULL
    il <- num[which(l>b)]
    R[il] <- lapply(R[il], hfnc, digits = bh)
    ih <- num[which(h<=b)]
    R[ih] <- lapply(R[ih], lfnc, digits = bl)
    not_zero <- function(x) ifelse(x < p_b & x > 0, paste0("<", p_b), x)
    maybe_zero <- function(x) ifelse(x < p_b & x >= 0, paste0("<", p_b), x)
    zero <- function(x, not) if(not) not_zero(x) else maybe_zero(x)
    R[num[p]] <- lapply(R[num[p]], zero, not = peq0)
    i_rest <- setdiff(num, c(il, ih))
    R[i_rest] <- lapply(R[i_rest], rfnc, br)
    ## format character
    chr <- which(classy == "character")
    foo <- function(x){
        ifelse(nchar(x)>tmax + 2,
               paste0(substring(x, 1, tmax), "..."),
               x)
    }
    R[chr] <- lapply(R[chr], foo)
    if(repus){
        bar <- function(x) gsub("_", "\\_", x, fixed = TRUE)
        R[chr] <- lapply(R[chr], bar)
    }
    attributes(R) <- attributes(dt)
    R
}

