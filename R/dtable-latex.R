##' latexify a dtable
##'
##' process dtable into latex code via \code{Hmisc::latex}.
##' @param dt a dtable
##' @param bling shall we compose suitable table bling from the
##'     attributes? (default \code{TRUE})
##' @param file (default empty string) passed to \code{Hmisc::latex}
##' @param where (default "htb") passed to \code{Hmisc::latex}
##' @param rowname (default \code{NULL}) passed to \code{Hmisc::latex}
##' @param ... passed to \code{Hmisc::latex}
##' @param format use \code{dtable_format}?
##' @param format.param list of parameters to pass to
##'     \code{dtable_format} (only used if \code{format = TRUE}).
##' @importFrom Hmisc latex
##' @export
dtable_latex <- function(dt, bling = TRUE,
                         file = "", where = "htb", rowname = NULL,
                         ...,
                         format = FALSE,
                         format.param = as.list(NULL)){
    if(format) dt <- dtable_format(dt, param = format.param)
    x <- as.data.frame.dtable(dt)
    A <- attributes(dt)
    d <- A$dtable
    d1 <- gsub("(meta)|(desc:*)", "", d)
    d2 <- gsub("comp", "Comparison", d1)
    r <- rle(d2)
    gs <- A$glist_size
    gw <- A$glist_weight
    foo <- function(s) paste(paste0(names(s), " ", s), collapse = ", ")
    s_text <- if(!is.null(gs)) paste0("Count: ", foo(gs), ".") else NULL
    w_text <- if(!is.null(gw)) paste0("Weight: ", foo(gw), ".") else NULL
    text <- if(!is.null(gs) | !is.null(gw)){
        paste0("{\\small\\emph{",s_text, " ", w_text, "}}")
    } else NULL
    if(all(d2 == "")) bling <- FALSE
    if(bling){
        Hmisc::latex(object = x, file = file, where = where,
                     rowname = rowname, cgroup = r$values,
                     n.cgroup = r$lengths, insert.bottom = text, ...)
    } else {
        Hmisc::latex(object = x, file = file, where = where,
                     rowname = rowname, ...)
    }
}

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
    vikt <- 0.5 + rbinom(n, 2, 0.4)

    g <- dtable_guide(df)
    dt <- dtable(data = df, type = "real", guide = g)
    dt <- dtable(data = df, type = "real", glist = "b1", guide = g, w
    = vikt)

    r <- rle(c("", "A", "A", "B", "B", "FOo"))

    dtable_latex(dt = dt)
    attributes(dt)

    foo <- function(x, glist, ...){
        t.test(x = x[glist[[1]]], y = x[glist[[2]]])$p.value
    }
    dattr(foo) <- "comp"
    opts_desc$set("compare_real" = flist(c("c_std.r", "foo" = "foo")))
    dt <- dtable(data = df, type = "real", glist = "b1",
                 guide = g, w = vikt)
    dtable_format(dt)

}
##' format a dtable
##'
##' overall, low-precision formatting of dtable objects - quick and
##'     dirty way of getting something ok (hopefully), fast. could be
##'     developed... its weird argument structure (all parameters
##'     gathered in a list) is due to it being
##'     considered most useful when called from other functions.
##' @param dt a  dtable
##' @param param list of parameter values
##' @export
dtable_format <- function(dt, param = as.list(NULL)){
    ## get default values
    if(is.null(b    <- param$b))    b <- 1 ## boundary
    if(is.null(bh   <- param$bh))   bh <- 1 ## digits arguments for hfnc
    if(is.null(hfnc <- param$hfnc)) hfnc <- base::round ## format fnc
    ## for numbers all > boundary
    if(is.null(bl   <- param$bl))   bl <- 2 ## digits argument for lfnc
    if(is.null(lfnc <- param$lfnc)) lfnc <- base::signif ## format fnc
    ## for numbers all < boundary
    if(is.null(p_b  <- param$p_b))  p_b <- 0.0001 ## threshold for p-values
    if(is.null(tmax <- param$tmax)) tmax <- 30
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
    R[num[p]] <- lapply(R[num[p]], function(x) ifelse(x < p_b & x > 0, paste0("<", p_b), x))
    i_rest <- setdiff(num, c(il, ih))
    R[i_rest] <- lapply(R[i_rest], round, 2)
    ## format character

    chr <- which(classy == "character")
    foo <- function(x){
        ifelse(nchar(x)>tmax + 2,
               paste0(substring(x, 1, tmax), "..."),
               x)
    }
    R[chr] <- lapply(R[chr], foo)
    attributes(R) <- attributes(dt)
    R
}
