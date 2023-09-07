##' data + vlist -> kable table (experimental)
##'
##' Create a dtables from data and make a nice table with "rgroup:s" using a
##' list that groups variables
##' @param data a data frame to be described
##' @param guide a dtable guide
##' @param var.list a variable list
##' @param caption caption
##' @param label label
##' @param ... arguments passed to \code{dtables}
##' @param format logical; want formatting?
##' @param format.param list; formatting parameters
##' @param n size indicator in table (set to NULL to suppress this)
##' @param tot.name name of single column (if no glist)
##' @param kbl.format character; format passed to \code{kableExtra::kbl}
##' @export
data_vlist2kbl <- function(data,
                           guide = NULL,
                           var.list = NULL,
                           caption = NULL,
                           label = NULL,
                           ...,
                           format = TRUE,
                           format.param = as.list(NULL),
                           n = c(n = "size"),
                           tot.name = "All",
                           kbl.format = NULL
){
    if(is.null(kbl.format)) kbl.format = "html"
    ok.kbl.formats <- c('latex', 'html', 'pipe', 'simple','rst')
    if( !(kbl.format %in% ok.kbl.formats) ){
        s <- paste0("kbl.format should be one of {",
                    paste0(ok.kbl.formats, collapse = ", "), "}")
        stop(s)
    }
    perc.code <- if(kbl.format == "latex") "\\%" else "%"
    if(is.null(guide)){
        guide <- dtable_guide(data = data)
        guide$label <- decipher(x = guide$label, key = var.key)
    }
    if(is.null(var.list)) stop("var.list cannot be NULL")
    var.key <- delist(var.list)
    dots <- list(...) ## dots <- as.list(NULL) ## for testing
    dt <- do.call(what = "dtables",
                  args = c(list(data = data, guide = guide,
                                indent = "", perc.sign = perc.code),
                           dots))
    oas <- order_as_list(given = dt$variable, wanted = var.list)
    dt2 <- dt[oas$order, ]
    rownames(dt2) <- NULL ## kbl seems to want to print rownames unless thay are 1:n
    da <- dattr(dt2)
    if(length(i <- which(da == "desc")) == 1){
        da[i] <- paste0("desc:", tot.name)
        dattr(dt2) <- da
    }
    dt2 <- dtable_prune(dt2, rm = c("variable"))
    dt2 <- dtable_prune(dt2, rm = "info", info = TRUE)
    dt2 <- do.call(what = "dtable_format", args = c(list(dt = dt2), format.param))
    add_p <- "pinfo" %in% names(dt2) & "p" %in% names(dt2)
    if(add_p){
        fn <- dt2$pinfo
        fn2 <- factor(fn)
        dt2 <- dtable_prune(dt2, rm = "pinfo")
        dt2$p <- ifelse(test = dt2$p != "",
                        yes = paste(dt2$p,
                                    kableExtra::footnote_marker_symbol(x = as.numeric(fn2),
                                                                       format = kbl.format)),
                        no = dt2$p)
    } else {
        fn2 <- NULL
    }
    if (!is.null(n)) {
        n <- n[1]
        ref <- c(n = "size", units = "units", weight = "weight")
        if (!n %in% ref) {
            s <- paste0("'n' must be one of: ", paste0(ref, collapse = ", "))
            stop(s)
        }
        i <- which(n == ref)
        nm <- if (!is.null(names(n)[1])){
                  names(n)[1]
              } else names(ref)[i]
        A <- attr(dt2, paste0("glist_", n))
        if(is.null(A)) A <- attr(dt2, paste0(n))
        names(dt2)[names(dt2) == "Summary"] <- paste0(nm, "=", A)
    }
    pr <- setNames(oas$list.name.lengths, nm = oas$list.name.values)
    ind <- which(grepl("^:", dt2$Variables))
    k <- kableExtra::kbl(x = dt2, format = kbl.format, caption = caption, escape = FALSE)
    k <- kableExtra::pack_rows(kable_input = k, index = pr)
    k <- kableExtra::add_indent(kable_input = k, position = ind)
    k <- kableExtra::add_header_above(kable_input = k,
                                      header = dattr2rh(dt2), align = "l")
    k <- kableExtra::footnote(kable_input = k,
                              general = attr2text(dt2), general_title = "Note:",
                              symbol = levels(fn2), symbol_title = "Tests:",
                              footnote_as_chunk = TRUE)
    k
}

dattr2rh <- function(dt){
    d <- dattr(dt)
    d <- gsub("meta", " ", d)
    d <- gsub("(^desc:)|(^comp:)", "", d)
    d <- gsub("comp", "Comparison", d)
    d <- gsub("test", "Test", d)
    r <- rle(d)
    setNames(r$lengths, nm = r$values)
}
