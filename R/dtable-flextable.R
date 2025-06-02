##' turn dtables to flextable
##'
##' A function to turn a dtables into a flextable. Note: you probably want to
##' create you dtables with \code{dtables( ... , indent = '', perc.sign = '%')},
##' i.e. set parameters 'indent' and 'perc.sign' to NOT give the default LaTeX
##' code for indentation and percentage sign.
##' @param object A return object from \code{dtables}
##' @param template Optional; a data.frame with entries 'label' (corresponding
##'     to the labels used for variables in the dtables object) in the order
##'     wanted, and (optionally) an entry 'group' if grouping is wanted.
##' @param caption The caption
##' @param use.groups Use grouping given by the template. Will use grouping (if
##'     such is given) if NULL
##' @param indent flextable printing will recognise spaces in docx and pdf,
##'     indent is a length 2 vector. First entry is the indentation used for
##'     variables/labels with respect to row groups. The second entry is used
##'     for categorical level displayed with respect to variables/labels.
##' @param gray logical; mark every other variable with gray background
##' @param fontsize length 3 vector; fontsize used for header, body and footer,
##'     respectively.
##' @param all.name if there is no (column) grouping, what should be the name
##'     used to the single column displayed (default "All")
##' @examples
##' ## dd is the template
##' dd <- data.frame(label = names(mtcars),
##'                  group = rep(c("A Foo", "The baz"), c(5,6)))
##' dt <- dtables(mtcars, indent = "", perc.sign = "%")
##' dtf <- dtables2flextable(dt, template = dd, caption = "Example",
##'                          all.name = "All cars!")
##' ## to view:
##' ## print(dtf, preview = "html")
##' ## print(dtf, preview = "docx")
##' ## print(dtf, preview = "pdf", latex_engine = "xelatex")
##' @export
dtables2flextable <- function (object, template = NULL, caption = NULL,
                               use.groups = NULL, indent = c(3, 3),
                               gray = TRUE, fontsize = c(11, 11, 9),
                               all.name = "All") {
    ## XK fix this:
    align <- ucR:::align
    addNArow <- ucR:::addNArow
    cluster.by.incl.next <- ucR:::cluster.by.incl.next

    labs <- object$variable
    if (is.null(template)) {
        template <- data.frame(group = "noGroup", label = labs[!duplicated(labs)])
        use.groups <- FALSE
    } else {
        if (is.null(use.groups)) use.groups <- TRUE
    }
    if (use.groups) {
        indent1 <- paste(rep(" ", indent[1]), collapse = "")
        indent2 <- paste(rep(" ", indent[1] + indent[2]), collapse = "")
    } else {
        indent1 <- ""
        indent2 <- paste(rep(" ", indent[2]), collapse = "")
    }

    group.names <- unique(template$group)
    al <- align(labs, template$label, all = FALSE)
    DB <- dtable_prune(object, rm = "variable", info = FALSE)
    vari <- object$variable[al$order]
    m <- length(vari)
    graa <- cluster.by.incl.next(c(vari[1:(m - 1)] == vari[2:m],
                                   FALSE)) %% 2
    pinfo <- NULL
    if("pinfo" %in% names(object)){
        pinfo <- object$pinfo[al$order]
        DB <- dtable_prune(DB, rm = "pinfo")
        DB$p <- dformat_num(DB$p)
    }
    if("Std" %in% names(DB)){
        DB$Std <- dformat_num(DB$Std, maybe.p = FALSE)
    }
    if("info" %in% names(object)){
        DB <- dtable_prune(DB, rm = "info", info = TRUE)
    }
    DB <- DB[al$order, ]

    sel_attr <- c("size", "weight", "units", "info")
    w_used <- !is.null(attr(object, "weight"))
    ignore_u <- is.null(attr(object, "units")) ||
        attr(object, "size") == attr(object, "units")

    if(w_used){
        sel_attr <- setdiff(sel_attr, "weight")
    } else {
        sel_attr <- setdiff(sel_attr, "size")
    }
    if(ignore_u) sel_attr <- setdiff(sel_attr, "units")
    a2t <- attr2text(DB, vector = TRUE, attr = sel_attr)
    index.s <- which(names(DB) == "Summary")

    ech <- rep(NA_character_, ncol(DB))
    ech[1] <- names(DB)[1]

    if(length(index.s) == 1){
        names(DB)[index.s] <- all.name ## XK as argument?
        if(w_used){
            ech[index.s] <- paste0("w = ", attr(DB, "weight"))
        } else {
            ech[index.s] <- paste0("n = ", attr(DB, "size"))
        }
    } else {
        z <- dattr(DB)[grepl("desc:", dattr(DB))]
        names(DB)[index.s] <- gsub("desc:", "", z)
        if(w_used){
            ech[index.s] <- paste0("w = ", attr(DB, "glist_weight"))
        } else {
            ech[index.s] <- paste0("n = ", attr(DB, "glist_size"))
        }
    }

    i.na <- NULL

    DB <- as.data.frame(DB)
    tmp <- DB$Variable
    DB$Variables <- ifelse(grepl("^ *:", tmp),
                          yes = paste0(indent2, tmp),
                          no = paste0(indent1, tmp))

    g <- merge(x = data.frame(label = vari),
               y = template[, c("label", "group")],
               by = "label", sort = FALSE)
    g$group <- factor(g$group, levels = unique(g$group))

    if (use.groups) {
        DB <- do.call(what = "rbind",
                      args = lapply(split(as.data.frame(DB), f = g$group),
                                    FUN = addNArow))
        graa.update <- rep(2, nrow(DB))
        graa.update[!is.na(DB$Variables)] <- graa
        graa <- graa.update
        if (!is.null(pinfo)) {
            pinfo.update <- rep(NA_character_, nrow(DB))
            pinfo.update[!is.na(DB$Variables)] <- pinfo
            pinfo <- pinfo.update
        }
        i.na <- which(is.na(DB$Variables))
        DB$Variables[is.na(DB$Variables)] <- levels(g$group)
        rownames(DB) <- NULL
    }
    names(DB)[1] <- " "

    ft <- flextable(DB)
    ft <- set_table_properties(ft, opts_pdf = list(tabcolsep = 3),
                               layout = "autofit")

    for (i in seq_along(i.na)) {
        ft <- bold(ft, i = i.na[i], j = 1)
        ft <- italic(ft, i = i.na[i], j = 1)
        ft <- merge_h(ft, i = i.na[i])
    }
    ft <- add_header_row(ft, values = ech, top = FALSE)
    ft <- hline(ft, i = 1, border = fp_border_default(width = 0),
        part = "header")
    ft <- bold(ft, i = 1, part = "header")

    ft <- if(TRUE){
              add_footer_lines(ft, values = paste0(a2t, collapse = ". "))
          } else {
              add_footer_lines(ft, values = a2t)
          }
    ft <- padding(ft, padding = 0, part = "footer")
    ft <- fontsize(ft, size = fontsize[1], part = "header")
    ft <- fontsize(ft, size = fontsize[2], part = "body")
    ft <- fontsize(ft, size = fontsize[3], part = "footer")

    if(!is.null(pinfo)){
        tests <- unique(na.omit(pinfo))
        j <- which(names(DB) == "p")
        dummy <- FALSE
        for(i in seq_along(tests)){ ## i = 1
            t <- tests[i]
            indx <- which(!is.na(DB$p) & DB$p != "" & !is.na(pinfo) & pinfo == t)
            ft <- footnote(ft, i = indx, j = j,
                           value = as_paragraph(t),
                           ref_symbols = letters[i], inline = dummy)
            dummy <- TRUE
        }
    }

    if (gray) ft <- bg(ft, i = graa == 1, bg = "#EFEFEF")
    if (!is.null(caption)) ft <- set_caption(ft, caption = caption)
    ft
}

if(FALSE){

    ## library(descripteur)
    library(flextable)

    n <- 6000
    D <- data.frame(
        id = sprintf("id%s", 1:n),
        gr2 = factor(rep(c("Group 1", "Group 2"), c(2000, 4000))),
        gr3 = factor(rep(c(sprintf("Group %s", LETTERS[1:3])), each = n / 3)),
        age = runif(n, 35, 95),
        sex = factor(sample(c("M", "F"), size = n, replace = TRUE)),
        NotInTemplate = runif(n),
        measx = 100*rbeta(n, 2, 1),
        measy = rexp(n, 1/100),
        catgx = factor(sample(0:1, size = n, replace = TRUE)),
        catgy = factor(sample(c("foo", "bar", "baz"), size = n, replace = TRUE)),
        vikt = rep(c(1.5, 0.75), c(2000, 4000))
        )
    D$age[97] <- NA
    D$measx[c(501, 842)] <- NA
    d <- data.frame(
        group = rep(c("Demographics", "A set of interesting covariates"), each = 4),
        variable = c("sex", "age", "gr2", "gr3",
                     "catgx", "catgy", "measx", "measy"),
        label = c("Sex", "Baseline age", "Foo group", "Bar group",
                  "Zero one variable", "Programming names",
                  "Higgs density", "Flogiston")
    )
    X <- D
    for(v in names(X)){
        l <- d$label[d$variable == v]
        if(length(l) != 0) attr(X[[v]], "label") <- d$label[d$variable == v]
    }

    g <- dtable_guide(X, unit.id = "id")

    dt <- dtables(D, guide = g, glist = "gr2",
                  indent = "", perc.sign = "%",
                  comp = TRUE, test = TRUE)
    dtf <- dtables2flextable(dt, template = d)
    print(dtf, preview = "html")
    print(dtf, preview = "docx")
    print(dtf, preview = "pdf", latex_engine = "xelatex")

    dt <- dtables(D, guide = g, glist = "gr2",
                  indent = "", perc.sign = "%",
                  comp = FALSE, test = TRUE)
    dtf <- dtables2flextable(dt, template = d)
    print(dtf, preview = "html")
    print(dtf, preview = "docx")
    print(dtf, preview = "pdf", latex_engine = "xelatex")

    dt <- dtables(D, guide = g, glist = "gr2",
                  indent = "", perc.sign = "%",
                  comp = TRUE, test = FALSE)
    dtf <- dtables2flextable(dt, template = d)
    print(dtf, preview = "html")
    print(dtf, preview = "docx")
    print(dtf, preview = "pdf", latex_engine = "xelatex")

    dt <- dtables(D, guide = g,
                  indent = "", perc.sign = "%")
    dtf <- dtables2flextable(dt, template = d)
    print(dtf, preview = "html")
    print(dtf, preview = "docx")
    print(dtf, preview = "pdf", latex_engine = "xelatex")

    dt <- dtables(D, guide = g, glist = "gr2", w = "vikt",
                  indent = "", perc.sign = "%",
                  comp = TRUE, test = TRUE)
    dtf <- dtables2flextable(dt, template = d)
    print(dtf, preview = "html")
    print(dtf, preview = "docx")
    print(dtf, preview = "pdf", latex_engine = "xelatex")

    dt <- dtables(D, guide = g, w = "vikt",
                  indent = "", perc.sign = "%")
    dtf <- dtables2flextable(dt, template = d, all.name = "All patients")
    print(dtf, preview = "html")
    print(dtf, preview = "docx")
    print(dtf, preview = "pdf", latex_engine = "xelatex")

    dt <- dtables(D, guide = g, indent = "", perc.sign = "%")
    dtf <- dtables2flextable(dt, all.name = "All patients")
    print(dtf, preview = "html")
    print(dtf, preview = "docx")
    print(dtf, preview = "pdf", latex_engine = "xelatex")

    object = dt
    template = d
    caption = "Lorem ipsum."
    use.groups = NULL
    indent = c(3, 3)
    gray = TRUE
    fontsize = c(11, 11, 9)
    all.name = "All"

    ## ft
    ## print(ft, preview = "docx")
    ## print(ft, preview = "pdf", latex_engine = "xelatex")

    test <- data.frame(x="a",y=2)
    rbind(test[NA, ][1, ], test)

}
