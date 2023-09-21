##' dtable for multiple types
##'
##' concatenate dtables for multiple types into a single dtable
##' @param data the data set
##' @param guide a dtable guide
##' @param ... arguments passed to dtable
##' @param types types wanted
##' @param desc.flists flists for description
##' @param comp.flists flists for comparison
##' @param test.flists flists for testing
##' @export
dtables <- function(data, guide = NULL, ..., types = NULL,
                    desc.flists = NULL, comp.flists = NULL, test.flists = NULL){
    ok_types <- descripteur_desc_types() ## c("real", "bnry", "catg", "date", "surv")
    if(is.null(guide)) guide <- dtable_guide(data)
    if(is.null(types)) types <- intersect_if_notnull(names(desc.flists),
                                intersect_if_notnull(names(comp.flists),
                                                     names(test.flists)))
    if(is.null(types)) types <- ok_types
    types <- intersect(types, unique(guide$type))
    if(is.null(desc.flists)) desc.flists <- flists_default(types = types,
                                                          thing = "desc")
    if(is.null(comp.flists)) comp.flists <- flists_default(types = types,
                                                          thing = "comp")
    if(is.null(test.flists)) test.flists <- flists_default(types = types,
                                                           thing = "test")
    if(!all(types %in% ok_types)){
        wot <- paste0(setdiff(types, ok_types), collapse = ", ")
        stop(paste0("some types specified are unknow: ", wot, "."))
    }
    R <- NULL
    dots <- list(...)
    for(TYP in types){ ## TYP = types[1] ## TYP = "catg"
        if(!TYP %in% guide$type) next
        ## cat("typ:", TYP, "\n")
        tmp <- do.call(dtable, args = c(list(data = data, type = TYP,
                                             guide = guide,
                                             desc.flist = desc.flists[[TYP]],
                                             comp.flist = comp.flists[[TYP]],
                                             test.flist = test.flists[[TYP]]),
                                        dots))
        if(nrow(tmp) == 0) next
        suppressWarnings(R <- if(is.null(R)){
            tmp
        } else {
            dtable_rbind(R, tmp)
        })
    }
    mod_guide <- subset(guide, guide$type %in% c(types, "unit.id"))
    mod_guide$type <- "real" ## this choice should not matter
    META <- dtable(data, type = "real",
                   desc = FALSE, comp = FALSE, test = FALSE,
                   guide = mod_guide, glist = dots$glist)
    aM <- attributes(META)
    transf <- setdiff(names(aM), c("names", "row.names", "class", "dc_param"))
    for(K in transf) attr(R, K) <- attr(META, K)
    R
}

intersect_if_notnull <- function(a, b){
    if(is.null(a) & is.null(b)){
        NULL
    } else if(is.null(a)){
        b
    } else if(is.null(b)){
        a
    } else {
        intersect(a, b)
    }
}

##' dtables to latex code
##'
##' dtables to latex code
##' @param dt a dtables
##' @param format logical; do you want to format?
##' @param format.param list of parameter values passed to format function
##' @param n size indicator in table (set to NULL to suppress this)
##' @param ... arguments passed to \code{dtable_latex}
##' @export
dtables2latex <- function(dt, format = TRUE, format.param = as.list(NULL),
                          n = c(n = "size"), ...){
    a1 <- dtable_prune(x = dt, rm = "variable")
    a2 <- dtable_prune(x = a1, rm = "info", info = TRUE)
    if("pinfo" %in% names(dt)){
        a2 <- dtable_fnote(dt = a2, info = "pinfo", fn.var = "p",
                           format = format, format.param = format.param)
    }
    ## da <- dattr(a2)
    ## if(length(i <- which(da == "desc")) == 1){
    ##     da[i] <- paste0("desc:", tot.name) ## <-- new arg tot.name
    ##     dattr(a2) <- da
    ## }
    if(!is.null(n)){
        n <- n[1]
        ref <- c(n = "size", units = "units", weight = "weight")
        if(! n %in% ref){
            s <- paste0("'n' must be one of: ", paste0(ref, collapse = ", "))
            stop(s)
        }
        i <- which(n == ref)
        nm <- if(!is.null(names(n)[1])) names(n)[1] else names(ref)[i]
        A <- attr(a2, paste0("glist_", n))
        if(is.null(A)) A <- attr(a2, paste0(n))
        names(a2)[names(a2) == "Summary"] <- paste0(nm, "=", A)
    }
    dtable_latex(a2, format = format, format.param = format.param, ...)
}

##' @describeIn dtables2latex deprecated function
##' @export
dtables2latex_ungrouped_helper <- function(dt, format = TRUE, ...){
    message("deprecated function?")
    a1 <- dtable_prune(x = dt, rm = "variable")
    a2 <- dtable_prune(x = a1, rm = "info", info = TRUE)
    dtable_latex(a2, format = format, ...)
}

##' @describeIn dtables2latex deprecated function
##' @export
dtables2latex_grouped_helper <- function(dt, format = TRUE, ...){
    message("deprecated function?")
    a1 <- dtable_prune(x = dt, rm = "variable")
    a2 <- dtable_prune(x = a1, rm = "info", info = TRUE)
    a3 <- if("pinfo" %in% names(dt)){
              dtable_fnote(dt = a2, info = "pinfo", fn.var = "p",
                           format = format)
          } else if(format){
              dtable_format(a2, ...)
          } else a2
    dtable_latex(a3, ...)
}

##' for exporting dtables (experimental)
##'
##' make a dtable ready to write to file
##' @param dt a dtables
##' @param rm columns to remove
##' @param reps vector of replacements, where the name replaces the entry,
##'     e.g. \code{c('foo' = 'bar')} will replace all 'bar' with 'foo'
##' @param format use \code{dtable_format}?
##' @export
dtables2file_helper <- function(dt, rm = NULL, reps = NULL, format = FALSE){
    if(is.null(reps)) reps <- c("   " = "\\quad:", "%" = "\\%")
    if(is.null(rm)) rm <- intersect(names(dt), c("variable", "pinfo", "info"))
    a1 <- dtable_prune(dt, rm = rm)
    if(format) a1 <- dtable_format(a1)
    for(i in seq_along(reps)){
        a1[] <- lapply(a1[], FUN = function(x) gsub(reps[i], names(reps)[i], x=x, fixed = TRUE))
    }
    ## a1[] <- lapply(a1[], FUN = function(x) gsub("\\quad:", "   ", x=x, fixed = TRUE))
    ## a1[] <- lapply(a1[], FUN = function(x) gsub("\\%", "%", x=x, fixed = TRUE))
    a1
}

##' @describeIn dtables2file_helper quick peek at dtable (in particular dtables)
##' @export
peek <- function(dt){
    dtables2file_helper(dt, format = TRUE)
}

##' data + vlist -> latex table (experimental)
##'
##' Create a dtables from data and make a nice table with "rgroup:s" using a
##' list that groups variables
##' @param data a data frame to be described
##' @param guide a dtable guide
##' @param var.list a variable list
##' @param caption caption
##' @param caption.lot caption for list of tables
##' @param label label
##' @param longtable use longtable?
##' @param ... arguments passed to \code{dtables}
##' @param format logical; want formatting?
##' @param format.param list; formatting parameters
##' @param n size indicator in table (set to NULL to suppress this)
##' @param tot.name name of single column (if no glist)
##' @examples
##' n = 100
##' d <- data.frame(
##'     id = sprintf("id%s", 1:n),
##'     age = rpois(n, 65),
##'     sex = sample(c("M", "F"), n, replace = TRUE),
##'     meas1 = rnorm(n, 10),
##'     meas2 = rnorm(n, 20),
##'     dag = as.Date("2020-01-01") + runif(n, min = 0, max = 365)
##' )
##' vlist = list(
##'     "ID" = c(id = "Identity code"),
##'     "Foo bar" = c(age = 'Age (years)',
##'                   sex = 'Biological gender',
##'                   dag = 'Index date'),
##'     "Baz quuz" = c(meas1 = 'Hiphopinin',
##'                    meas2 = 'Jazzerum')
##' )
##' g <- dtable_guide(d, unit.id = "id", no.bnry = TRUE)
##' data_vlist2latex(data = d, guide = g, var.list = vlist)
##' gl <- make_glist(x = "sex", ref = d)
##' data_vlist2latex(data = d, guide = g, var.list = vlist,
##'                  glist = gl, comp = TRUE, test = TRUE)
##' @export
data_vlist2latex <- function(data,
                             guide = NULL,
                             var.list = NULL,
                             caption = NULL,
                             caption.lot = caption,
                             label = NULL,
                             longtable = FALSE,
                             ...,
                             format = TRUE,
                             format.param = as.list(NULL),
                             n = c(n = "size"),
                             tot.name = "All",
                             attach = FALSE,
                             attach.path = "table",
                             attach.name = NULL
){
    if(is.null(guide)) guide <- dtable_guide(data = data)
    if(is.null(var.list)) stop("var.list cannot be NULL")
    var.key <- delist(var.list)
    guide$label <- decipher(x = guide$label, key = var.key)
    dt <- dtables(data = data, guide = guide, ...)
    ## oas <- order_as_list(given = dt$variable, wanted = var.list)
    dfv <- vlist2df(var.list)
    oas <- align(dt$variable, template = dfv$term, group = dfv$group)
    dt2 <- dt[oas$order, ]
    da <- dattr(dt2)
    if(length(i <- which(da == "desc")) == 1){
        da[i] <- paste0("desc:", tot.name)
        dattr(dt2) <- da
    }
    a1 <- dtable_prune(dt2, rm = c("variable", "Variables"))
    a2 <- dtable_prune(a1, rm = "info", info = TRUE)
    if("pinfo" %in% names(dt2)){
        a2 <- dtable_fnote(a2, info = "pinfo", fn.var = "p",
                           format = format, format.param = format.param)
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
        A <- attr(a2, paste0("glist_", n))
        if(is.null(A)) A <- attr(a2, paste0(n))
        names(a2)[names(a2) == "Summary"] <- paste0(nm, "=", A)
    }
    if(attach){
        if(is.null(attach.name)) attach.name <- gsub(":", "-", label, fixed = TRUE)
        fp <- file.path(attach.path, paste0(attach.name, ".csv"))
        utils::write.csv2(x = peek(dt), file = fp, row.names = FALSE)
        caption = paste0(caption, " \\attachfile{", fp, "}")
    }
    ## x <- dtables2file_helper(dt2)
    ## Add option to attach the data?
    dtable_latex(a2, caption = caption,
                 caption.lot = caption.lot,
                 label = label, title = "Variables",
                 format = format, format.param = format.param,
                 longtable = longtable,
                 ## rgroup = oas$list.name.values,
                 ## n.rgroup = oas$list.name.lengths,
                 rgroup = oas$group.rle$values,
                 n.rgroup = oas$group.rle$lengths,
                 rowname = dt2$Variables)
}
