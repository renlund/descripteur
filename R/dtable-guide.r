#' @title Description guide
#' @description Determine how to describe a data frame
#' @param data a data frame
#' @param elim.set names of variables to exclude from description
#' @param catg.tol only describe categorical data with no more than this
#'   many unique values
#' @param real.tol force numeric data with few (\eqn{<=} \code{real.tol}) unique data
#'   points to be described as categorical
#' @param as.is if TRUE ignore all tolerance parameters
#' @param no.bnry if TRUE, there will be no distinction between 'bnry' and
#'     'catg', they will all be 'catg' (default \code{FALSE})
#' @param reduce.levels if  \code{TRUE}, empty levels will be removed
#' @param row.id the row identifier, does not really do much at this point
#' @param unit.id the unit identifier, this can later provide information on how
#'     many unique units there are in a table or subgroups thereof
#' @return a data frame describing each variable in the data set (excluding
#'     \code{elim.set}, \code{id}, and \code{unit.id}). each variable has
#'     \itemize{
#'       \item{type}{this decides how other descripteur functions handles the variables}
#'       \item{imposed_class}{this is the class we impose}
#'       \item{original_class}{what the variable was}
#'       \item{has_missing}{is there missing in the data frame for this variable}
#'     }
#'    The return object will also hold some information on id-variables and
#'     factorial levels within its attributes
#' @export
dtable_guide <- function(data, elim.set = NULL,
                         catg.tol = 20, real.tol = 5,
                         as.is = FALSE, no.bnry = FALSE,
                         reduce.levels = TRUE,
                         row.id = NULL, unit.id = NULL){
    if(catg.tol < 3 | real.tol < 3){
        stop(paste0("tolerance threshold or catg and real ",
                    "should be at least 3"))
    }
    data_source <- as.character(substitute(data))
    org_data <- data
    n_unique <- function(x) length(unique(stats::na.omit(x)))
    n_levels <- function(x) length(levels(x))
    n_is_1 <- function(x){
        if(is.factor(x) & !reduce.levels){
            n_levels(x) <= 1
        } else {
            n_unique(x) <= 1
        }
    }
    const <- names(data)[unlist(lapply(data, n_is_1))] ## MOVED down
    val <- setdiff(names(org_data), c(elim.set, row.id, unit.id, const))
    data <- org_data[, val, drop = FALSE]
    classy <- lapply(data, get_class)
    ## ## --- START: COULD we identify 'surv' by prefixes?
    ## ##     this would probably require lots of changes
    ## ##     but could be worth some
    ## surv.parts = TRUE,
    ## surv.prefix = c("time" = "t.", "event" = "ev.")
    ## if(surv.parts){
    ##     ti <- surv.prefix['time']
    ##     ev <- surv.prefix['event']
    ##     it <- grepl(ti, names(data))
    ##     ie <- grepl(ev, names(data))
    ##     tcan <- gsub(ti, "", names(data)[it], fixed = TRUE)
    ##     ecan <- gsub(ev, "", names(data)[ie], fixed = TRUE)
    ##     can <- intersect(tcan, ecan)
    ##     tvar <- paste0(ti, can)
    ##     evar <- paste0(ev, can)
    ## }
    ## ## --- END
    any_na <- function(x) any(is.na(x))
    real <- classy %in% c("numeric", "integer")
    char <- classy %in% c("character")
    data[char] <- lapply(data[char], factor)
    catg <- classy %in% c("factor", "character")
    if(reduce.levels){
        data[catg] <- lapply(data[catg], factor)
    }
    bnry <- classy %in% c("logical")
    data[bnry] <- lapply(data[bnry], factor)
    date <- classy %in% c("POSIXct", "POSIXlt", "Date")
    surv <- classy %in% c("Surv")
    unknown <- !real & !catg & !bnry & !date & !surv
    if(as.is){
        r  <- names(data)[real]
        c1 <- NULL
        c2 <- names(data)[catg]
        b1 <- NULL
        b2 <- NULL
        b3 <- NULL
        b4 <- names(data)[bnry]
        ign <- NULL
        u <- names(data)[unknown]
    } else {
        ## real_n <- lapply(subset(data,TRUE,names(data)[real]), n_unique)
        real_n <- lapply(data[, names(data)[real], drop = FALSE], n_unique)
        ## catg_n <- lapply(subset(data,TRUE,names(data)[catg]), n_levels)
        catg_n <- lapply(data[, names(data)[catg], drop = FALSE], n_levels)
        ## date_n <- lapply(subset(data,TRUE,names(data)[date]), n_unique)
        date_n <- lapply(data[, names(data)[date], drop = FALSE], n_unique)
        ## bnry_n <- lapply(subset(data,TRUE,names(data)[bnry]), n_unique)
        bnry_n <- lapply(data[, names(data)[bnry], drop = FALSE], n_unique)
        r  <- names(data)[real][real_n >  real.tol]
        c1 <- names(data)[real][real_n <= real.tol & real_n != 2]
        c2 <- names(data)[catg][catg_n <= catg.tol & catg_n != 2]
        b1 <- names(data)[real][real_n == 2]
        b2 <- names(data)[catg][catg_n == 2]
        b3 <- names(data)[date][date_n == 2]
        b4 <- names(data)[bnry]
        ign <- names(data)[catg][catg_n > catg.tol]
        u <- names(data)[unknown]
    }
    s <- names(data)[surv]
    d  <- names(data)[date]
    tmp_row <- if(!is.null(row.id)){
                   data.frame(
                       variable = row.id,
                       type = "row id.",
                       original_class = get_class(org_data[[row.id]]),
                       has_missing = any(is.na(org_data[[row.id]])),
                       check.names = FALSE,
                       row.names = NULL,
                       stringsAsFactors = FALSE
                   )
               } else NULL
    tmp_unit <- if(!is.null(unit.id)){
                   data.frame(
                       variable = unit.id,
                       type = "unit id.",
                       original_class = get_class(org_data[[unit.id]]),
                       has_missing = any(is.na(org_data[[unit.id]])),
                       check.names = FALSE,
                       row.names = NULL,
                       stringsAsFactors = FALSE
                   )
               } else NULL
    tmp_const <- if(length(const) > 0){
                     any_na <- function(x) any(is.na(x))
                     data.frame(
                         variable = const,
                         type = "constant",
                         original_class = unlist(lapply(org_data[const],
                                                        get_class)),
                         has_missing = unlist(lapply(org_data[const], any_na)),
                         check.names = FALSE,
                         row.names = NULL,
                         stringsAsFactors = FALSE
                     )
                 } else NULL
    tmp_var <- data.frame(
        variable = c(r, c1, c2, b1, b2, b3, b4, d, s, ign, u),
        type = rep(c("real", "catg", "bnry", "date", "surv", "ignored", "unknown"),
                   c(length(r), length(c(c1, c2)), length(c(b1,b2,b3,b4)),
                     length(d), length(s), length(ign), length(u))),
        original_class = unlist(classy[c(r, c1, c2, b1, b2, b3, b4, d, s, ign, u)]),
        has_missing = unlist(lapply(data[,c(r, c1, c2, b1, b2, b3, b4, d, s,
                                            ign, u),
                                         drop = FALSE], any_na)),
        check.names = FALSE,
        row.names = NULL,
        stringsAsFactors = FALSE
    )
    if(no.bnry){
        tmp_var$type[tmp_var$type == "bnry"] <- "catg"
    }
    tmp <- rbind(tmp_row, tmp_unit, tmp_const, tmp_var)
    labels <- get_label(org_data)
    ret <- merge(tmp, labels, by = "variable")
    L <- as.list(NULL)
    for(K in ret$variable[ret$type %in% c("catg", "bnry")]){
        ## L[[K]] <- levels(factor(data[[K]]))
        get_or_make_levels <- function(x){
            if(is.factor(x)) {
                levels(x)
            } else {
                as.character(sort(unique(stats::na.omit(x))))
            }
        }
        L[[K]] <- get_or_make_levels(data[[K]])
    }
    if(!is.null(L)) attr(ret, "levels") <- L
    if(!is.null(row.id)){
        attr(ret, "row.id") <- if(row.id %in% names(org_data)){
            if(length(unique(org_data[[row.id]])) != nrow(org_data)){
                warning(paste0("row.id does not seem to identify the rows\n",
                               "(there are too few unique values)\n"))
            }
            row.id
        } else {
            warning("row.id variable does not seem to exist")
            NULL
        }
    }
    if(!is.null(unit.id)){
        attr(ret, "unit.id") <- if(unit.id %in% names(org_data)){
            unit.id
        } else {
            warning("unit.id variable does not seem to exist")
            NULL
        }
    }
    attr(ret, "data_source") <- data_source
    class(ret) <- c("dtable_guide", "data.frame")
    ret
}

##' get class
##'
##' get/infer the class of a vector, giving priority to numeric, integer,
##'     character, factor, logical, Surv, Date, POSIXct and POSIXt
##' @param x a vector
##' @export
get_class <- function(x){
    x <- class(x)
    if(any(x == "numeric")) return("numeric")
    if(any(x == "integer")) return("integer")
    if(any(x == "character")) return("character")
    if(any(x == "factor")) return("factor")
    if(any(x == "logical")) return("logical")
    if(any(x == "Surv")) return("Surv")
    if(any(x == "Date")) return("Date")
    if(any(x == "POSIXct")) return("POSIXct")
    if(any(x == "POSIXt")) return("POSIXt")
    x[1]
}

##' print dtable_guide objects
##'
##' prints the data source attribute as well as the data frame
##' @param x a \code{dtable_guide } object
##' @param ... arguments passed to \code{print.data.frame}
##' @export
print.dtable_guide <- function(x, ...){
    if(!is.null(ds <- attr(x, "data_source"))){
        cat(paste("## This guide was created from data set:", ds, "\n"))
    }
    print.data.frame(x, ...)
}

# - # get labels of variables, if any
get_label <- function(data){
    Names <- names(data)
    if(is.null(Names)) return(NULL)
    R <- rep(NA_character_, length(Names))
    for(k in seq_along(Names)){
        R[k] <- if(is.null(x <- attr(data[[k]], "label"))){
                    Names[k]
                } else {
                    if(length(x) == 1 & is.character(x)){
                        x
                    } else {
                        s <- paste0("label for '", Names[k], "' is not what",
                                    " one, i.e. me the programmer, would expect.")
                        message(s)
                        Names[k]
                    }
                }
    }
    data.frame(variable = Names, label = R, stringsAsFactors = FALSE)
}

if(FALSE){
    ## tests on ordering
    ##  ... should/could dtable_guide imply an ordering of variables in
    ##      e.g. dtable_latex

    n <- 100
    df  <- data.frame(
        c_var = round(rnorm(10), 1),
        a_var = sample(letters[1:3], size = n, replace = TRUE),
        rid = as.character(1:n),
        f_var = as.Date("2010-01-01") + rpois(n, 365),
        b_var = factor(rpois(n, 50)),
        d_var = sample(c(TRUE, FALSE), size = n, replace = TRUE),
        g_var = rep("foo", n),
        id = paste0(letters, 1:n),
        h_var = rep(1:2, n/2),
        e_var = round(runif(n, 10, 20), 1),
        t.BAR = rexp(n, 1/10),
        t.FOO = rexp(n, 1/50),
        ev.FOO = rbinom(n, 1, .3),
        stringsAsFactors = FALSE
    )
    (g <- dtable_guide(
        data = df,
        elim.set = "h_var",
        catg.tol = 20,
        real.tol = 5,
        as.is = FALSE,
        no.bnry = FALSE,
        reduce.levels = TRUE,
        row.id = "rid",
        unit.id = "id",
        surv.parts = TRUE,
        surv.prefix = c("time" = "t.", "event" = "ev.")
     ))

    ## names(df)
    ## g$variable

    ## m <- match(names(df), g$variable)
    ## (g2 <- g[m[!is.na(m)], ])

    ## g2[order(g2$type, decreasing = TRUE), ]

}
