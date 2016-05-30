#' @title Description guide
#' @description Determine how to describe a data frame
#' @param data a data frame
#' @param elim.set names of variables to exclude from description
#' @param catg.tol only describe categorical data with no more than this
#'   many unique values
#' @param real.tol force numeric data with few ($<= \code{real.tol}) unique data
#'   points to be describes as categorical
#' @param date.tol force date data with few ($<= \code{date.tol}) unique data
#'   points to be describes as categorical
#' @param as.is if TRUE ignore all tolerence parameters
#' @export

dtable_guide <- function(data, elim.set = NULL, catg.tol = 6,
                           real.tol = catg.tol, date.tol = catg.tol,
                           as.is = FALSE){
    if(catg.tol < 3 | real.tol < 3 | date.tol < 3) stop("be more tolerant...")
    data <- subset(data, TRUE, select = setdiff(names(data), elim.set))
    class2 <- function(x) class(x)[1]
    classy <- lapply(data, class2)
    any_na <- function(x) any(is.na(x))
    # has_missing <- unlist(lapply(data, any_na))
    real <- classy %in% c("numeric", "integer")
    catg <- classy %in% c("factor", "character")
    date <- classy %in% c("POSIXct", "POSIXlt", "Date")
    bnry <- classy %in% c("logical")

    if(as.is){
        r  <- names(data)[real]
        c1 <- NULL
        c2 <- names(data)[catg]
        c3 <- NULL
        b1 <- NULL
        b2 <- NULL
        b3 <- NULL
        b4 <- names(data)[bnry]
        d  <- names(data)[date]
    } else {
        n_unique <- function(x) length(unique(na.omit(x)))
        real_n <- lapply(subset(data,TRUE,names(data)[real]), n_unique)
        catg_n <- lapply(subset(data,TRUE,names(data)[catg]), n_unique)
        date_n <- lapply(subset(data,TRUE,names(data)[date]), n_unique)
        bnry_n <- lapply(subset(data,TRUE,names(data)[bnry]), n_unique)
        r  <- names(data)[real][real_n >  real.tol]
        c1 <- names(data)[real][real_n <= real.tol & real_n != 2]
        c2 <- names(data)[catg][catg_n <= catg.tol & catg_n != 2]
        c3 <- names(data)[date][date_n <= date.tol & date_n != 2]
        b1 <- names(data)[real][real_n == 2]
        b2 <- names(data)[catg][catg_n == 2]
        b3 <- names(data)[date][date_n == 2]
        b4 <- names(data)[bnry]
        d  <- names(data)[date][date_n >  date.tol]
    }
    ret <- data.frame(
        variable = c(r, c1, c2, c3, b1, b2, b3, b4, d),
        type = rep(c("real", "catg", "bnry", "date"),
                   c(length(r), length(c(c1, c2, c3)), length(c(b1,b2,b3,b4)), length(d))),
        imposed_class = rep(c("numeric", "factor", "Date"),
                           c(length(r), length(c(c1, c2, c3, b1, b2, b3, b4)), length(d))),
        original_class = unlist(classy[c(r, c1, c2, c3, b1, b2, b3, b4,d)]),
        has_missing = unlist(lapply(data[,c(r, c1, c2, c3, b1, b2, b3, b4, d)], any_na)),
        check.names = FALSE,
        row.names = NULL,
        stringsAsFactors = FALSE
    )
    L <- NULL
    for(K in ret$variable[ret$type %in% c("catg", "bnry")]){
        L[[K]] <- levels(factor(data[[K]]))
    }
    if(!is.null(L)) attr(ret, "levels") <- L
    ret
}
