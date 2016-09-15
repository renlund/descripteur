#' @title Description guide
#' @description Determine how to describe a data frame
#' @param data a data frame
#' @param elim.set names of variables to exclude from description
#' @param catg.tol only describe categorical data with no more than this
#'   many unique values
#' @param real.tol force numeric data with few ($<= \code{real.tol}) unique data
#'   points to be described as categorical
#' @param as.is if TRUE ignore all tolerence parameters
#' @param id the row identifier
#' @param unit.id the unit identifier
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
                         as.is = FALSE, id = NULL, unit.id = NULL){
    if(catg.tol < 3 | real.tol < 3) stop("be more tolerant...")
    data <- subset(data, TRUE,
                   select = setdiff(names(data), c(elim.set, id, unit.id)))
    class2 <- function(x) class(x)[1]
    classy <- lapply(data, class2)
    any_na <- function(x) any(is.na(x))
    real <- classy %in% c("numeric", "integer")
    catg <- classy %in% c("factor", "character")
    date <- classy %in% c("POSIXct", "POSIXlt", "Date")
    bnry <- classy %in% c("logical")
    surv <- classy %in% c("Surv")
    if(as.is){
        r  <- names(data)[real]
        c1 <- NULL
        c2 <- names(data)[catg]
        b1 <- NULL
        b2 <- NULL
        b3 <- NULL
        b4 <- names(data)[bnry]
    } else {
        n_unique <- function(x) length(unique(stats::na.omit(x)))
        real_n <- lapply(subset(data,TRUE,names(data)[real]), n_unique)
        catg_n <- lapply(subset(data,TRUE,names(data)[catg]), n_unique)
        date_n <- lapply(subset(data,TRUE,names(data)[date]), n_unique)
        bnry_n <- lapply(subset(data,TRUE,names(data)[bnry]), n_unique)
        r  <- names(data)[real][real_n >  real.tol]
        c1 <- names(data)[real][real_n <= real.tol & real_n != 2]
        c2 <- names(data)[catg][catg_n <= catg.tol & catg_n != 2]
        b1 <- names(data)[real][real_n == 2]
        b2 <- names(data)[catg][catg_n == 2]
        b3 <- names(data)[date][date_n == 2]
        b4 <- names(data)[bnry]
    }
    s <- names(data)[surv]
    d  <- names(data)[date]
    ret <- data.frame(
        variable = c(r, c1, c2, b1, b2, b3, b4, d, s),
        type = rep(c("real", "catg", "bnry", "date", "surv"),
                   c(length(r), length(c(c1, c2)), length(c(b1,b2,b3,b4)),
                     length(d), length(s))),
        ## imposed_class = rep(c("numeric", "factor", "Date", "Surv"),
        ##                    c(length(r), length(c(c1, c2, b1, b2, b3, b4)),
        ##                      length(d), length(s))),
        original_class = unlist(classy[c(r, c1, c2, b1, b2, b3, b4, d, s)]),
        has_missing = unlist(lapply(data[,c(r, c1, c2, b1, b2, b3, b4, d, s)], any_na)),
        check.names = FALSE,
        row.names = NULL,
        stringsAsFactors = FALSE
    )
    L <- NULL
    for(K in ret$variable[ret$type %in% c("catg", "bnry")]){
        L[[K]] <- levels(factor(data[[K]]))
    }
    if(!is.null(L)) attr(ret, "levels") <- L
    if(!is.null(id)){
        attr(ret, "id") <- if(id %in% names(data)){
            id
        } else {
            warning("id variable does not seem to exist")
            NULL
        }
    }
    if(!is.null(unit.id)){
        attr(ret, "unit.id") <- if(unit.id %in% names(data)){
            unit.id
        } else {
            warning("unit.id variable does not seem to exist")
            NULL
        }
    }
    lab <- rep(NA_character_, nrow(ret))
    for(k in 1:nrow(ret)){
        tmp_lab <- attr(data[[ret$variable[k]]], "label")
        if(!is.null(tmp_lab)) lab[k] <- tmp_lab
    }
    ret$label <- lab
    ret
}

