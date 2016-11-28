#' @title Description guide
#' @description Determine how to describe a data frame
#' @param data a data frame
#' @param elim.set names of variables to exclude from description
#' @param catg.tol only describe categorical data with no more than this
#'   many unique values
#' @param real.tol force numeric data with few ($<= \code{real.tol}) unique data
#'   points to be described as categorical
#' @param as.is if TRUE ignore all tolerence parameters
#' @param row.id the row identifier
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
                         as.is = FALSE, row.id = NULL, unit.id = NULL){
    if(catg.tol < 3 | real.tol < 3) stop("be more tolerant...")
    org_data <- data
    data <- subset(org_data, TRUE,
                   select = setdiff(names(data), c(elim.set, row.id, unit.id)))
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
    tmp_row <- if(!is.null(row.id)){
                   data.frame(
                       variable = row.id,
                       type = "row id.",
                       original_class = class(org_data[[row.id]]),
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
                       original_class = class(org_data[[unit.id]]),
                       has_missing = any(is.na(org_data[[unit.id]])),
                       check.names = FALSE,
                       row.names = NULL,
                       stringsAsFactors = FALSE
                   )
               } else NULL
    tmp_var <- data.frame(
        variable = c(r, c1, c2, b1, b2, b3, b4, d, s),
        type = rep(c("real", "catg", "bnry", "date", "surv"),
                   c(length(r), length(c(c1, c2)), length(c(b1,b2,b3,b4)),
                     length(d), length(s))),
        original_class = unlist(classy[c(r, c1, c2, b1, b2, b3, b4, d, s)]),
        has_missing = unlist(lapply(data[,c(r, c1, c2, b1, b2, b3, b4, d, s)],
                                    any_na)),
        check.names = FALSE,
        row.names = NULL,
        stringsAsFactors = FALSE
    )
    tmp <- rbind(tmp_row, tmp_unit, tmp_var)
    labels <- descripteur:::get_label(org_data)
    ret <- merge(tmp, labels, by = "variable")
    L <- as.list(NULL)
    for(K in ret$variable[ret$type %in% c("catg", "bnry")]){
        ## K <- ret$variable[ret$type %in% c("catg", "bnry")][1]
        L[[K]] <- levels(factor(data[[K]]))
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
    ## lab <- rep(NA_character_, nrow(ret))
    ## for(k in 1:nrow(ret)){
    ##     tmp_lab <- attr(data[[ret$variable[k]]], "label")
    ##     if(!is.null(tmp_lab)) lab[k] <- tmp_lab
    ## }
    ## ret$label <- lab
    ret
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
                    x
                }
    }
    data.frame(variable = Names, label = R, stringsAsFactors = FALSE)
}
