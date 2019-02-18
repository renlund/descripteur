##' describable types
##'
##' function that returns the names of all types that can be described
##' @export
descripteur_desc_types <- function(){
    c("real", "catg", "bnry", "date", "surv")
}

##' other types
##'
##' function that returns the types, other than describable ones, that can occur
##'     in a guide
##' @export
descripteur_other_types <- function(){
    c("row id.", "unit id.", "constant", "ignored", "unknown")
}
