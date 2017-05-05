##' get constants
##'
##' get the value of the constants in a data set
##' @param data data frame or some such object
##' @param guide a guide (optional)
##' @export
dtable_constants <- function(data, guide = NULL){
    if(is.null(guide)) guide <- dtable_guide(data)
    cguide <- guide[guide$type == "constant",]
    cdata <- data[, cguide$variable, drop = FALSE]
    if(nrow(cdata) == 0) return(as.data.frame(NULL))
    foo <- function(x){
        val <- unique(stats::na.omit(x))
        if(length(val) > 1){
            stop("Some variable deemed constant is, in fact, not constant")
        } else {
            if(length(val) == 0) "missing" else val
        }
    }
    data.frame(
        variable = cguide$label,
        value = unlist(lapply(cdata, foo))
    )
}

