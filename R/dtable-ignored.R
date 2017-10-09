##' get ignored
##'
##' get the number of distinct data points for the ignored in a data set
##' @param data data frame or some such object
##' @param guide a guide (optional)
##' @export
dtable_ignored <- function(data, guide = NULL){
    if(is.null(guide)) guide <- dtable_guide(data)
    cguide <- guide[guide$type == "ignored",]
    cdata <- data[, cguide$variable, drop = FALSE]
    if(ncol(cdata) == 0) {
        message("There are no ignored variables in data")
        return(as.data.frame(NULL))
    }
    foo <- function(x) length(unique(stats::na.omit(x)))
    data.frame(
        variable = cguide$label,
        `distinct values` = unlist(lapply(cdata, foo)),
        stringsAsFactors = FALSE
    )
}
