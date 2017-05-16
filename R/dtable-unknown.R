##' get unknown
##'
##' get the class for the unknowns in a data set
##' @param data data frame or some such object
##' @param guide a guide (optional)
##' @export
dtable_unknown <- function(data, guide = NULL){
    if(is.null(guide)) guide <- dtable_guide(data)
    uguide <- guide[guide$type == "unknown", c("variable", "original_class")]
    names(uguide) <- c("variable", "original.class")
    uguide
}
