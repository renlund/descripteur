##' unlist alternative
##'
##' unlist a list of vectors, keeping vector names but not list names
##' @param x a list
##' @return vector
##' @examples
##' test <- list(foo = c("a" = 1), bar = c("b" = 2))
##' ## unlist will generally (in my opinion) keep too many or too few names
##' unlist(test)
##' unlist(test, use.names = FALSE)
##' ## delist will (for some purposes) do this better
##' delist(test)
##' ## also, recursively
##' test2 <- list(baz = c("c" = 3), quuz = test)
##' delist(test2)
##' @export
delist <- function(x){
    if(!is.list(x)) warning("'x' is not a list")
    R <- NULL
    for(i in seq_along(x)){
        y <- x[[i]]
        if(is.list(y)) y <- delist(x[[i]])
        R <- c(R, y)
    }
    R
}

##' invert list of named characters
##'
##' switch names/values in a list of named vectors
##' @param x a list of named vectors
##' @examples
##' test <- list(foo = c("a" = "first letter"),
##'              bar = c("b" = "second letter",
##'                      "c" = "third letter"))
##' invert_list(test)
##' @export
invert_list <- function(x){
    if(!is.list(x)) warning("'x' is not a list")
    foo <- function(z) setNames(object = names(z), nm = z)
    lapply(x, foo)
}
