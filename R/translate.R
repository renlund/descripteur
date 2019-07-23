##' translate by key
##'
##' if you  have a key \code{key <- c('a'  = 'A', 'b' = 'B')} you  can use it to
##'     translate \code{s  <- c('a', 'b',  'a')} to \code{c('A', 'B',  'A')} via
##'     \code{key[v]}, which is  nice. But if \code{s <- c('a',  'b', 'c')} then
##'     \code{key[s]} is  \code{c('A', 'B', NA)}. The  function \code{translate}
##'     will  make sure  that translations  occur whenever  a corresponding  key
##'     entry exists, else keep the orginal value.
##' @title translate
##' @param s vector to translate
##' @param key translation key
##' @param flexible allow a key to be inversed if that seems more plausible?
##' @importFrom stats setNames
##' @return vector
##' @export
translate <- function(s, key, flexible = TRUE){
    if(flexible){
        n <- names(key)
        us <- unique(s)
        if(sum(us %in% key) > sum(us %in% n)){
            key <- setNames(object = n, nm = key)
        }
    }
    r = s
    i = s %in% names(key)
    r[i] <- key[s[i]]
    r
}

if(FALSE){

    nyck = c("foo" = "Foo la la", "bar" = "Bar di da",
             "baz" = "baz", "quz" = "Quz")
    sset = c("foo", "bar", "baz", "quz")[c(2,1,1,4,1,2,2,3,2,3)]
    table(new = translate(s = sset, key = nyck), old = sset, useNA = "ifany")

    sset = c("Foo la la", "baz", "Quz")
    table(new = translate(s = sset, key = nyck), old = sset, useNA = "ifany")

}
