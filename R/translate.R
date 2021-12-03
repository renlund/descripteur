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
##' @param flexible allow the key to be inversed if that seems more plausible?
##' @param within logical; use \code{gsub} to translate within strings?
##' @param rep1w logical; if \code{within = TRUE}, should replacement within
##'     particular entry occur only once?
##' @param f2c should factors be as character? (default \code{TRUE})
##' @importFrom stats setNames
##' @return vector
##' @examples
##' kEy = c("foo" = "Foo la la", "bar" = "Bar di da",
##'         "baz" = "baz", "quz" = "Quz")
##' sset = c("foo", "bar", "baz", "quz")[c(2,1,1,4,1,2,2,3,2,3)]
##' table(new = translate(s = sset, key = kEy), old = sset, useNA = "ifany")
##' sset = c("Foo la la", "baz", "Quz")
##' table(new = translate(s = sset, key = kEy), old = sset, useNA = "ifany")
##' ## 'within = TRUE' uses gsub to look WITHIN strings
##' KeY <- c("foo" = "The Foo", "bar" = "The Bar", "a" = "A")
##' sset <- c("I have foo", "no bar today", "i got an a", "foo", "a bar")
##' translate(s = sset, key = KeY, within = TRUE, rep1w = TRUE)
##' translate(s = sset, key = KeY, within = TRUE, rep1w = FALSE)
##' translate(s = sset, key = KeY, within = FALSE, rep1w = TRUE)
##' @export
decipher <- function(s, key, flexible = TRUE, within = FALSE,
                      rep1w = TRUE, f2c = TRUE){
    n <- names(key)
    if(is.factor(s) & f2c) s <- as.character(s)
    us <- unique(s)
    if(within){
        if(flexible){
            tmp <- function(z, u) grepl(pattern = z, x = u)
            g1 <- sum(unlist(lapply(key, tmp, u = us)))
            g2 <- sum(unlist(lapply(n, tmp, u = us)))
            if(g1 > g2){
                key <- setNames(object = n, nm = key)
            }
            r1 <- r2 <- s
            indx.cum <- rep(FALSE, length(s))
            for(i in seq_along(key)){ ## i = 1
                indx <- grepl(pattern = names(key)[i], x = s)
                r1[indx] <- gsub(pattern = names(key)[i], replacement = key[i],
                                 x = r1)[indx]
                r2[indx & !indx.cum] <- gsub(pattern = names(key)[i], replacement = key[i],
                                             x = r1)[indx & !indx.cum]
                indx.cum <- indx.cum | indx
            }
            if(rep1w) r2 else r1
        }
    } else {
        if(flexible){
            if(sum(us %in% key) > sum(us %in% n)){
                key <- setNames(object = n, nm = key)
            }
        }
        r = s
        i = s %in% names(key)
        r[i] <- key[s[i]]
        r
    }
}

#' @rdname decipher
#' @export
translate <- decipher

## deciphactor <- function(x, key, ...){
##     v <- if(is.factor(x)){
##              levels(x)
##          } else {
##              as.character(unique(x[!is.na(x)]))
##          }
##     factor(decipher(x, key),
##            levels = decipher())
## ... no
## }
