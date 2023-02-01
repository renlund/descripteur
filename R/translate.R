##' replace selected values
##'
##' Change values that corresponds to those of a key.
##' @param x Character (or factor) vector
##' @param key Named character vector; where (typically) the names corresponds
##'     to values appearing in x, and the values are those we want as
##'     replacements. However, if \code{flexible = TRUE}, then a check is
##'     performed to see if it makes more sense to invert the key.
##' @param flexible Logical; if TRUE the key can be inverted
##' @param within Logical; if TRUE occurences of the keyed values within a
##'     string can be replaced. Such occurences must be encapsulated by
##'     beginning of line, space characters, punctuation characters or end of
##'     line, so that replacement does not take place within words.
##' @examples
##' x <- c("foo", "foo!", "A foo", "A foo and two bar",
##'        "football", "barely", "bar")
##' key <- c(foo = "yay", bar = "bohnanza")
##' data.frame(
##'   x = x,
##'   d0 = key[x], ## useful only if all values of s exists in the key
##'   d1 = decipher(x, key),
##'   d2 = decipher(x, key, within = TRUE)
##' )
##' x <- factor(c(1,3,2), levels = 1:3,
##'             labels = c("foo", "bar", "baz"))
##' key <- c(A = "foo", C = "baz")
##' str(data.frame(x = x, d = decipher(x, key)))
##' @return vector of same length and class as s (character or factor)
##' @export
decipher <- function (x, key, flexible = TRUE, within = FALSE) {
    properties(x, class = c("character", "factor"))
    properties(key, class = "character", na.ok = FALSE)
    properties(names(key), nm = "names of key", class = "character",
               length = length(key), na.ok = FALSE)
    properties(flexible, class = "logical", length = 1, na.ok = FALSE)
    properties(within, class = "logical", length = 1, na.ok = FALSE)
    w_patt <- function(x){
        paste0("(^|[[:punct:]]|[[:space:]])",
               "(", x, ")",
               "($|[[:punct:]]|[[:space:]])")
    }
    if("factor" %in% class(x)){
        r <- factor(x = as.numeric(x),
                    labels = decipher(x = levels(x),
                                      key = key,
                                      flexible = flexible,
                                      within = within))
        return(r)
    }
    if(flexible){
        if(within){
            n1 <- sum(unlist(lapply(X = names(key),
                                    FUN = function(z) sum(grepl(w_patt(z), x)))))
            n2 <- sum(unlist(lapply(X = key,
                                    FUN = function(z) sum(grepl(w_patt(z), x)))))
        } else {
            n1 <- sum(names(key) %in% x)
            n2 <- sum(key %in% x)
        }
        if(n2 > n1) key <- setNames(names(key), nm = key)
    }
    if(within){
        r <- x
        for (i in seq_along(key)) {
            r <- gsub(pattern = w_patt(names(key)[i]),
                      replacement = paste0("\\1", key[i], "\\3"),
                      x = r)
        }
        r
    } else {
        r <- key[x]
        as.character(ifelse(is.na(r), x, r))
    }
}

#' @rdname decipher
#' @export
translate <- decipher

##:# translate by key
##:#
##:# if you  have a key \code{key <- c('a'  = 'A', 'b' = 'B')} you  can use it to
##:#     translate \code{s  <- c('a', 'b',  'a')} to \code{c('A', 'B',  'A')} via
##:#     \code{key[v]}, which is  nice. But if \code{s <- c('a',  'b', 'c')} then
##:#     \code{key[s]} is  \code{c('A', 'B', NA)}. The  function \code{translate}
##:#     will  make sure  that translations  occur whenever  a corresponding  key
##:#     entry exists, else keep the orginal value.
##:# @title translate
##:# @param s vector to translate
##:# @param key translation key
##:# @param flexible allow the key to be inversed if that seems more plausible?
##:# @param within logical; use \code{gsub} to translate within strings?
##:# @param rep1w logical; if \code{within = TRUE}, should replacement within
##:#     particular entry occur only once?
##:# @param f2c should factors be as character? (default \code{TRUE})
##:# @importFrom stats setNames
##:# @return vector
##:# @examples
##:# kEy = c("foo" = "Foo la la", "bar" = "Bar di da",
##:#         "baz" = "baz", "quz" = "Quz")
##:# sset = c("foo", "bar", "baz", "quz")[c(2,1,1,4,1,2,2,3,2,3)]
##:# table(new = translate(s = sset, key = kEy), old = sset, useNA = "ifany")
##:# sset = c("Foo la la", "baz", "Quz")
##:# table(new = translate(s = sset, key = kEy), old = sset, useNA = "ifany")
##:# ## 'within = TRUE' uses gsub to look WITHIN strings
##:# KeY <- c("foo" = "The Foo", "bar" = "The Bar", "a" = "A")
##:# sset <- c("I have foo", "no bar today", "i got an a", "foo", "a bar")
##:# translate(s = sset, key = KeY, within = TRUE, rep1w = TRUE)
##:# translate(s = sset, key = KeY, within = TRUE, rep1w = FALSE)
##:# translate(s = sset, key = KeY, within = FALSE, rep1w = TRUE)
## decipher <- function(s, key, flexible = TRUE, within = FALSE,
##                       rep1w = TRUE, f2c = TRUE){
##     n <- names(key)
##     if(is.factor(s) & f2c) s <- as.character(s)
##     us <- unique(s)
##     if(within){
##         if(flexible){
##             tmp <- function(z, u) grepl(pattern = z, x = u)
##             g1 <- sum(unlist(lapply(key, tmp, u = us)))
##             g2 <- sum(unlist(lapply(n, tmp, u = us)))
##             if(g1 > g2){
##                 key <- setNames(object = n, nm = key)
##             }
##             r1 <- r2 <- s
##             indx.cum <- rep(FALSE, length(s))
##             for(i in seq_along(key)){ ## i = 1
##                 indx <- grepl(pattern = names(key)[i], x = s)
##                 r1[indx] <- gsub(pattern = names(key)[i], replacement = key[i],
##                                  x = r1)[indx]
##                 r2[indx & !indx.cum] <- gsub(pattern = names(key)[i], replacement = key[i],
##                                              x = r1)[indx & !indx.cum]
##                 indx.cum <- indx.cum | indx
##             }
##             if(rep1w) r2 else r1
##         }
##     } else {
##         if(flexible){
##             if(sum(us %in% key) > sum(us %in% n)){
##                 key <- setNames(object = n, nm = key)
##             }
##         }
##         r = s
##         i = s %in% names(key)
##         r[i] <- key[s[i]]
##         r
##     }
## }
