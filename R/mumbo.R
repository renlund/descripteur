##' random mumbo jumbo
##'
##' create random strings of letters of length m
##' @param n integer, how many random strings
##' @param m integer, length of each string
##' @param try.unique logical, make an honest try of getting the strings to be
##'     unique?
##' @export
mumbo <- function(n, m = 2, try.unique = TRUE){
    set1 <- c(letters, LETTERS)
    ## set2 <- c(set1, 0:9, ",", ";", "+", "-", "_", "*")
    R <- rep(NA_character_, n)
    dummy <- 1
    threshold <- if(try.unique) 100 else 0
    while ((all(is.na(R)) | any(duplicated(R))) & dummy < threshold) {
        for(k in 1:n){
            R[k] <- paste(
                sample(set1, size = m, replace = TRUE), ##m > length(set1)),
                collapse = ""
            )
        }
        dummy <- dummy + 1
    }
    if(dummy == threshold & try.unique) warning("uniqueness failed!")
    R
}

##' unique row names
##'
##' Make ostensibly (in LaTeX tables) non-unique rownames by adding random
##'     letters hidden in 'phantom' code. This is useful in combination with
##'     'rgroups'-feature of \code{Hmisc::latex}
##' @param df data frame
##' @param x rownames to be visible, in particular non-unique such names
##' @export
##' @examples
##' df <- data.frame(x = 1:100)
##' rownames(df) <- urnames(df, rep("A", 100))
urnames <- function(df, x = NULL){
    n <- nrow(df)
    m <- if(n<100) 2 else if(n<700) 3 else 4
    y <- paste0("$\\phantom{", mumbo(n, m, try.unique = TRUE), "}$")
    if(is.null(x)) y else paste0(x, y)
}

if(FALSE){

    n = 45
    m = 1
    unique = TRUE

    mumbo(100, 2, T)
    mumbo(1000, 3, T)

    df <- data.frame(x = 1:100)
    rownames(df) <- urnames(df, rep("A", 100))
    df
}
