#' make a grouping list
#'
#' description tables with grouping requires a list of indexes to
#'     group on. Often this should be equivalent to categorization
#'     according to some grouping variable. If so, the 'glist' needed
#'     can be created by either specifying that variable or the name
#'     of that variable in data source 'ref'.
#' @title make glist
#' @param x object
#' @param ref reference, a data frame to get \code{x} from (if character) else a
#'     data frame or vector to compare with lengthwise
#' @param max.levels if the number of groups exceed this level stop.
#' @export
make_glist <- function(x, ref = NULL, max.levels = 25){
    if(!is.null(ref)){
        if(is.data.frame(ref)){
            if(is.character(x)){
                x <- ref[[x]]
            } else {
                if(length(x) != nrow(ref)){
                    stop("[make_glist] 'x' not a fit for the reference")
                }
            }
        } else {
            if(length(x) != length(ref)){
                stop("[make_glist] 'x' not equal in length to reference")
            }
        }
    }
    y <- as.factor(x)
    if(length(levels(y)) > max.levels){
        stop("[make_glist] the number of levels exceed 'max.levels'")
    }
    g <- as.list(NULL)
    for(k in levels(y)){
        g[[k]] <- y == k
    }
    g
}

#' factorize a glist
#'
#' reverse-engineer a categorical variable from glist, if possible
#' @param glist a list of indices
#' @export
factorize_glist <- function(glist){
    g <- as.data.frame(glist)
    rS <- rowSums(g)
    if(any(is.na(rS)) | any(stats::na.omit(rS) != 1)){
        text1 <- paste0("[descripteur::factorize_glist]: The grouping in glist",
                        " is not equivalent to a categorical variable")
        ss <- all(rowSums(g, na.rm = TRUE) <= 1)
        text2 <- if(ss){
                     "\n -- But there may be a natural subset that is!"
                 } else NULL
        stop(paste0(text1, text2))
    } else {
        r <- g
        for(k in seq_along(g)){
            r[[k]] <- ifelse(g[[k]], names(g)[k], "")
        }
        apply(r, 1, paste0, collapse = "")
    }
}

