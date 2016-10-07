# - #' make a grouping list
# - #'
# - #' description tables with grouping requires a list of indexes to
# - #'     group on. Often this should be equivalent to categorization
# - #'     according to some grouping variable. If so, the 'glist' needed
# - #'     can be created by either specifying that variable or the name
# - #'     of that variable in data source 'ref'.
# - #' @title make glist
# - #' @param x object
# - #' @param ref reference
make_glist <- function(x, ref = NULL){
    if(!is.null(ref)){
        if(is.data.frame(ref)){
            if(is.character(x)){
                x <- ref[[x]]
            } else {
                if(length(x) != nrow(ref)) stop("nah1")
            }
        } else {
            if(length(x) != length(ref)) stop("nah2")
        }
    }
    y <- as.factor(x)
    if(length(levels)>100) stop("nah3")
    g <- as.list(NULL)
    for(k in levels(y)){
        g[[k]] <- y == k
    }
    g
}

# - #' factorize a glist
# - #'
# - #' reverse-engineer a categorical variable from glist, if possible
# - #' @param glist
# - #' @return a character vector
factorize_glist <- function(glist){
    g <- as.data.frame(glist)
    rS <- rowSums(g)
    if(any(is.na(rS)) | any(stats::na.omit(rS) != 1)){
        text1 <- "[descripteur/factorize_glist]: The grouping in glist is not equivalent to a categorical variable"
        ss <- all(rowSums(g, na.rm = TRUE) <= 1)
        text2 <- if(ss) "\n -- But there may be a natural subset that is!" else NULL
        stop(paste0(text1, text2))
    } else {
        r <- g
        for(k in seq_along(g)){
            r[[k]] <- ifelse(g[[k]], names(g)[k], "")
        }
        apply(r, 1, paste0, collapse = "")
    }
}

