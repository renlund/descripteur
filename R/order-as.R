##' ordering of vector
##'
##' order a given vector, which may contain duplicates, according to the
##'   wanted order given by some other vector
##' @param given the vector that needs ordering
##' @param wanted the wanted order
##' @param incl.unordered should given elements not in wanted be kept?
##' @return an index vector
##' @examples
##' g <- c("b", "d", "b", "f", "c", "c", "a")
##' order_as(given = g, wanted = letters[1:4])
##' g[order_as(given = g, wanted = letters[1:4])]
##' g[order_as(given = g, wanted = letters[1:4], incl.unordered = FALSE)]
##' @export
order_as <- function(given, wanted, incl.unordered = TRUE){
    .s <- "_." ## necessary feature (?) text below explains why
    if(any(grepl(paste0("_\\.[0-9]_\\.$"), given))){
        mess <- paste0("'order_as' uses suffix '", .s, "<number>", .s, "' ",
                       "intervally hoping noone would ever use such a ",
                       "strange variable name, but if so then this might ",
                       "cause the ordering to fail. Please check the results ",
                       "(or rename your variables)")
        warning(mess)
    }
    want <- wanted[wanted %in% given]
    if(any(duplicated(want))){
        warning("duplicated entries in 'wanted'")
        want <- unique(want)
    }
    foo <- function(X) {
        n <- nrow(X)
        X$nr <- if(n==1) "" else 1:n
        NR <- if(n==1) "" else paste0(.s, 1:n, .s)
        X$attention  <- if(n==1) 0 else c(rep(0, n-1), 1)
        X$edited <- paste0(X$given, NR)
        X
    }
    df <- data.frame(given = given, stringsAsFactors = FALSE)
    spl <- lapply(split(df, f = df$given), foo)
    dc <- unsplit(spl, f = df$given)
    rownames(dc) <- NULL
    sdc <- subset(dc, dc$attention == 1)
    lw <- as.list(want)
    names(lw) <- want
    for(k in seq_along(sdc$given)){ ## k = 1
        K <- as.character(sdc$given[k])
        if(!K %in% names(lw)) next
        n <- sdc$nr[k]
        lw[[K]] <- sprintf(paste0(lw[[K]], .s, "%s", .s), 1:n)
    }
    W <- unlist(lw)
    G <- dc$edited
    indx <- match(W, G)
    rest <- setdiff(1:length(given), indx)
    if(incl.unordered){
        c(indx, rest)
    } else {
        indx
    }
}
##' ordering of vector by list
##'
##' order a given vector, which may contain duplicates, according to the
##'   wanted order given by some list
##' @param given the vector that needs ordering
##' @param wanted the wanted order (list)
##' @param incl.unordered should given elements not in wanted be kept?
##' @param unordered.label label for the not-in-wanted elements
##' @return list consisting of an index vector and a dataframe that connects the
##'   sorted elements with the corresponding list names. Also, the elements of
##'   \code{rle} applied to list.names entry of the dataframe
##' @examples
##' g <- c("b", "d", "b", "f", "c", "c", "a")
##' w <- list(
##'     "FOO" = c("a", "b"),
##'     "BAR" = c("c"),
##'     "BAZ" = c("e"),
##'     "QUUZ" = c("d")
##' )
##' order_by_list(g, w)
##' order_by_list(g, w, incl.unordered = FALSE)
##' @export
order_by_list <- function(given, wanted, incl.unordered = TRUE,
                          unordered.label = "Unlabelled"){
    if(!is.list(wanted)) stop("'wanted' should be a list")
    stxt <- paste0("\ncant have 'unordered.label' already ",
                   "be a name in 'wanted'\n")
    if(unordered.label %in% names(wanted)) stop(stxt)
    w <- unlist(wanted)
    if(any(duplicated(w))){
        stop("cannot have duplicated elements in 'wanted'")
    }
    if(incl.unordered){
        rest <- unique(setdiff(given, w))
        if(length(rest) > 0){
            wanted[[unordered.label]] <- rest
        }
    }
    indx <- order_as(given = given, wanted = w, incl.unordered = incl.unordered)
    g <- given[indx]
    how_many_in <- function(x, s = g) sum(s %in% x)
    repeat_listnames_by_entry <- function(l){
        r <- NULL
        for(i in seq_along(l)){
            r <- c(r, rep(names(l)[i], l[[i]][1]))
        }
        r
    }
    lab <- repeat_listnames_by_entry(lapply(wanted, how_many_in))
    Rle <- rle(lab)
    list(
        order = indx,
        sorted = data.frame(given = g, list.name = lab,
                            stringsAsFactors = FALSE),
        list.name.lengths = Rle$lengths,
        list.name.values = Rle$values
    )
}

##' @rdname order_by_list
##' @export
order_as_list <- order_by_list
