##' ordering of vector
##'
##' Order a given vector, which may contain duplicates, according to the wanted
##'   order given by some other vector. Supercedes 'order_as' and
##'   'order_by_list' in a single function.
##' @param x the vector that needs ordering
##' @param template the wanted order
##' @param group possible grouping
##' @param all logical; should given elements not in template be kept?
##' @param outgroup group name of elements not in template (if kept)
##' @export
align <- function(x, template = NULL, group = NULL, all = TRUE, outgroup = ".Other"){
    if(length(x) == 0){
        warning("zero length input makes no sense")
        return(as.list(NULL))
    }
    if(!is.null(group) & !is.null(template)){
        if(length(group) != length(template)){
            stop("template and group of the same length, please")}
    }
    if(is.null(template)) template = sort(unique(x))
    m <- match(x, template)
    distinct_m <- sort(unique(na.omit(m)))
    order <- rep(NA_integer_, length(x))
    dummy <- 0L
    for(d in distinct_m){
        i <- which(x %in% x[which(d==m)][1])
        n <- length(i)
        order[dummy + 1:n] <- i
        dummy <- dummy + n
    }
    if(any(is.na(m))){
        if(all){
            order[which(is.na(order))] <- which(is.na(m))
        } else {
            order <- order[!is.na(order)]
        }
    }
    z <- data.frame(x = x[order])
    if(is.null(group)){
        z$group <- outgroup
        list(order = order,
             sorted = z,
             group.rle = list(lengths = nrow(z),
                              values = outgroup))
    } else {
        tg <- data.frame(template = template, group = group)
        s <- merge(x = z, y = tg, all.x = TRUE,
                   by.x = "x", by.y = "template", sort = FALSE)
        if(all){
            s$group[is.na(s$group)] <- outgroup
        }
        Rle <- rle(s$group)
        class(Rle) <- "list"
        list(order = order,
             sorted = s,
             group.rle = Rle)
    }
}

vlist2df <- function(vlist){
    d <- delist(vlist)
    data.frame(
        term = as.character(d),
        label = names(d),
        group = rep(names(vlist), unlist(lapply(vlist, length)))
    )
}

df2vlist <- function(df, term = "term", label = "label", group = "group"){
    d <- df[, c(term, label, group)]
    names(d) <- c("term", "label", "group")
    L <- as.list(NULL)
    if(any(duplicated(v <- rle(d$group)$values))){
        s <- paste0("group values in df not contiguously arranged")
        stop(s)
    }
    for(i in seq_along(v)){
        tmp <- d[d$group == v[i], ]
        L[[v[i]]] <- setNames(tmp$term, nm = tmp$label)
    }
    L
}

if(FALSE){

    x <- c("bar", "foo", "foo", "apa", "apa", "quuz")
    w <- c("foo", "bar", "quuz")
    g <- c("A-team", "B-team", "B-team")
    L <- list("A-team" = c(Foo = w[1]),
              "B-team" = c(Bar = w[2], Quuz = w[3]))

    (df <- vlist2df(L))
    df2vlist(df)
    identical(L, df2vlist(vlist2df(L)))

    align(x, template = w)
    align(x, template = w, group = g)
    align(x, template = w, group = g, outgroup = "Monkeyz")
    align(x, template = w, group = g, all = FALSE)


    oas <- align(x, template = df$term, group = df$group)
    setNames(oas$group.rle$lengths, nm = oas$group.rle$values)

    order_as(x, w)
    order_by_list(x, L)


}

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
    .Deprecated(new = "align")
    .s <- "_." ## necessary feature (?) text below explains why
    if(any(grepl(paste0("_\\.[0-9]_\\.$"), given))){
        mess <- paste0("'order_as' uses suffix '", .s, "<number>", .s, "' ",
                       "internally hoping noone would ever use such a ",
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
    .Deprecated(new = "align")
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
