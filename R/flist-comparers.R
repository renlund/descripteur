     ## +----------------------------------------+ ##
     ## | comparing functions for real variables | ##
     ## +----------------------------------------+ ##

d.shift <- function(x, glist, ...){
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]

    (median(x1, na.rm = TRUE) - median(x2, na.rm = TRUE)) /
        IQR(x1, na.rm = TRUE)
}

cr_def <- list(
    "shift" = d.shift
)
attr(cr_def, "dtable") <- "comp"

d.std.diff <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    x1 <- x[glist[[1]]]*w1*length(w1[w1!=0])/sum(w1)
    x2 <- x[glist[[2]]]*w2*length(w2[w2!=0])/sum(w2)
    (mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE)) /
        sqrt(((sd(x1, na.rm = TRUE))^2 + (sd(x2, na.rm = TRUE))^2) / 2)
}

cr_sym <- list(
    "std" = d.std.diff
)
attr(cr_sym, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for bnry variables | ##
     ## +----------------------------------------+ ##

d.RR <- function(x, glist, w = NULL, ...){
    d.p(x[glist[[1]]], w = w[glist[[1]]]) /
        d.p(x[glist[[2]]], w = w[glist[[2]]])
}
d.OR <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    d.odds(x = x1, w = w1) / d.odds(x2, w = w2)
}
if(FALSE){
    x = c(1, 1, 1, 0, 0, 0, 0, 0)
    gl <- list("A" = c(T,T,F,F,T,T,F,F),
               "B" = c(F,F,T,T,F,F,T,T))
    w1 = rep(1, 8)
    w2 = 2*x + 1
    w3 = 1:8
    d.bnryify(x)
    d.odds(x)
    d.odds(x, w = w1)
    d.odds(x, w = w2)
    ##
    d.odds(x[gl[[1]]])
    d.odds(x[gl[[2]]])
    d.OR(x, glist = gl)
    ##
    d.odds(x[gl[[1]]], w = w2[gl[[1]]])
    d.odds(x[gl[[2]]], w = w2[gl[[2]]])
    d.OR(x, glist = gl, w = w2)
    ##
    d.odds(x[gl[[1]]], w = w3[gl[[1]]])
    d.odds(x[gl[[2]]], w = w3[gl[[2]]])
    d.OR(x, glist = gl, w = w3)
    ##
    apply_flist(x, flist = descripteur:::db_def, w = w3)
    apply_flist(x[gl[[1]]], flist = descripteur:::db_def, w =w3[gl[[1]]])
    apply_flist(x[gl[[2]]], flist = descripteur:::db_def, w =w3[gl[[2]]])
    apply_flist(x, flist = descripteur:::cb_def, glist = gl, w = w3)
    }
d.stdp <- function(x, glist, w = NULL, ...){
    if(is.null(w)) w <- rep(1, length(x))
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    p1 <- d.p(x1, w = w1)
    p2 <- d.p(x2, w = w2)
    (p1 - p2) / sqrt((p1*(1-p1) + p2*(1-p2)) / 2)
}
cb_def <- list(
    ## "RR" = d.RR,
    "std" = d.stdp,
    "OR" = d.OR
)
attr(cb_def, "dtable") <- rep("comp", 2)

     ## +----------------------------------------+ ##
     ## | comparing functions for date variables | ##
     ## +----------------------------------------+ ##

d.overlap <- function(x, glist, ...){
    x1 <- x[glist[[1]]]
    x2 <- x[glist[[2]]]
    a <- max(min(x1, na.rm = TRUE), min(x2, na.rm = TRUE))
    b <- min(max(x1, na.rm = TRUE), max(x2, na.rm = TRUE))
    sum(x >= a & x <= b, na.rm = TRUE) ## / sum(!is.na(x))
}

cd_def <- list(
    "n.overlap" = d.overlap
)
attr(cd_def, "dtable") <- "comp"

     ## +----------------------------------------+ ##
     ## | comparing functions for catg variables | ##
     ## +----------------------------------------+ ##

d.cc_diff <- function(x, glist, useNA = FALSE, w = NULL, ...){
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    p1 <- d.tab(x[glist[[1]]], w = w1)
    p2 <- d.tab(x[glist[[2]]], w = w2)
    dp <- p1 - p2
    if(useNA) c(dp, NA) else dp
}

d.cc_OR <- function(x, glist, useNA = FALSE, w = NULL, ...){
    w1 <- w[glist[[1]]]
    w2 <- w[glist[[2]]]
    p1 <- d.tab(x[glist[[1]]], w = w1)
    p2 <- d.tab(x[glist[[2]]], w = w2)
    OR <- (p1 / (1-p1)) / (p2 / (1-p2))
    if(useNA) c(OR, NA) else OR
}
## d.levels exists in two locations ... problem?
d.levels <- function(x, useNA = TRUE, w = NULL, ...){
    y <- d.catgify(x)
    if(useNA) c(levels(y), .missing_char) else levels(y)
}

cc_def <- list(
    "levels" = d.levels,
    "diff" = d.cc_diff,
    "OR" = d.cc_OR
)
attr(cc_def, "dtable") <- c("meta", "comp", "comp")


if(FALSE){
    df <- data.frame(
        x = c(1:4,7),
        ## w = c(4:1,0)
        w = rep(1,5)
        )
    with(df, weighted.mean(x, w))
    with(df, mean(x*w*length(w)/sum(w)))
    with(df[1:2,], weighted.mean(x, w))
    with(df[1:2,], mean(x*w*length(w)/sum(w)))
    with(df[3:5,], weighted.mean(x, w))
    with(df[3:5,], mean(x*w*length(w)/sum(w)))
}
