##' apply function list
##'
##' apply the functions in flist to the variable 'x'
##' @title apply flist
##' @param x variable
##' @param flist list of functions
##' @param ... arguments passed to the functions in flist
##' @param xname name of variable
##' @export

apply_flist <- function(x, flist, ..., xname = NULL){
    r <- as.list(NULL)
    r$variable <- if(!is.null(xname)){
        xname
    } else {
        paste0(as.character(substitute(x)), collapse = "")
    }
    dots <- list(...)
    dots$x <- x
    dots$xname <- r$variable
    for(k in names(flist)){ ## k = names(flist)[1]
        r[[k]] <- if(is.function(fnc <- flist[[k]])){
                      ## fnc(x, ...)
                      do.call(what = fnc, args = dots)
                  } else {
                      tryCatch(as.character(flist[[k]]),
                               error = function(e) "*anomalie*")
                  }
    }
    r <- as.data.frame(r, stringsAsFactors = FALSE)
    class(r) <- c("dtable", "data.frame")
    attr(r, "dtable") <- c("meta", dattr(flist))
    r
}


if(FALSE){

    foo <- function(x, ..., y){
        bla <- list(...)
        bla[['x']] <- x
        return(bla)
    }
    foo(x=1, z = 1:4, tryck = "A", y = -1)
    do.call(foo, list(x=1, y=2, z=3))

    xr <- rnorm(10)
    xr[1] <- NA
    apply_flist(x = xr, flist = dr_def)
    apply_flist(x = xr, flist = dr_def, xname = "foO")
    apply_flist(x = xr, flist = dr_sym)
    apply_flist(x = xr, flist = dr_sym, w = (1:10)/10)

    xb <- rbinom(100, 1, 0.3)
    xb[c(3,89)] <- NA
    d.bnryify(xb)
    apply_flist(x = xb, flist = db_def)
    apply_flist(x = xb, flist = db_def, w = ifelse(xb == 1, 2, 1))

    xb <- sample(c(T, F), size = 100, replace = TRUE)
    xb[c(71,18)] <- NA
    d.bnryify(xb)
    apply_flist(x = xb, flist = db_def)

    xb <- sample(c("T", "F"), size = 100, replace = TRUE)
    xb[c(71,18)] <- NA
    d.bnryify(xb)
    apply_flist(x = xb, flist = db_def)

    xb <- factor(xb, levels = c("F", "T"))
    d.bnryify(xb)
    apply_flist(x = xb, flist = db_def)

    xc <- sample(letters[1:3], size = 100, replace = TRUE)
    xc[c(1,6,9,14,67)] <- NA
    apply_flist(x = xc, flist = dc_def)
    xc <- factor(xc, levels = letters[3:1])
    apply_flist(x = xc, flist = dc_def)
    apply_flist(x = xc, flist = dc_def, useNA = T)
    apply_flist(x = xc, flist = dc_def, w = 100:1)
    gl <- list("a" = rep(c(T, F),100), "b" = rep(c(F, T),100))
    apply_flist(x = xc, flist = cc_def, glist = gl)


    xd <- as.Date("2010-01-01") + round(rnorm(100, 0, 100))
    apply_flist(x = xd, flist = dd_def)
    apply_flist(x = xd[gl[[1]]], flist = dd_def)
    apply_flist(x = xd[gl[[2]]], flist = dd_def)
    apply_flist(x = xd, flist = cd_def, glist = gl)

    n <- 6
    f <- sample(letters[1:2], n, T)
    u <- rbinom(n, 1, 0.5)
    make_glist(f)
    make_glist(f, ref = u)
    tryCatch(make_glist(f, ref = c(u, 1)),
             error = function(e) "error, like expected")
    df <- data.frame(f, u)
    make_glist("f", ref = df)
    make_glist("u", ref = df)

    xr <- rnorm(n)
    apply_flist(xr, flist = cr_def, glist = make_glist(f))
    apply_flist(xr, flist = cr_sym, glist = make_glist(f))
}
