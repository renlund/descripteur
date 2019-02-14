
magnitude <- function(x){
    x <- abs(x)
    if(x == 0) return(0)
    e <- 1
    if(x>=1){
        func <- `/`
        if(x < 10) return(1)
    } else {
        func <- `*`
    }
    e <- e * 10
    y <- func(x, e)
    while(y < 1 | y > 10){
        e <- e*10
        y <- func(x, e)
    }
    1 / func(1, e)
}
magnitudes <- function(x){
    n <- length(x)
    if(n<1) stop("want length >0")
    r <- rep(NA, n)
    for(k in 1:n){
        r[k] <- magnitude(x[k])
    }
    r
}
rep2 <- function(s, times = 1, collapse = ""){
    n <- length(times)
    if(n < 1) stop("times should be at least length 1")
    ## if(min(times) < 1) stop("elements of time should be > 0")
    r <- rep(NA, n)
    for(i in 1:n){
        r[i] <- paste0(rep(s, times = times[i]), collapse = collapse)
    }
    r
}
couldBnumeric <- function(x){
    if(is.logical(x)) return(FALSE)
    if(is.factor(x)) x <- as.character(x)
    !is.null(tryCatch(
        expr = as.numeric(x),
        warning = function(w){
            ## warning("Does not seem to be completely interpretable as numeric")
            NULL
        }
    ))
}
vtype <- function(x, t2n = TRUE){
    if(t2n & couldBnumeric(x) & !is.numeric(x)){
        x <- as.numeric(as.character(x))
    }
    if(class(x) %in% c('character', 'factor', 'logical')){
        'text'
    } else if(is.numeric(x)){
        y <- x[!is.na(x)]
        l <- min(y)
        u <- max(y)
        if(l>=0 & u <= 1){
            'frac'
        } else if(abs(u) <= 1){
            'small'
        } else if(abs(l) >= 1){
            'large'
        } else {
            'mix'
        }
    } else {
        'unknown'
    }
}
dform_text <- function(x, chars = 30, ...){
    if(chars<4) chars = 4
    x <- as.character(x)
    n <- nchar(x)
    y <- paste0(substr(x, 1, chars-3), "...")
    ifelse(n>30, y, x)
}
dform_large <- function(x, ldigits = 1, lbound = 1e8, sdigits = 3, ...){
    x <- if(couldBnumeric(x)) as.numeric(x) else stop("bad x")
    x_copy <- x
    i <- which(x > lbound)
    a <- sprintf(paste0('%#.', ldigits, 'f'), x)
    if(length(i) > 0){
        a[i] <- sprintf(paste0('%#.', sdigits, 'g'), x[i])
    }
    a
}
dform_frac <- function(x, sdigits = 3, sbound = 1e-5, ...){
    x <- if(couldBnumeric(x)) as.numeric(x) else stop("bad x")
    x_copy <- x
    m <- magnitudes(x)
    i <- which(x < sbound)
    a <- sprintf(paste0('%#.', sdigits-1, 'f'), x / m)
    aa <- paste0(rep2(0, nchar(round(1/m))-1), gsub("\\.", "", a))
    aaa <- gsub("^0", "0.", aa)
    if(length(i) > 0){
        aaa[i] <- sprintf(paste0('%#.', sdigits-1, 'g'), x[i])
    }
    aaa
}
dform_small <- function(x, sdigits = 3, sbound = 1e-5, ...){
    i <- which(x < 0)
    a <- dform_frac(abs(x), sdigits = sdigits, sbound = sbound)
    if(length(i)>0){
        paste0("-", a)
    } else {
        a
    }
}
dform_mix <- function(x, ldigits = 1, lbound = 1e8, sdigits = 3, sbound = 1e-5, ...){
    ## XK NOT DONE YET
    y <- if(couldBnumeric(x)) as.numeric(x) else stop("bad x")
    a <- y[abs(y) < 1]
    b <- y[abs(y) >= 1]
    x[abs(y) < 1]  <- dform_small(a,  sdigits = sdigits, sbound = sbound)
    x[abs(y) >= 1] <- dform_large(b, ldigits = ldigits,
                                  lbound = lbound, sdigits = sdigits)

}
dform <- function(x, ...){
    switch(vtype(x),
           text = dform_text(x, ...),
           frac = dform_frac(x, ...),
           small = dform_small(x, ...),
           large = dform_large(x, ...),
           mix = dform_mix(x, ...))
}
dform2 <- function(x, ...){
    D <- x
    D[] <- lapply(x, dform, ...)
    D
}




if(FALSE){

    df <- within(data = data.frame(
        x = c(0.98, 0.4111, 0.07659, 0.005734, 0.8, 0.00000000345345),
        y = c(123456789, 101, 1000.123, -9876.1234, 90000, 1.01)
        ),
    {z = sample(c(-1,1), 6, replace = TRUE)*x;
        u = as.character(ifelse(z == -1, x, y));
        v = proh::mumbo(n=6, m = 40);
        w = sample(c(TRUE, FALSE), size = 6, replace = TRUE);
        s = factor(!w);
        t = factor(x)})

    vtype(x = df$w)
    lapply(df, vtype)

    dform_large(c(1.25, 19.98, 100.49876123, 987654321))

    dform(df$x)
    dform(df$x, chars = 5)
    dform(df$v, chars = 5)
    dform(x = df$y)

    df
    dform2(df)
    dform2(df, chars = 8)

}
