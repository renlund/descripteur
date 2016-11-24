##' describe missing
##'
##' get some simple stats on missingness for a set of variables
##' @param data the data.frame or similar
##' @param v if \code{NULL} all variables are included, else a character vector of the
##'     names of wanted variables or a formula (if data is null it will look for
##'     the variables in the global workspace, but they need to be of the same length)
##' @param glist an index list or name of grouping variable
##' @export
##' @return a data.frame with
##' \itemize{
##'   \item{variable} name of variable
##'   \item{NA.n} number of \code{NA} in that variable
##'   \item{NA.perc} percent \code{NA} in that variable
##'   \item{n.assoc} number of variables with an OR > 1 and a p-value of <
##'     threshold p-value specified (according to fisher.test)
##'   \item{max.assoc} the variable name with the largest OR (>1) among those
##'     with a p < threshold p given
##' }
dtable_missing <- function(data = NULL, v = NULL, glist = NULL){
    df <- get_variables(x = v, data = data)
    N <- nrow(df)
    m <- ncol(df)
    if(N == 0) stop("empty data set")
    guide <- data.frame(variable = names(df), type = "real")
    a_flist <- flist(c("NA.n" = "d_missing", "NA.perc" = "d_missing.perc"))
    dtable(data = data, type = "real", desc = TRUE, guide = guide,
           desc.flist = a_flist, comp = FALSE, glist = glist)
}

# - # round numbers > 1, use 1 significant digit when >t, else "<t"
round_helper <- function(x, t = 0.1){
    if(length(x) != 1) stop("want length 1 vector")
    if(x==0) return(0)
    if(abs(x)>=1) return(round(x))
    if(abs(x)>=t) return(signif(x, 1))
    paste0("<", gsub("^0", "", t))
}

# - # round_helper for vectors
roundisch <- function(x, t = 0.1){
    n <- length(x)
    R <- rep(NA_character_, n)
    for(i in 1:n) R[i] <- round_helper(x[i], t=t)
    R
}

# - # create data frame from formula or names of variables
get_variables <- function(x = NULL, data = NULL){
    if(is.null(x)){
        if(is.null(data)) stop("need 'x' or 'data'")
        return(data)
    } else if(class(x) == "formula"){
        vars <- all.vars(x)
    } else if(class(x) == "character"){
        vars <- x
    } else {
        stop("what weird beast is 'x'?")
    }
    if(is.null(data)){
        for(k in seq_along(vars)){
            tmp <- get(vars[k], envir = .GlobalEnv)
            if(k == 1){
                R <- data.frame(wot = tmp)
                names(R) <- vars[k]
            } else {
                tryCatch({R[[vars[k]]] <- tmp},
                         error = function(e) stop("computer says no"))
            }
        }
        R
    } else {
        subset(data, TRUE,  select = vars)
    }
}

dtable_TNA <- function(data = NULL, tv = NULL, v = NULL, glist = NULL, NA.only =
                                                                           TRUE){
    ## is this superfluous?? can we just use dtable_missing with adequate flist
    ## and grouping on factorized time variable??
    t <- if(is.character(tv)) data[[tv]] else tv
    if(!any(c("date", "POSIXct") %in% class(t))){
        stop("need a date-isch variable as 'tv'")
    }
    df <- get_variables(x = v, data = data)
    if(NA.only){
        indx <- unlist(lapply(df, function(x) any(is.na(x))))
        df <- df[,indx]
    }
    if(length(df) == 0) return(as.data.frame(NULL))

    ## gör tiden numerisk, sätt origin = min(t)
    ## låt n vara ett argument för antal hugg (typ = 10 default)
    ## kom ihåg vilka datum klippen motsvarar (?)
    ## skapa en matrix för procentuell andel saknade per tidshugg o variabel


    ## continue here
    invisible(NULL) ## ...for now
}


###############################################################################

if(FALSE){
    f <- x ~ y + u
    all.vars(f)
    foo <- data.frame(bar=1, baz="A")
    get("bar", foo)
    bar <- as.data.frame(NULL)
    within(bar, x <- 1)

    n <- 10*3*2*51
    data <- data.frame(
        a  = rpois(n, 5),
        gr = rbinom(n, 1, 0.2),
        frt = sample(letters[1:5], n, T),
        lyy = rep(c("FOO", "BAR"), n/2),
        boo = round(rnorm(n, 10, 1), 1)
    )
    i <- sample(1:n, ceiling(runif(1, 1, n/3)))
    ic <- setdiff(1:n, i)
    data$a[i] <- NA
    data$gr[c(sample(i, length(i)*0.8),sample(ic, length(i)*0.1))] <- NA
    data$frt[c(sample(ic, length(i)*0.8),sample(i, length(i)*0.1))] <- NA
    data$lyy[sample(1:n, ceiling(runif(1, 1, n/3)))] <- NA
    data$boo[n/2] <- NA
    x <- NULL
    glist = "lyy"

    (R <- dtable_missing(data, x))
    print(R, sep = " ")
    class(R)

    dtable_missing(data)

    dtable_latex(R)


    get_variables(x = ~a+gr+frt+boo, data)
    get_variables(x = c("a", "gr"), data)
    get_variables(data = data)
    get_variables(c("n"))

}
