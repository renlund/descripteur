##' Standardized differences
##'
##' Table with standardized differences for all variable types
##' @param data data frame or similar
##' @param v if \code{NULL} all variables are included, else a character vector of the
##'     names of wanted variables or a formula (if data is null it will look for
##'     the variables in the global workspace, but they need to be of the same length)
##' @param guide a guide (by \code{dtable_guide}), either to select variables OR
##'     to provide unit information (the type-info will not be used)
##' @param glist an index list or name of grouping variable
##' @param comp (kind of) comparison
##' @param ... arguments passed to \code{dtables}(\code{dtable})
##' @export
dtable_std <- function(data = NULL, v = NULL, guide = NULL, glist,
                       comp = "across", ...){
    df <- get_variables(x = v, data = data)
    N <- nrow(df)
    m <- ncol(df)
    if(N == 0) stop("empty data set")
    if(is.null(guide)) guide <- dtable_guide(df)
    a_flists <- flists(real = flist(c("Std" = "c_rstd")),
                       bnry = flist(c("Std" = "c_bstd")),
                       catg = flist(c("Std" = "c_cstd")),
                       date = flist(c("Std" = "c_dstd")),
                       surv = flist(c("Std" = "c_sstd")))
    dots <- list(...)
    dt <- do.call(dtables, args = c(list(data = data, guide = guide,
                  comp.flists = a_flists, desc.flists = NULL,
                  comp = comp, desc = FALSE, glist = glist,
                  expand.levels = FALSE), dots))
    ## dt <- dtables(data = data, guide = guide,
    ##               comp.flists = a_flists, desc.flists = NULL,
    ##               comp = comp, desc = FALSE, glist = glist,
    ##               expand.levels = FALSE, ...)
    dt
}
