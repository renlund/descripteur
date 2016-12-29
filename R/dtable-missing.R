#' describe missing
#'
#' get some simple stats on missingness for a set of variables
#' @param data the data.frame or similar
#' @param v if \code{NULL} all variables are included, else a character vector of the
#'     names of wanted variables or a formula (if data is null it will look for
#'     the variables in the global workspace, but they need to be of the same length)
#' @param guide a guide (by \code{dtable_guide}), either to select variables OR
#'     to provide unit information (the type-info will not be used)
#' @param glist an index list or name of grouping variable
#' @param info type of display
#' @param only.with only show those variables with some missing
#' @export
#' @return a data.frame with
#' \itemize{
#'   \item{variable} name of variable
#'   \item{count} number of \code{NA} in that variable
#'   \item{percent} percent \code{NA} in that variable
#' }
dtable_missing <- function(data = NULL, v = NULL, guide = NULL, glist = NULL,
                           info = "latex", only.with = TRUE){
    df <- get_variables(x = v, data = data)
    N <- nrow(df)
    m <- ncol(df)
    if(N == 0) stop("empty data set")
    if(is.null(guide)) guide <- dtable_guide(df)
    guide$type[!guide$type %in% c("unit id.", "row id.")] <- "real"
    if(only.with){
        no_miss <- guide$label[!guide$has_missing]
        guide <- subset(guide, guide$has_missing)
        if(nrow(guide) == 0) {
            message("there are no missing")
            invisible(NULL)
        }
    }
    a_flist <- flist(c("Count" = "d_missing", "Percent" = "d_missing.perc"))
    dt <- dtable(data = data, type = "real", desc = TRUE, guide = guide,
                 desc.flist = a_flist, comp = FALSE, glist = glist)
    if(only.with & length(no_miss)>0){
        a <- if(info == "latex"){
                 paste0(
                     paste0("\\texttt{",
                            gsub("_", "\\_", no_miss, fixed = TRUE),
                            "}"),
                 collapse = ", ")
             } else {
                 paste0(no_miss, collapse = ", ")
             }
        attr(dt, "info") <- c(attr(dt, "info"),
            paste0("Variables examined but found to be complete: ", a, "."))
    }
    dt
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


dtable_std <- function(data = NULL, v = NULL, guide = NULL, glist, comp = "across"){
    df <- get_variables(x = v, data = data)
    N <- nrow(df)
    m <- ncol(df)
    if(N == 0) stop("empty data set")
    if(is.null(guide)) guide <- dtable_guide(df)
    foo <- function(x, glist, ...) c_cstd(x = x, glist = glist, expand = FALSE)
    attr(foo, "dtable") <- "comp"
    a_flists <- flists(real = flist(c("Std" = "c_rstd")),
                       bnry = flist(c("Std" = "c_bstd")),
                       catg = flist(c("Std" = "foo"), local = TRUE))
    dt <- dtables(data = data, guide = guide,
                  comp.flists = a_flists, desc.flists = NULL,
                  comp = comp, desc = FALSE, glist = glist)
    dt
}
