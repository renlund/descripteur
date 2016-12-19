## These function creates and handles overall descripteur options
## and will hopefully be elaborated. The options are stored in
## an environment 'le_milieu'

## @title le_milieu
## @description an environment

le_milieu <- new.env(parent = getNamespace("descripteur"))

# @title desc_get
# @description this function retrieves the descripteur settings
# @param name name of desc setting variable

desc_get <- function(name){
   if(length(ls(envir=le_milieu))==0) desc_restore()
   defaults <- get("defaults", envir=le_milieu)
   if (missing(name))
      defaults
   else {
      L <- as.list(NULL)
      for(k in name){
         L[[k]] <- defaults[[k]]
      }
      if(length(L) == 1) L[[1]] else L
   }
}

# @title desc_set
# @description this function sets the descripteur settings
# @param ... the names and values you want set

desc_set <- function(...){
   if(length(ls(envir=le_milieu))==0) desc_restore()
   dots <- list(...)
   value <- get("value", le_milieu)
   for(k in names(dots)) if(!(k %in% value)) dots[[k]] <- NULL
   current <- desc_get()
   for(k in names(dots)) current[[k]] <- dots[[k]]
   assign(x="defaults", value=current, envir=le_milieu)
   desc_check()
   invisible(NULL)
}

# @title desc_restore
# @description this function restores the default descripteur settings

desc_restore <- function(){
    assign(x="defaults",
           value=list(
               "describe_real" = flist(c("mean" = "d_mean", "sd" = "d_sd")),
               "describe_bnry" = flist(c("value" = "d_ref_level", "prop" = "d_bp")),
               "describe_catg" = flist(c("levels" = "d_levels", "prop" = "d_cp")),
               "describe_date" = flist(c("min" = "d_dmin", "max" = "d_dmax")),
               "describe_surv" = flist(c("time" = "d_tsum",
                                 "events" = "d_esum", "rate" = "d_rate")),
               "compare_real"  = flist(c("std" = "c_rstd")),
               "compare_bnry"  = flist(c("std" = "c_bstd")),
               "compare_catg"  = flist(c("levels" = "d_levels", "diff" = "c_pdiff")),
               "compare_date"  = flist(c("overlap" = "c_overlap")),
               "compare_surv"  = flist(c("RR" = "c_rr")),
               "desc_compact" = flists(real = list("dt_name",  "dt_Q.info",     "dt_Q"),
                                       bnry = list("dt_bname", "dt_bcp.info",   "dt_bcp"),
                                       catg = list("dt_cname", "dt_ccp.info",   "dt_ccp"),
                                       date = list("dt_name",  "dt_date.info",  "dt_date"),
                                       surv = list("dt_name",  "dt_event.info", "dt_event"),
                                       names = c("Variables",  "info",  "Summary")),
               "comp_compact" = flists(real = list("dt_name",  "c_rstd"),
                                       bnry = list("dt_bname", "c_bstd"),
                                       catg = list("dt_cname", "c_cstd"),
                                       date = list("dt_name",  "dt_empty_comp"),
                                       surv = list("dt_name",  "dt_empty_comp"),
                                       names = c("Variables",  "Std"))
   ), envir=le_milieu)
   assign(x="value", value = names(get(x="defaults", envir=le_milieu)), envir=le_milieu)
   desc_check()
   invisible(NULL)
}

# @title desc_check
# @description some checks of the descripteur options

desc_check <- function(){
   NULL
}

#' @title desc options
#' @description This list tries to mimic the behaviour of opts_chunk from knitr.
#' Currently these values are maintained with the functions in (the list)
#' \code{opts_desc}:
#' \itemize{
#' \item value - default: foo
#' }

#' @export

opts_desc <- list(
   "get" = desc_get,
   "set" = desc_set,
   "restore" = desc_restore,
   "check" = desc_check
)

