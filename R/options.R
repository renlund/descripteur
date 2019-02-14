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
               ## describe ----------------------------------------------------
               ## "describe_real" = flist(c("median" = "d_median",
               ##                           "IQR" = "d_IQR")),
               "describe_real" = flist(c("mean" = "d_mean",
                                         "sd" = "d_sd")),
               "describe_bnry" = flist(c("value" = "d_ref_level",
                                         "prop" = "d_bp")),
               "describe_catg" = flist(c("levels" = "d_levels",
                                         "prop" = "d_cp")),
               "describe_date" = flist(c("min" = "d_dmin",
                                         "max" = "d_dmax")),
               "describe_surv" = flist(c("time" = "d_tsum",
                                         "events" = "d_esum",
                                         "rate" = "d_rate")),
               ## compare -----------------------------------------------------
               "compare_real"  = flist(c("std" = "c_rstd")),
               "compare_bnry"  = flist(c("std" = "c_bstd")),
               "compare_catg"  = flist(c("levels" = "d_levels",
                                         "diff" = "c_pdiff")),
               "compare_date"  = flist(c("overlap" = "c_overlap")),
               "compare_surv"  = flist(c("rr" = "c_rr")),
               ## describe compact --------------------------------------------
               "describe_real_compact" = flist(c("Variables" = "dt_name",
                                                 "info" = "dt_Q.info",
                                                 "Summary" = "dt_Q")),
               "describe_bnry_compact" = flist(c("Variables" = "dt_bname",
                                                 "info" = "dt_bcp.info",
                                                 "Summary" = "dt_bcp")),
               "describe_catg_compact" = flist(c("Variables" = "dt_cname",
                                                 "info" = "dt_ccp.info",
                                                 "Summary" = "dt_ccp")),
               "describe_date_compact" = flist(c("Variables" = "dt_name",
                                                 "info" = "dt_date.info",
                                                 "Summary" = "dt_date")),
               "describe_surv_compact" = flist(c("Variables" = "dt_name",
                                                 "info" = "dt_event.info",
                                                 "Summary" = "dt_event")),
               ## compare compact ---------------------------------------------
               "compare_real_compact" = flist(c("Variables" = "dt_name",
                                                ##"Std" = "c_rstd",
                                                ##"pinfo" = "dt_wilcox.p.info",
                                                "pinfo" = "dt_kruskal.p.info",
                                                ##"p" = "dt_wilcox.p",
                                                "p" = "dt_kruskal.p")),
               "compare_bnry_compact" = flist(c("Variables" = "dt_bname",
                                                ##"Std" = "c_bstd",
                                                "pinfo" = "dt_chisq.p.info",
                                                "p" = "dt_chisq.p")),
               "compare_catg_compact" = flist(c("Variables" = "dt_cname",
                                                ##"Std" = "c_cstd",
                                                "pinfo" = "dt_chisq.p.info",
                                                "p" = "dt_chisq.p")),
               "compare_date_compact" = flist(c("Variables" = "dt_name",
                                                ##"Std" = "c_dstd",
                                                "pinfo" = "dt_empty_meta",
                                                "p" = "dt_empty_comp")),
               "compare_surv_compact" = flist(c("Variables" = "dt_name",
                                                ##"Std" = "c_sstd",
                                                "pinfo" = "dt_empty_meta",
                                                "p" = "dt_empty_comp")),
               ## other -------------------------------------------------------
               "warn_if_weight_not_used" = TRUE,
               "warn_if_wrong_glist_length" = TRUE
           ), envir=le_milieu)
    assign(x="value", value = names(get(x="defaults", envir=le_milieu)),
           envir=le_milieu)
    desc_check()
    invisible(NULL)
}

# @title desc_check
# @description some checks of the descripteur options
desc_check <- function(){
   NULL
}

#' @title desc options
#' @description This list tries to mimic the behavior of opts_chunk from knitr.
#' Currently these values are maintained with the functions in (the list)
#' \code{opts_desc}:
#' \itemize{
#' \item get - get the current values
#' \item set - set new values
#' \item restore - restore default values
#' }
#' @export
opts_desc <- list(
   "get" = desc_get,
   "set" = desc_set,
   "restore" = desc_restore,
   "check" = desc_check
)
