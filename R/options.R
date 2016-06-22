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
               "describe_bnry" = flist(c("p" = "d_p.b")),
               "describe_catg" = flist(c("levels" = "d_levels", "p" = "d_p.c")),
               "describe_date" = flist(c("min" = "d_min", "max" = "d_max")),
               "compare_real"  = flist(c("std" = "c_std.r")),
               "compare_bnry"  = flist(c("std" = "c_std.b", "OR" = "c_OR")),
               "compare_catg"  = flist(c("diff" = "c_diff.c")),
               "compare_date"  = flist(c("overlap" = "c_overlap.d"))
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

