## function for setting up sane 'comp' and 'desc' defaults
dc_param <- function(desc = NULL, comp = NULL, glist = NULL){
    if(is.null(desc)) desc <- TRUE
    if(is.null(comp)) comp <- if(is.null(glist)) FALSE else TRUE
    if(is.character(desc)){
        if(!desc %in% c("each", "first")){
            stop("if character, desc should be 'each' or 'first'")
        }
        desc.style <- desc
        desc <- TRUE
    } else {
        desc.style <- NA_character_
    }
    if(is.character(comp)){
        if(!comp %in% c("overall", "across", "adjacent")){
            stop("if character, comp should be 'overall', 'across' or 'adjacent'")
        }
        comp.style <- comp
        comp <- TRUE
    } else {
        comp.style <- NA_character_
    }
    if(comp){
        if(is.null(glist)){
            warning("comp set, but no glist?")
            comp <- FALSE
            comp.style <- NA_character_
        } else if(is.na(comp.style)){
            comp.style <- "overall"
        }
    }
    if(desc){
        if(is.null(glist)){
            if(is.na(desc.style)) desc.style <- "first"
        } else {
            if(comp){
                desc.style <- if(comp.style == "overall"){
                                  "each"
                              } else {
                                  "first"
                              }
            } else {
                desc.style <- "each"
            }
        }
    }
    list("desc" = desc,
         "desc.style" = desc.style,
         "comp" = comp,
         "comp.style" = comp.style)
}

if(FALSE){
    dc_param()
    dc_param(desc = T, glist = 1)
    dc_param(glist=1)
    dc_param(comp = "across", glist = 1)
    dc_param(comp = "adjacent", glist = 1)
    dc_param(comp = "adjacent", glist = NULL)
}


## this might be useful later if there is ever a function to convert
## a variable to a 'footnote'
latex_symbols <- function(){
    symb <- c("bot", "forall", "flat", "sharp", "top", "S", "bigstar", "Join",
               "clubsuit", "diamondsuit", "spadesuit",  "heartsuit",
               "dagger", "ast", "star", "circ", "ddagger", "bullet")
    greekl <- c("alpha", "beta", "gamma", "delta", "epsilon", "varepsilon",
                "zeta", "eta", "theta", "vartheta", "iota", "kappa", "lambda",
                "mu", "nu", "xi", "pi", "varpi", "rho", "varrho", "sigma", "varsigma",
                "tau", "upsilon", "phi", "varphi", "chi", "psi", "omega")
    greeku <- c("Gamma","Delta","Theta","Lambda","Xi","Pi","Sigma",
                "Upsilon","Phi","Psi","Omega")
    invisible(NULL)
}
