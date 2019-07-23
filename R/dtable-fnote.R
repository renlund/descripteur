##' create (latex) footnotes from table variables
##'
##' make \code{unique(variable)} a part of attribute 'info' and
##'     place a footnote on another variable
##' @param dt a dtable
##' @param info variable with the footnote info
##' @param fn.var variable to get footnotes
##' @param info.attr name of attribute to store info in
##' @param format format the dtable?
##' @param format.param parameters for formatting
##' @export
dtable_fnote <- function(dt, info, fn.var,
                         info.attr = "info",
                         format = FALSE,
                         format.param = as.list(NULL)){
    if(length(info) != 1 | length(fn.var) != 1){
        stop("want 'info' and 'fn.var' to be length 1")
    }
    foo <- function(x){
        if(is.character(x)){
            if(sum(names(dt) %in% x) > 1){
                stop(paste0("'", x, "' identifies more than one variable"))
            }
        }
    }
    foo(info)
    foo(fn.var)
    if(length(format.param)>0) format <- TRUE
    if(format){
        dt <- do.call(dtable_format,
                      c('dt' = list(dt), format.param))
    }
    infot   <- unique(as.character(stats::na.omit(unlist(dt[[info]]))))
    i.infot <- as.numeric(factor(dt[[info]], levels = infot))
    symb <- latex_symbols(n = max(i.infot, na.rm = TRUE),
                          pre = "$\\phantom{.}^{\\", suff = "}$")
    symb2 <- latex_symbols(n = max(i.infot, na.rm = TRUE),
                           pre = "$^{\\", suff = "}$")
    sym.infot <- paste0(symb, infot)
    attr(dt, info.attr) <- c(attr(dt, info.attr), sym.infot)
    fn_var <- dt[[fn.var]]
    new_var <- paste0(id_or_empty(fn_var), id_or_empty(symb2[i.infot]))
    new_var[is.na(fn_var)] <- ""
    dt[[fn.var]] <- new_var
    dtable_prune(dt, rm = info)
}

#-#' --- itself or empty string if NA
id_or_empty <- function(s) ifelse(is.na(s), "", s)

#-#'  -- a variable to a 'footnote'
latex_symbols <- function(n, pre = "\\", suff  = "", start = 1){
    symb <- c("bot", "forall", "flat", "sharp", "top", "S", "bigstar", "Join",
               "clubsuit", "diamondsuit", "spadesuit",  "heartsuit",
               "dagger", "ast", "star", "circ", "ddagger", "bullet")
    greekl <- c("alpha", "beta", "gamma", "delta", "epsilon", "varepsilon",
                "zeta", "eta", "theta", "vartheta", "iota", "kappa", "lambda",
                "mu", "nu", "xi", "pi", "varpi", "rho", "varrho", "sigma",
                "varsigma", "tau", "upsilon", "phi", "varphi", "chi",
                "psi", "omega")
    greeku <- c("Gamma","Delta","Theta","Lambda","Xi","Pi","Sigma",
                "Upsilon","Phi","Psi","Omega")
    S <- c(symb, greekl, greeku)
    N <- length(S)
    if(start<1) stop(paste0("need 1<'start'<", N))
    if( (start-1+n) > N) stop("there are not enough latex symbols")
    paste0(pre, S[(start):(start - 1 + n)], suff)
}
