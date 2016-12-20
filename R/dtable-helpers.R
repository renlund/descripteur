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

# - # create data set to test things on
dtable_data_example <- function(n = 100, seed = 20161207){
    set.seed(seed)
    df <- data.frame(
        id = paste0("id", 1001:(1000 + n)),
        r1 = round(stats::rnorm(n, 20, 5)),
        r2 = round(stats::rexp(n, 1/20)),
        c1 = sample(letters[1:5], size = n, replace = TRUE),
        c2 = factor(sample(LETTERS[5:3], size = n, replace = TRUE)),
        b1 = sample(LETTERS[6:7], size = n, replace = TRUE, prob = 2:3),
        b2 = stats::rbinom(n, 1, 0.1),
        b3 = sample(c("No", "Yes"), size = n, replace = TRUE, prob = 1:2),
        b4 = sample(c(TRUE, FALSE), size = n, replace = TRUE),
        d1 = as.Date("2000-01-01") + stats::rpois(n, 365),
        d2 = as.Date(floor(stats::rexp(n, 1/3650)), origin = "1975-01-01"),
        stringsAsFactors = FALSE
    )
    misser <- function(x, m = length(x)){
        p <- floor(stats::runif(1, min = 1, max = m/2))
        x[sample(1:n, size = p, replace = FALSE)] <- NA
        x
    }
    df[2:length(df)] <- lapply(df[2:length(df)], misser)
    df$vikt <- 0.5 + stats::rbinom(n, 2, 0.4)
    df
}
