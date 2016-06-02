test_that("dtable works", {

    n <- 100
    set.seed(20160216)
    df <- data.frame(
        id = paste0("id", 1001:(1000 + n)),
        r1 = round(rnorm(n, 20, 5)),
        r2 = round(rexp(n, 1/20)),
        c1 = sample(letters[1:5], size = n, replace = TRUE),
        c2 = factor(sample(LETTERS[5:3], size = n, replace = TRUE)),
        b1 = sample(LETTERS[6:7], size = n, replace = TRUE, prob = 2:3),
        b2 = rbinom(n, 1, 0.1),
        b3 = sample(c("No", "Yes"), size = n, replace = TRUE, prob = 1:2),
        b4 = sample(c(TRUE, FALSE), size = n, replace = TRUE),
        d1 = as.Date("2000-01-01") + rpois(n, 365),
        d2 = as.Date(floor(rexp(n, 1/3650)), origin = "1975-01-01"),
        stringsAsFactors = FALSE
    )
    misser <- function(x, m = length(x)){
        p <- floor(runif(1, min = 1, max = m/2))
        x[sample(1:n, size = p, replace = FALSE)] <- NA
        x
    }
    df[2:length(df)] <- lapply(df[2:length(df)], misser)
    df
    (dtb <- dtable_guide(df))
    gl <- make_glist("b1", ref = df)
    gl3 <- list(
        "abacus" = sample(c(T,F), size = n, replace =T),
        "quuz" = sample(c(T,F), size = n, replace =T),
        "k__7" = sample(c(T,F), size = n, replace =T)
        )

})
