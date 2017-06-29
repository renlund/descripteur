x <- c(1, NA, 2)
w <- c(1, 2, 4)
test_that("d_n", {
    expect_equal(d_n(x), 2)
    expect_equal(d_n(x, w, weight = 'sample'), 2)
    expect_equal(d_n(x, w, weight = 'case'), 5)
})

test_that("d_length", {
    expect_equal(d_length(x), 3)
})

test_that("d_missing works", {
    expect_equal(d_missing(x), 1)
    expect_equal(d_missing(x, w), 2)
})

test_that("d_missing.perc works", {
    expect_equal(d_missing.perc(x), 100/3)
    expect_equal(d_missing.perc(x, w), 200/7)
})

test_that("d_mean works", {
    expect_equal(d_mean(x), 1.5)
    expect_equal(d_mean(x, w), 9/5)
})

test_that("d_sd works (1)", {
    expect_equal(d_sd(x), sqrt(.5))
    expect_equal(d_sd(x = x, w = w), sqrt(1.6 / 5))
})

test_that("d_sum works", {
    expect_equal(d_sum(x), 3)
    expect_equal(d_sum(x = x, w = w), (1+2*4)*2/5)
    expect_equal(d_sum(x = c(1,1), w = c(2,2)), 2)
})

x <- c(1, NA, 7, 3, 5, 4)
w <- c(3,  1, 1, 1, 1, 1)
y <- c(NA, 2, 4, 1, 3)
v <- c(1,  1, 2, 2, 1)

test_that("d_median works", {
    expect_equal(d_median(x), 4)
    expect_equal(d_median(x = x, w = w), 3)
    expect_equal(d_median(y),    2.5)
    expect_equal(d_median(y, v), 2.5)
})

test_that("d_sum works (2)", {
    expect_equal(d_sum(x), 20)
    expect_equal(d_sum(x = x, w = w), 22*5/7)
    ## d_sum(x[1:3], w = w[1:3])
    ## d_sum(x[4:6], w = w[4:6])
})
######################################################

x <- factor(c(1,1,0,NA,0,0), levels = 0:1)
w <-        c(2,2,1,7, 1,1)
test_that("d_bn works", {
    expect_equal(d_bn(x), 2)
    expect_equal(d_bn(x, w, weight = 'case'), 4)
})

test_that("d_bp works", {
    expect_equal(d_bp(x), 2/5)
    expect_equal(d_bp(na.omit(x)), 2/5)
    expect_equal(d_bp(x, w), 4/7)
    expect_equal(d_bp(na.omit(x), w[!is.na(x)]), 4/7)
})

test_that("d_odds works", {
    expect_equal(d_odds(x), 2/3)
    expect_equal(d_odds(x, w), 4/3)
    expect_equal(d_odds(na.omit(x), w[!is.na(x)]), 4/3)
})
#######################################################

x <- factor(letters[c(1,2,3,NA)])
w <- c(2,3,5,7)
test_that("weighted_p works", {
    expect_equal(weighted_p(x), c(1,1,1)/3)
    expect_equal(weighted_p(x, w), c(2,3,5)/10)
    expect_equal(weighted_p(x, count = TRUE), c(1,1,1))
    expect_equal(weighted_p(x, w, count = TRUE), c(2,3,5))
})

test_that("weighted_tab works", {
    expect_equal(weighted_tab(x), c(1,1,1,1)*100/4)
    expect_equal(weighted_tab(x, w), c(2, 3, 5, 7)*100/17)
    expect_equal(weighted_tab(x, count = TRUE), c(1,1,1,1))
    expect_equal(weighted_tab(x, w, count = TRUE), c(2, 3, 5, 7))
})

test_that("d_percent works", {
    expect_equal(d_percent(x), c(1,1,1)*100/4)
    expect_equal(d_percent(x, useNA = TRUE), c(1,1,1,1)*100/4)
    expect_equal(d_percent(x, useNA = TRUE, w = w), c(2, 3, 5, 7)*100/17)
    expect_equal(d_percent(x = x,  w = w), c(2, 3, 5)*100/17)
})

test_that("d_cp works", {
    expect_equal(d_cp(x, useNA = FALSE), c(1,1,1)/3)
    expect_equal(d_cp(x, useNA =TRUE), c(1,1,1,NA)/3)
    expect_equal(d_cp(x, w=w, useNA = FALSE), c(2,3,5)/10)
    expect_equal(d_cp(x, useNA =TRUE, w=w), c(2,3,5, NA)/10)
})

test_that("d_tsum works", {
    a <- c(1, 2, 3)
    b <- c(0, 1, 0)
    x <- survival::Surv(time = a, event = b)
    expect_equal(d_tsum(x, cens.type = "right"), 6)
    expect_equal(d_esum(x, cens.type = "right"), 1)
    a <- c(1, 2, NA, 4, 5)
    b <- c(0, 1, 1, 0, NA)
    x <- survival::Surv(time = a, event = b)
    expect_equal(d_tsum(x, cens.type = "right"), 12)
    expect_equal(d_esum(x, cens.type = "right"), 2)
    df <- data.frame(x = x)
    expect_error(dtable(df, cens.type = "left"))
})

## test_that("d_compact works", {
##     x <- c(1:5, NA)
##     G <- data.frame(variable = "x", type = "real")
##     expect_equal(d_compact(x, type.guide = G, median = TRUE), "3 (2-4)[1]")
##     expect_equal(d_compact(x, type.guide = G, median = FALSE), "3 (1.6)[1]")
##     x <- c(0,0,0,0,1)
##     G <- data.frame(variable = "x", type = "bnry")
##     expect_equal(d_compact(x, type.guide = G), "1 (20\\%)")
##     x <- rep(LETTERS[1:3], c(6,3,1))
##     G <- data.frame(variable = "x", type = "catg")
##     expect_equal(d_compact(x, type.guide = G),
##                  c("6 (60\\%)", "3 (30\\%)", "1 (10\\%)"))
##     x <- as.Date("2007-03-17") + c(0,367,NA)
##     G <- data.frame(variable = "x", type = "date")
##     expect_equal(d_compact(x, type.guide = G), "2007-03-17/2008-03-18")
##     df <- data.frame(x = 1:20)
##     dtable_guide(df)
##     tg <- dtable_guide(df)
##     cf <- flist(c("a" = "d_compact_info", "b" = "d_compact"))
##     dtable(df)
##     dtable(df, guide = FALSE, desc.flist = cf, type.guide = tg)
## })

test_that("dt_Q works", {
    x <- c(1:5, NA)
    expect_equal(dt_Q(x, useNA = TRUE), "3 (2-4)[1]")
    expect_equal(dt_Q(x, useNA = FALSE), "3 (2-4)")
    expect_equal(dt_Q.info(x), "Numeric variables: median(Q1-Q3)")
    expect_equal(dt_msd(x, useNA = TRUE), "3 (1.6)[1]")
    expect_equal(dt_msd(x), "3 (1.6)")
    x <- c(0,0,0,0,1)
    expect_equal(dt_bcp(x, useNA=FALSE), "1 (20\\%)")
    expect_equal(dt_bcp(x, useNA=TRUE), "1 (20\\%)[0]")
    x <- rep(LETTERS[1:3], c(6,3,1))
    expect_equal(dt_ccp(x, useNA = FALSE),
                 c("6 (60\\%)", "3 (30\\%)", "1 (10\\%)"))
    expect_equal(dt_ccp(x, useNA = TRUE),
                 c("6 (60\\%)[0]", "3 (30\\%)", "1 (10\\%)"))
    x <- as.Date("2007-03-17") + c(0,367,NA)
    expect_equal(dt_date(x, useNA = FALSE), "2007-03-17/2008-03-18")
    expect_equal(dt_date(x, useNA = TRUE), "2007-03-17/2008-03-18[1]")
    x <- survival::Surv(time = c(1, 2, NA, 4, 5), event = c(0, 1, 0, 1, NA))
    expect_equal(dt_rate(x, useNA = FALSE), "0.2")
    expect_equal(dt_rate(x, useNA = TRUE), "0.2 [2]")
})

rm(x, w, y, v)

