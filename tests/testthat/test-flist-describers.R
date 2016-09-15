x <- c(1, NA, 2)
w <- c(1, 2, 4)
test_that("d_missing works", {
    expect_equal(d_missing(x), 1)
    expect_equal(d_missing(x, w), 2)
})

test_that("d_mean works", {
    expect_equal(d_mean(x), 1.5)
    expect_equal(d_mean(x, w), 9/5)
})

test_that("d_sd works", {
    expect_equal(d_sd(x), sqrt(.5))
    expect_equal(d_sd(x = x, w = w), sqrt(1.6 / 5))
})
######################################################

x <- factor(c(1,1,0,NA,0,0), levels = 0:1)
w <-        c(2,2,1,7, 1,1)
test_that("d_p.b works", {
    expect_equal(d_p.b(x), 2/5)
    expect_equal(d_p.b(na.omit(x)), 2/5)
    expect_equal(d_p.b(x, w), 4/7)
    expect_equal(d_p.b(na.omit(x), w[!is.na(x)]), 4/7)
})

test_that("d_odds works", {
    expect_equal(d_odds(x), 2/3)
    expect_equal(d_odds(x, w), 4/3)
    expect_equal(d_odds(na.omit(x), w[!is.na(x)]), 4/3)
})
#######################################################

x <- factor(letters[c(1,2,3,NA)])
w <- c(2,3,5,7)
test_that(".weighted_p works", {
    expect_equal(.weighted_p(x), c(1,1,1)/3)
    expect_equal(.weighted_p(x, w), c(2,3,5)/10)
})

test_that(".weighted_tab works", {
    expect_equal(.weighted_tab(x), c(1,1,1,1)*100/4)
    expect_equal(.weighted_tab(x, w), c(2, 3, 5, 7)*100/17)
})

test_that("d_percent works", {
    expect_equal(d_percent(x), c(1,1,1,1)*100/4)
    expect_equal(d_percent(x, useNA = FALSE), c(1,1,1)*100/4)
    expect_equal(d_percent(x, w = w), c(2, 3, 5, 7)*100/17)
    expect_equal(d_percent(x, useNA = FALSE, w), c(2, 3, 5)*100/17)
})

test_that("d_p.c works", {
    expect_equal(d_p.c(x), c(1,1,1,NA)/3)
    expect_equal(d_p.c(x, useNA =FALSE), c(1,1,1)/3)
    expect_equal(d_p.c(x, w=w), c(2,3,5,NA)/10)
    expect_equal(d_p.c(x, useNA =FALSE, w=w), c(2,3,5)/10)
})
rm(x, w)