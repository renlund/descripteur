x <- c(1, NA, 2)
w <- c(1, 2, 4)
test_that("d_n", {
    expect_equal(d_n(x), 2)
    expect_equal(d_n(x, w), 5)
})

test_that("d_length", {
    expect_equal(d_length(x), 3)
})

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

test_that("d_sum works", {
    expect_equal(d_sum(x), 20)
    expect_equal(d_sum(x = x, w = w), 22*5/7)
})

######################################################

x <- factor(c(1,1,0,NA,0,0), levels = 0:1)
w <-        c(2,2,1,7, 1,1)
test_that("d_bn works", {
    expect_equal(d_bn(x), 2)
    expect_equal(d_bn(x, w), 4)
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
})

test_that("weighted_tab works", {
    expect_equal(weighted_tab(x), c(1,1,1,1)*100/4)
    expect_equal(weighted_tab(x, w), c(2, 3, 5, 7)*100/17)
})

test_that("d_percent works", {
    expect_equal(d_percent(x), c(1,1,1)*100/4)
    expect_equal(d_percent(x, useNA = TRUE), c(1,1,1,1)*100/4)
    expect_equal(d_percent(x, useNA = TRUE, w = w), c(2, 3, 5, 7)*100/17)
    expect_equal(d_percent(x = x,  w = w), c(2, 3, 5)*100/17)
})

test_that("d_p.c works", {
    expect_equal(d_p.c(x, useNA = FALSE), c(1,1,1)/3)
    expect_equal(d_p.c(x, useNA =TRUE), c(1,1,1,NA)/3)
    expect_equal(d_p.c(x, w=w, useNA = FALSE), c(2,3,5)/10)
    expect_equal(d_p.c(x, useNA =TRUE, w=w), c(2,3,5, NA)/10)
})
rm(x, w, y, v)
