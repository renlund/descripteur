## test_that(".weighter works", {
##     x = 1:5
##     v1 = rep(1, 5)
##     v2 = 2.7*v1
##     v3 = c(4,2,1,2,4)
##     v4 = c(0,1,1,1,0)
##     v5 = c(-1,1,2,1,-1)
##     expect_equal(weighted.mean(x, v1), mean(.weighter(x, v1)))
##     expect_equal(weighted.mean(x, v2), mean(.weighter(x, v2)))
##     expect_equal(weighted.mean(x, v3), mean(.weighter(x, v3)))
##     expect_equal(weighted.mean(x, v4), mean(.weighter(x, v4)))
##     expect_equal(weighted.mean(x, v5), mean(.weighter(x, v5)))
##     x[3] <- NA
##     expect_equal(weighted.mean(x, v1, na.rm = TRUE),mean(.weighter(x, v1)))
##     expect_equal(weighted.mean(x, v2, na.rm = TRUE),mean(.weighter(x, v2)))
##     expect_equal(weighted.mean(x, v3, na.rm = TRUE),mean(.weighter(x, v3)))
##     expect_equal(weighted.mean(x, v4, na.rm = TRUE),mean(.weighter(x, v4)))
##     expect_error(mean(.weighter(x, v5)))
##     x[3] <- 3
##     expect_equal(sd(x), sd(.weighter(x,v1)))
##     expect_equal(sd(x), sd(.weighter(x,v2)))
##     expect_equal(sd(x[2:4]), sd(.weighter(x,v4)))
##     x[3] <- NA
##     expect_equal(sd(x, na.rm = TRUE), sd(.weighter(x,v1)))
##     expect_equal(sd(x, na.rm = TRUE), sd(.weighter(x,v2)))
##     expect_equal(sd(x[2:4], na.rm = TRUE), sd(.weighter(x,v4)))
## })

test_that(desc = "d.std.r works", code = {
    x  <- c(1,5,9, 1,6,11)
    w  <- c(1,1,1, 2,2,2)
    w2 <- c(1,0,1, 1,0,1)
    w3 <- c(1,4,1, 1,4,1)
    gl <- list("a" = rep(c(T, F), each = 3),
               "b" = rep(c(F, T), each = 3))
    expect_equal(d.std.r(x = x, glist = gl),
                (5-6) / sqrt((4^2+5^2)/2))
    expect_equal(d.std.r(x = x, glist = gl, w = w),
    (5-6) / sqrt((4^2+5^2)/2))
    expect_equal(d.std.r(x = x, glist = gl, w = w2),
    (5-6) / sqrt((32+50)/2))
    ## d.sd(x[1:3], w3[1:3])^2
    ## d.sd(x[4:6], w3[4:6])^2
    expect_equal(d.std.r(x = x, glist = gl, w = w3), (5-6) / sqrt((8+12.5)/2))
})

test_that("d.OR works", {
    x = factor(c(1,1, 1,0, 0,0, 0,0, NA), levels = 0:1)
    gl <- list("A" = c(T,T, F,F, T,T, F,F, F),
               "B" = c(F,F, T,T, F,F, T,T, F))
    w1 = as.numeric(x)
    expect_equal(d.OR(x = x, glist = gl), 1/(1/3))
    expect_equal(d.OR(x = x, glist = gl, w = w1), 2/(2/3))
})

test_that("d.std.b works", {
    x = factor(c(1,1, 1,0, 0,0, 0,NA), levels = 0:1)
    gl <- list("A" = c(T,T, F,F, T,T, F,F),
               "B" = c(F,F, T,T, F,F, T,T))
    w1 = as.numeric(x)
    expect_equal(d.std.b(x = x, glist = gl), (1/2-1/3)/sqrt((1/4+2/9)/2))
    expect_equal(d.std.b(x = x, glist = gl), (2/3-1/2)/sqrt((2/9+1/4)/2))
})

test_that("d.cc_diff works", {
    x <- letters[c(1,2,2,3, 1,1,1,2,3,3)]
    gl <- list("a"=rep(c(T,F), c(4,6)),
               "b"=rep(c(F,T), c(4,6)))
    w1 <- c(2,1,1,1, 2,2,2,1,1,1)
    expect_equal(d.cc_diff(x, glist = gl, useNA=F),
                 c(1/4-1/2, 1/2-1/6,1/4-1/3))
    expect_equal(d.cc_diff(x, glist = gl),
                 c(1/4-1/2, 1/2-1/6,1/4-1/3))
    expect_equal(d.cc_diff(x, glist = gl, w=w1, useNA=F),
                 c(2/5-6/9, 2/5-1/9, 1/5-2/9))
})

test_that("d.cc_OR works", {
    x <- letters[c(1,2,2,3, 1,1,1,2,3,3)]
    gl <- list("a"=rep(c(T,F), c(4,6)),
               "b"=rep(c(F,T), c(4,6)))
    w1 <- c(2,1,1,1, 2,2,2,1,1,1)
    expect_equal(d.cc_OR(x, glist = gl, useNA=F),
                 c(1/3, 5, 2/3))
    expect_equal(d.cc_OR(x, glist = gl, w = w1, useNA=F),
                 c((2/3)/2, (2/3)/(1/8), (1/4)/(2/7)))
})
