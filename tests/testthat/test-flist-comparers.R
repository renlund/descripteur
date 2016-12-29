test_that(desc = "c_rstd works", code = {
    x  <- c(1,5,9, 1,6,11)
    w  <- c(1,1,1, 2,2,2)
    w2 <- c(1,0,1, 1,0,1)
    w3 <- c(1,4,1, 1,4,1)
    gl <- list("a" = rep(c(T, F), each = 3),
               "b" = rep(c(F, T), each = 3))
    expect_equal(c_rstd(x = x, glist = gl),
                (5-6) / sqrt((4^2+5^2)/2))
    expect_equal(c_rstd(x = x, glist = gl, w = w),
    (5-6) / sqrt((4^2+5^2)/2))
    expect_equal(c_rstd(x = x, glist = gl, w = w2),
    (5-6) / sqrt((32+50)/2))
    ## d.sd(x[1:3], w3[1:3])^2
    ## d.sd(x[4:6], w3[4:6])^2
    expect_equal(c_rstd(x = x, glist = gl, w = w3), (5-6) / sqrt((8+12.5)/2))
})

test_that("c_OR works", {
    x = factor(c(1,1, 1,0, 0,0, 0,0, NA), levels = 0:1)
    gl <- list("A" = c(T,T, F,F, T,T, F,F, F),
               "B" = c(F,F, T,T, F,F, T,T, F))
    w1 = as.numeric(x)
    expect_equal(c_OR(x = x, glist = gl), 1/(1/3))
    expect_equal(c_OR(x = x, glist = gl, w = w1), 2/(2/3))
})

test_that("c_bstd works", {
    x = factor(c(1,1, 1,0, 0,0, 0,NA), levels = 0:1)
    gl <- list("A" = c(T,T, F,F, T,T, F,F),
               "B" = c(F,F, T,T, F,F, T,T))
    w1 = as.numeric(x)
    expect_equal(c_bstd(x = x, glist = gl), (1/2-1/3)/sqrt((1/4+2/9)/2))
    expect_equal(c_bstd(x = x, glist = gl), (2/3-1/2)/sqrt((2/9+1/4)/2))
})

test_that("c_pdiff works", {
    x <- letters[c(1,2,2,3, 1,1,1,2,3,3)]
    gl <- list("a"=rep(c(T,F), c(4,6)),
               "b"=rep(c(F,T), c(4,6)))
    w1 <- c(2,1,1,1, 2,2,2,1,1,1)
    expect_equal(c_pdiff(x, glist = gl, useNA=F),
                 c(1/4-1/2, 1/2-1/6,1/4-1/3))
    expect_equal(c_pdiff(x, glist = gl),
                 c(1/4-1/2, 1/2-1/6,1/4-1/3))
    expect_equal(c_pdiff(x, glist = gl, w=w1, useNA=F),
                 c(2/5-6/9, 2/5-1/9, 1/5-2/9))
})

test_that("c_cOR works", {
    x <- letters[c(1,2,2,3, 1,1,1,2,3,3)]
    gl <- list("a"=rep(c(T,F), c(4,6)),
               "b"=rep(c(F,T), c(4,6)))
    w1 <- c(2,1,1,1, 2,2,2,1,1,1)
    expect_equal(c_cOR(x, glist = gl, useNA=F),
                 c(1/3, 5, 2/3))
    expect_equal(c_cOR(x, glist = gl, w = w1, useNA=F),
                 c((2/3)/2, (2/3)/(1/8), (1/4)/(2/7)))
})

test_that("c_cstd works", {
    x <- letters[c(1,2,2,3, 1,1,1,2,3,3)]
    x3 <- x; x3[x3 == "c"] <- "b"
    x2 <- x; x2[x2 == "c"] <- "a"
    x1 <- x; x1[x1 == "b"] <- "a"
    gl <- list("a"=rep(c(T,F), c(4,6)),
               "b"=rep(c(F,T), c(4,6)))
    expect_equal(c_cstd.each(x, glist = gl), c(
                                            -c_bstd(x3, glist = gl),
                                            c_bstd(x2, glist = gl),
                                            c_bstd(x1, glist = gl)
                                            ))
})
