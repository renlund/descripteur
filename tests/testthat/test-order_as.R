test_that("'order_as' works", {
    df <- data.frame(
        x = letters[c(3,3, 1, 4, 4, 4, 2)]
    )
    df$control <- paste0(df$x, 1:nrow(df))
    w <- letters[2:5]
    expect_equal(
        order_as(given = df$x, wanted = w, incl.unordered = TRUE),
        c(7,1,2,4,5,6,3)
    )
    expect_equal(
        order_as(given = df$x, wanted = w, incl.unordered = FALSE),
        c(7,1,2,4,5,6)
    )
    ## manual check
    df[order_as(given = df$x, wanted = w, incl.unordered = TRUE), ]
    df[order_as(given = df$x, wanted = w, incl.unordered = FALSE), ]
})
