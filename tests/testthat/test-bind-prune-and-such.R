test_that("dtable_rbind works", {
    df1 <- data.frame(x = 1, y = 1, x = 2,
                      check.names = FALSE)
    class(df1) <- c("dtable", "data.frame")
    dattr(df1) <- rep("foo", 3)
    df2 <- data.frame(x = 2, y = 2, x = 3,
                     check.names = FALSE)
    dattr(df2) <- rep("foo", 3)
    class(df2) <- c("dtable", "data.frame")
    df <- data.frame(x = c(1,2), y = c(1,2), x = c(2,3),
                      check.names = FALSE)
    class(df) <- c("dtable", "data.frame")
    dattr(df) <- rep("foo", 3)
    expect_equal(
        dtable_rbind(x = df1, y = df2),
        df
    )
})

test_that("dtable_cbind", {
    df1 <- data.frame(variable = 1, y = 1, z = 2,
                      check.names = FALSE)
    class(df1) <- c("dtable", "data.frame")
    dattr(df1) <- c("meta", "HY", "HY")
    df2 <- data.frame(variable = 1, y = 2, u = 3,
                     check.names = FALSE)
    dattr(df2) <- c("meta", "HY", "meta")
    class(df2) <- c("dtable", "data.frame")
    expect_equal(
        dtable_cbind(x = NULL, y = df1),
        structure(list(variable = 1, y = 1, z = 2),
                  .Names = c("variable", "y", "z"),
                  class = c("dtable", "data.frame"),
                  row.names = c(NA, -1L),
                  dtable = c("meta", "HY", "HY"))
    )
    expect_equal(
        dtable_cbind(x = NULL, y = df1, groups = "A"),
        structure(list(variable = 1, y = 1, z = 2),
                  .Names = c("variable", "y", "z"),
                  class = c("dtable", "data.frame"),
                  row.names = c(NA, -1L),
                  dtable = c("meta", "HY:A", "HY:A"))
        )
    expect_equal(
        dtable_cbind(x = df1, y = df2, groups = letters[1:2]),
        structure(list(variable = 1, u = 3, y = 1, z = 2, y = 2),
                  .Names = c("variable", "u", "y", "z", "y"),
                  class = c("dtable", "data.frame"),
                  row.names = 1L,
                  dtable = c("meta", "meta", "HY:a", "HY:a", "HY:b"))
    )
    df1 <- data.frame(variable = c("x", "x"), level = 1:2, Foo = 3:4)
    class(df1) <- c("dtable", "data.frame")
    dattr(df1) <- c("meta", "meta", "HY")
    df2 <- data.frame(variable = c("x"), level = 1, Foo = 5)
    class(df2) <- c("dtable", "data.frame")
    dattr(df2) <- c("meta", "meta", "HY")
    expect_equal(
        dtable_cbind(x = df1, y = df2, groups = letters[1:2]),
        structure(list(variable = c("x", "x"),
                       levels = 1:2,
                       Foo = 3:4,
                       Foo = c(5, NA)),
                  .Names = c("variable", "level", "Foo", "Foo"),
                  class = c("dtable", "data.frame"),
                  row.names = c(NA, -2L),
                  dtable = c("meta", "meta", "HY:a", "HY:b"))
    )
    expect_equal(
        dtable_cbind(x = df2, y = df1, groups = letters[1:2]),
        structure(list(variable = c("x", "x"),
                       level = c(1, 2),
                       Foo = c(5, NA),
                       Foo = 3:4),
                  .Names = c("variable", "level", "Foo", "Foo"),
                  class = c("dtable", "data.frame"),
                  row.names = c(NA, -2L),
                  dtable = c("meta", "meta", "HY:a", "HY:b"))
    )
})

## test_that("dtable_order works", {})

test_that("dtable_prune works",{
    df <- data.frame(x = 1, y = 1, x = 2,
                     check.names = FALSE)
    dattr(df) <- rep("foo", 3)
    expect_equal(
        dtable_prune(df, 1),
        structure(list(y = 1, x = 2), .Names = c("y", "x"),
                  class = "data.frame",
                  row.names = c(NA, -1L),
                  dtable = c("foo", "foo"))
    )
    expect_equal(
       dtable_prune(x = df, rm = "x"),
        structure(list(y = 1), .Names = c("y"),
                  class = "data.frame",
                  row.names = c(NA, -1L),
                  dtable = c("foo"))
    )
    expect_equal(
        dtable_prune(x = df, keep = "x"),
        structure(list(x = 1, x = 2),
                  .Names = c("x", "x"),
                  class = "data.frame",
                  row.names = c(NA, -1L),
                  dtable = c("foo", "foo"))
    )
    expect_equal(
        dtable_prune(x = df, keep = "y"),
        structure(list(y = 1),
                  .Names = "y",
                  class = "data.frame",
                  row.names = c(NA, -1L),
                  dtable = "foo")
    )
    expect_equal(
        dtable_prune(df, rm = "x", info = TRUE),
        structure(list(y = 1),
                  .Names = "y",
                  class = "data.frame",
                  row.names = 1L,
                  dtable = "foo",
                  info = c("1", "2"))
    )
})
