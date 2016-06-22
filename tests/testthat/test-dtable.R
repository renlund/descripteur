test_that("dtable works", {

    foo <- list("type" = "real", "mean" = mean)
    dattr(foo) <- c("meta", "desc")
    opts_desc$set("describe_real" = foo)
    df <- data.frame(
        id = 1:4,
        g = LETTERS[c(1,1,2,2)],
        r1 = 0:3 + 0.5,
        r2 = seq(3.2, 3.8, len=4)
    )
    guide <- dtable_guide(df, elim.set = "id")
    guide$type[guide$variable %in% c("r1", "r2")] <- "real"
    dt <- dtable(data = df, type = "real", guide = guide)
    expect_equal(dattr(dt), c("meta", "meta", "desc"))
    expect_equal(as.character(dt$variable), c("r1", "r2"))
    expect_equal(as.character(dt$type), c("real", "real"))
    expect_equal(dt$mean, c(2.0, 3.5))
    dt <- dtable(data = df, type = "real", guide = guide,
                 glist = "g", comp = FALSE)
    expect_equal(dattr(dt), c("meta", "meta", "desc:A", "desc:B"))
    expect_equal(names(dt), c("variable", "type", "mean", "mean"))
    i <- which(names(dt) == "mean")
    expect_equal(dt[[i[1]]], c(1.0, 3.3))
    expect_equal(dt[[i[2]]], c(3.0, 3.7))

})
