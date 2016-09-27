test_that("'factorize_glist' works", {
    glist = list(Foo  = c(T, F, T), Bar = c(F, T, F))
    expect_equal(
        factorize_glist(glist),
        c("Foo", "Bar", "Foo")
    )
    glist = list(Foo = c(T, F, T, F),
                 Bar = c(F, T, F, F),
                 Baz = c(F, F, F, T))
    expect_equal(
        factorize_glist(glist),
        c("Foo", "Bar", "Foo", "Baz")
    )
})
