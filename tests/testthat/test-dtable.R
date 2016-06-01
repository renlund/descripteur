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

    ## this part is automaticall generated ...ish
expect_equal(
    dtable(data = df, type = 'real', guide = dtb),
    structure(list(variable = structure(1:2, .Label = c("r1", "r2"),
              class = "factor"),
              missing = c(8, 13),
              median = c(19, 13),
                  IQR = c(6, 26)),
              .Names = c("variable", "missing", "median", "IQR"),
              row.names = 1:2, dtable = c("meta", "desc", "desc", "desc"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'real', guide = dtb, glist = gl),
    structure(list(variable = structure(1:2, .Label = c("r1", "r2"),
              class = "factor"),
              missing = c(15, 17),
              median = c(19, 13),
                  IQR = c(5, 33.5),
              missing = c(16, 19),
              median = c(20.5, 13    ),
              IQR = c(6, 22),
              shift = c(-0.3, 0)),
              .Names = c("variable", "missing", "median", "IQR", "missing", "median", "IQR", "shift"),
              class = c("dtable", "data.frame"),
              row.names = 1:2, dtable = c("meta", "desc:F", "desc:F", "desc:F", "desc:G", "desc:G", "desc:G", "comp"))
)
expect_equal(
    dtable(data = df, type = 'real', guide = dtb, glist = gl3, comp = F),
    structure(list(variable = structure(1:2, .Label = c("r1", "r2"),
              class = "factor"),
              missing = c(6, 6),
              median = c(19, 15),
                  IQR = c(6, 28.25),
              missing = c(4, 4),
              median = c(20, 14),
                  IQR = c(6.5, 20),
              missing = c(4, 8),
              median = c(19, 12.5),
                  IQR = c(5.25, 17)),
              .Names = c("variable", "missing", "median", "IQR", "missing", "median", "IQR", "missing", "median", "IQR"),
              row.names = 1:2, dtable = c("meta", "desc:abacus", "desc:abacus", "desc:abacus", "desc:quuz", "desc:quuz", "desc:quuz", "desc:k__7", "desc:k__7", "desc:k__7"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'real', guide = dtb, glist = gl, comp = FALSE),
    structure(list(variable = structure(1:2, .Label = c("r1", "r2"),
              class = "factor"),
              missing = c(15, 17),
              median = c(19, 13),
                  IQR = c(5, 33.5),
              missing = c(16, 19),
              median = c(20.5, 13    ),
              IQR = c(6, 22)),
              .Names = c("variable", "missing", "median", "IQR", "missing", "median", "IQR"),
              row.names = 1:2, dtable = c("meta", "desc:F", "desc:F", "desc:F", "desc:G", "desc:G", "desc:G"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'real', guide = dtb, glist = gl, desc = F),
    structure(list(variable = structure(1:2, .Label = c("r1", "r2"),
              class = "factor"),
              shift = c(-0.3, 0)),
              .Names = c("variable", "shift"),
              row.names = 1:2, dtable = c("meta", "comp"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'bnry', guide = dtb),
    structure(list(variable = structure(1:4, .Label = c("b2", "b1", "b3", "b4"),
              class = "factor"),
              level = structure(1:4, .Label = c("1", "G", "Yes", "TRUE"),
              class = "factor"),
              missing = c(45, 12, 5, 15),
              p = c(0.145454545454545, 0.590909090909091, 0.715789473684211, 0.588235294117647),
              odds = c(0.170212765957447, 1.44444444444444, 2.51851851851852, 1.42857142857143)),
              .Names = c("variable", "level", "missing", "p", "odds"),
              row.names = c(NA, 4L),
              dtable = c("meta", "meta", "desc", "desc", "desc"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'bnry', guide = dtb, glist = gl),
    structure(list(variable = structure(1:4, .Label = c("b2", "b1", "b3", "b4"),
              class = "factor"),
              level = structure(1:4, .Label = c("1", "G", "Yes", "TRUE"),
              class = "factor"),
              missing = c(29, 12, 16, 19),
              p = c(0.105263157894737, 0, 0.6875, 0.517241379310345),
                  odds = c(0.117647058823529, 0, 2.2, 1.07142857142857),
              missing = c(36,     12, 13, 20),
              p = c(0.142857142857143, 1, 0.745098039215686,     0.590909090909091),
              odds = c(0.166666666666667, Inf, 2.92307692307692,     1.44444444444444),
              OR = c(0.705882352941177, 0, 0.752631578947368,     0.741758241758242)),
              .Names = c("variable", "level", "missing", "p", "odds", "missing", "p", "odds", "OR"),
              class = c("dtable", "data.frame"),
              row.names = c(NA, 4L),
              dtable = c("meta", "meta", "desc:F", "desc:F", "desc:F", "desc:G", "desc:G", "desc:G", "comp"))
)
expect_equal(
    dtable(data = df, type = 'bnry', guide = dtb, glist = gl3, comp = F),
    structure(list(variable = structure(1:4, .Label = c("b2", "b1", "b3", "b4"),
              class = "factor"),
              level = structure(1:4, .Label = c("1", "G", "Yes", "TRUE"),
              class = "factor"),
              missing = c(22, 6, 2, 7),
              p = c(0.208333333333333, 0.65, 0.727272727272727, 0.666666666666667),
              odds = c(0.263157894736842, 1.85714285714286, 2.66666666666667, 2),
              missing = c(30, 3, 4, 7),
              p = c(0.142857142857143, 0.583333333333333, 0.702127659574468, 0.522727272727273),
              odds = c(0.166666666666667, 1.4, 2.35714285714286, 1.0952380952381),
              missing = c(24, 4, 2, 5),
              p = c(0.125, 0.545454545454545, 0.695652173913043, 0.581395348837209),
              odds = c(0.142857142857143, 1.2, 2.28571428571429, 1.38888888888889)),
              .Names = c("variable", "level", "missing", "p", "odds", "missing", "p", "odds", "missing", "p", "odds"),
              row.names = c(NA, 4L),
              dtable = c("meta", "meta", "desc:abacus", "desc:abacus", "desc:abacus", "desc:quuz", "desc:quuz", "desc:quuz", "desc:k__7", "desc:k__7", "desc:k__7"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'bnry', guide = dtb, glist = gl, desc =F),
    structure(list(variable = structure(1:4, .Label = c("b2", "b1", "b3", "b4"),
              class = "factor"),
              OR = c(0.705882352941177, 0, 0.752631578947368, 0.741758241758242)),
              .Names = c("variable", "OR"),
              row.names = c(NA, 4L),
              dtable = c("meta", "comp"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'catg', guide = dtb),
    structure(list(variable = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
              .Label = c("c1", "c2"),
              class = "factor"),
              levels = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 6L),
              .Label = c("a", "b", "c", "d", "e", "missing", "C", "D", "E"),
              class = "factor"),
              percent = c(20, 13, 10, 8, 12, 37, 27, 17, 18, 38),
              p = c(0.317460317460317, 0.206349206349206, 0.158730158730159, 0.126984126984127, 0.19047619047619, NA, 0.435483870967742, 0.274193548387097, 0.290322580645161, NA)),
              .Names = c("variable", "levels", "percent", "p"),
              row.names = c(NA, 10L),
              dtable = c("meta", "meta", "desc", "desc"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'catg', guide = dtb, useNA = 'no'),
    structure(list(variable = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L),
              .Label = c("c1", "c2"),
              class = "factor"),
              levels = structure(1:8, .Label = c("a", "b", "c", "d", "e", "C", "D", "E"),
              class = "factor"),
              percent = c(20, 13, 10, 8, 12, 27, 17, 18),
              p = c(0.317460317460317, 0.206349206349206, 0.158730158730159, 0.126984126984127, 0.19047619047619, 0.435483870967742, 0.274193548387097, 0.290322580645161)),
              .Names = c("variable", "levels", "percent", "p"),
              row.names = c(NA, 8L),
              dtable = c("meta", "meta", "desc", "desc"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'catg', guide = dtb, glist = gl),
    structure(list(variable = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
              .Label = c("c1", "c2"),
              class = "factor"),
              levels = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 6L),
              .Label = c("a", "b", "c", "d", "e", "missing", "C", "D", "E"),
              class = "factor"),
              percent = c(16.6666666666667, 6.25, 8.33333333333333, 4.16666666666667, 12.5, 52.0833333333333, 20.8333333333333, 14.5833333333333, 8.33333333333333, 56.25),
                  p = c(0.347826086956522, 0.130434782608696, 0.173913043478261,     0.0869565217391304, 0.260869565217391, NA, 0.476190476190476,     0.333333333333333, 0.19047619047619, NA),
              percent = c(14.0625,     12.5, 6.25, 7.8125, 7.8125, 51.5625, 21.875, 15.625, 20.3125,     42.1875),
              p = c(0.290322580645161, 0.258064516129032, 0.129032258064516,     0.161290322580645, 0.161290322580645, NA, 0.378378378378378,     0.27027027027027, 0.351351351351351, NA),
              diff = c(0.0575035063113604,     -0.127629733520337, 0.0448807854137447, -0.0743338008415147,     0.0995792426367461, NA, 0.0978120978120978, 0.063063063063063,     -0.160875160875161, NA),
              OR = c(1.3037037037037, 0.43125,     1.42105263157895, 0.495238095238095, 1.83529411764706, NA,     1.49350649350649, 1.35, 0.434389140271493, NA)),
              .Names = c("variable", "levels", "percent", "p", "percent", "p", "diff", "OR"),
              class = c("dtable", "data.frame"),
              row.names = c(NA, 10L),
              dtable = c("meta", "meta", "desc:F", "desc:F", "desc:G", "desc:G", "comp", "comp"))
)
expect_equal(
    dtable(data = df, type = 'catg', guide = dtb, glist = gl3, comp = F),
    structure(list(variable = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
              .Label = c("c1", "c2"),
              class = "factor"),
              levels = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 6L),
              .Label = c("a", "b", "c", "d", "e", "missing", "C", "D", "E"),
              class = "factor"),
              percent = c(19.5652173913043, 15.2173913043478, 6.52173913043478, 6.52173913043478, 17.3913043478261, 34.7826086956522, 23.9130434782609, 17.3913043478261, 17.3913043478261, 41.304347826087),
              p = c(0.3, 0.233333333333333, 0.1, 0.1, 0.266666666666667, NA, 0.407407407407407, 0.296296296296296, 0.296296296296296, NA),
              percent = c(17.6470588235294, 15.6862745098039, 9.80392156862745, 5.88235294117647, 11.7647058823529, 39.2156862745098, 29.4117647058824, 21.5686274509804, 15.6862745098039, 33.3333333333333),
              p = c(0.290322580645161, 0.258064516129032, 0.161290322580645, 0.0967741935483871, 0.193548387096774, NA, 0.441176470588235, 0.323529411764706, 0.235294117647059, NA),
              percent = c(12.5, 14.5833333333333, 10.4166666666667, 6.25, 14.5833333333333, 41.6666666666667, 33.3333333333333, 14.5833333333333, 16.6666666666667, 35.4166666666667),
              p = c(0.214285714285714, 0.25, 0.178571428571429, 0.107142857142857, 0.25, NA, 0.516129032258065, 0.225806451612903, 0.258064516129032, NA)),
              .Names = c("variable", "levels", "percent", "p", "percent", "p", "percent", "p"),
              row.names = c(NA, 10L),
              dtable = c("meta", "meta", "desc:abacus", "desc:abacus", "desc:quuz", "desc:quuz", "desc:k__7", "desc:k__7"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'catg', guide = dtb, glist = gl, desc = F),
    structure(list(variable = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
              .Label = c("c1", "c2"),
              class = "factor"),
              levels = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 6L),
              .Label = c("a", "b", "c", "d", "e", "missing", "C", "D", "E"),
              class = "factor"),
              diff = c(0.0575035063113604, -0.127629733520337, 0.0448807854137447, -0.0743338008415147, 0.0995792426367461, NA, 0.0978120978120978, 0.063063063063063, -0.160875160875161, NA),
              OR = c(1.3037037037037, 0.43125, 1.42105263157895, 0.495238095238095, 1.83529411764706, NA, 1.49350649350649, 1.35, 0.434389140271493, NA)),
              .Names = c("variable", "levels", "diff", "OR"),
              row.names = c(NA, 10L),
              dtable = c("meta", "meta", "comp", "comp"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'date', guide = dtb),
    structure(list(variable = structure(1:2, .Label = c("d1", "d2"),
              class = "factor"),
              missing = c(32, 21),
              min = structure(c(11270, 1845),
              class = "Date"),
              max = structure(c(11360, 15515),
              class = "Date")),
              .Names = c("variable", "missing", "min", "max"),
              row.names = 1:2, dtable = c("meta", "desc", "desc", "desc"),
              class = c("dtable", "data.frame"))
)
expect_equal(
    dtable(data = df, type = 'date', guide = dtb, glist = gl),
    structure(list(variable = structure(1:2, .Label = c("d1", "d2"),
              class = "factor"),
              missing = c(26, 19),
              min = structure(c(11270, 2023),
              class = "Date"),
              max = structure(c(11359, 13900),
              class = "Date"),
                  missing = c(27, 24),
              min = structure(c(11292, 1845),
              class = "Date"),
                  max = structure(c(11360, 15515),
              class = "Date"),
              n.overlap = c(65L,     75L)),
              .Names = c("variable", "missing", "min", "max", "missing", "min", "max", "n.overlap"),
              class = c("dtable", "data.frame"),
              row.names = 1:2, dtable = c("meta", "desc:F", "desc:F", "desc:F", "desc:G", "desc:G", "desc:G", "comp"))
)
expect_equal(
    dtable(data = df, type = 'date', guide = dtb, glist = gl3, comp = F),
    structure(list(variable = structure(1:2, .Label = c("d1", "d2"),
              class = "factor"),
              missing = c(19, 10),
              min = structure(c(11270, 1975),
              class = "Date"),
              max = structure(c(11360, 12336),
              class = "Date"),
                  missing = c(20, 11),
              min = structure(c(11270, 1845),
              class = "Date"),
                  max = structure(c(11360, 13900),
              class = "Date"),
              missing = c(15,     10),
              min = structure(c(11270, 1975),
              class = "Date"),
              max = structure(c(11360,     15515),
              class = "Date")),
              .Names = c("variable", "missing", "min", "max", "missing", "min", "max", "missing", "min", "max"),
              row.names = 1:2, dtable = c("meta", "desc:abacus", "desc:abacus", "desc:abacus", "desc:quuz", "desc:quuz", "desc:quuz", "desc:k__7", "desc:k__7", "desc:k__7"),
              class = c("dtable", "data.frame"))
)
    ## end part that is automatically generated ..ish


})
