## test_that("d.ref_comp works", {
##     x <- 1:4
##     gl <- make_glist(factor(1:4))
##     names(gl) <- LETTERS[1:4]
##     fl_ref <- list("id" = identity)
##     dattr(fl_ref) <- "desc"
##     fl_comp <- list("diff" = function(x,glist) x[glist[[1]]] - x[glist[[2]]])
##     dattr(fl_comp) <- "comp"
##     facit <- data.frame(variable = "x", id = 1L, diff = -1L,
##                         diff = -2L, diff = -3L, stringsAsFactors = FALSE,
##                         check.names = FALSE)
##     dattr(facit) <- c("meta", "desc", "comp:B", "comp:C", "comp:D")

##     expect_equal(
##         as.data.frame(d.ref_comp(x, glist = gl, comp.flist = fl_comp,
##                                  ref.flist = fl_ref)),
##         facit
##     )
##     facit <- data.frame(variable = "x", id = 1L, diff = -1L,
##                         diff = -1L, diff = -1L, stringsAsFactors = FALSE,
##                         check.names = FALSE)
##     dattr(facit) <- c("meta", "desc", "comp:B", "comp:C", "comp:D")
##     expect_equal(
##         as.data.frame(d.ref_comp(x, glist = gl, comp.flist = fl_comp,
##                    ref.flist = fl_ref, comp.ref = "previous")),
##         facit
##     )
## })

