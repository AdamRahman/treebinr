#' @import methods
#' @export treebinr
treebinr <- setClass("treebinr", slots = c("points", "counts", "bins", "tree"))

#' @export bin
bin <- setClass("bin", slots = c("boundary", "contents", "measure", "index", "info"))
