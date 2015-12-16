test_geneFilter <- function() {
  data(lungData)
  expr <- t(lungData)
  tst <- geneFilter(dat = expr, mean.thred = 6, sd.thred = 4)
  checkEqualsNumeric(tst, c(50,52,160,163,168,174))
}
