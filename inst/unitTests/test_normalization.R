test_normalization <- function() {
  x <- matrix(1:6,3,2)
  tst <- normalization(x)
  checkEqualsNumeric(tst[,1], tst[,2])
}
