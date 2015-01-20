pause <- function(x) {
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time()
}