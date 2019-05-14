#Design Matrix for Truncated Basis Functions
truncated.power.design.matrix <- function(x){
  n <- length(x)
  z <- outer(x, x, FUN = ">")*outer(x, x, "-")
  z[,n] <- rep(1,n)
  return(z)
}
