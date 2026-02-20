# naive data
createData <- function(n){
  x <- runif(n, 0, 1)
  y <- runif(n, -1, 1.5)
  plot(x,y)
}
createData(64)

# first try to generate a sin-curve
createSinData <- function(n){
  x <-seq(0,1,by= 1/n)
  y <- (sin(seq(-1,1.5,by= 2.5/n)*-3) + runif(n, -0.25, 0.25))
  print(y)
  plot(x,y)
}
createSinData(100)

# data modelling example 6.3
createSinDataExample <- function(n, sigma = 0.2, plot = TRUE) {
  x <- runif(n, 0, 1)
  eps <- rnorm(n, mean = 0, sd = sigma)
  y <- sin(2 * pi * x) + eps
  
  if (plot) {
    plot(x, y, pch = 5, main = "Noisy sin(2πx)")
  }
  
  data.frame(x = x, y = y)
}
createSinDataExample(200)


       