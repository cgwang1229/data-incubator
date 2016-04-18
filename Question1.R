## display 10 digits of precision
options(digits=10)

## Assume the stream of T numbers are INTEGERS
## set random seed
set.seed(123)
## my function for Q1
myFunction <- function(N, T, a, b, Niter=1e8){
  ML <- vector()
  for (iter in 1:Niter){
    temp <- sample(1:10, T, replace = TRUE)
    max <- sort(temp, decreasing = TRUE)[1:N]
    last <- temp[(T-N+1):T]   
    M <- prod(max)
    L <- prod(last)
    ML[iter] <- M-L 
  }
  return(list=c("mean"=mean(ML), "sd"=sd(ML),
                "cond. prob."=sum(ML %in% a:b)/sum(ML<=b)))
}

myFunction(N=2, T=8, a=32, b=64)
myFunction(N=4, T=32, a=2048, b=4096)