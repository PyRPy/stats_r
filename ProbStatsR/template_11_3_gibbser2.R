# Introduction to Mathematical Statistics
# Example 11.3.2. 
gibbser2 <- function(alpha,m,n){
  x0 = 1
  yc = rep(0,m+n)
  xc = c(x0,rep(0,m-1+n))
  for(i in 2:(m+n)){yc[i] = rgamma(1,alpha+xc[i-1],2)
                xc[i] = rpois(1,yc[i])}
  y1=yc[1:m]
  y2=yc[(m+1):(m+n)]
  x1=xc[1:m]
  x2=xc[(m+1):(m+n)]
  list(y1 = y1,y2=y2,x1=x1,x2=x2)
}

res <- gibbser2(10, 3000, 6000)
mean(res$y2)
var(res$y2)
hist(res$y2)

mean(res$x2)
var(res$x2)
hist(res$x2)

