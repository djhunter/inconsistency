set.seed(1917)
m <- 3000
n <- 3000
A <- rnorm(m,mean=2,sd=1)
B <- rnorm(n,mean=6,sd=1)
plot(A,rep(1,m),xlim=c(0,8))
points(B,rep(1.1,n))

pairs <- expand.grid(A,B)
diffs <- pairs[,1]-pairs[,2]

Iab <- 1/(m*n)*sum(diffs[diffs>0])
cat("inconsistency = ", Iab/(mean(B)-mean(A)))