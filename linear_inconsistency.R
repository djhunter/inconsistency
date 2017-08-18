set.seed(1917)
m <- 500
n <- 200
A1 <- rnorm(m,mean=2,sd=1)
B1 <- rnorm(n,mean=6,sd=1)
# plot(A,rep(1,m),xlim=c(0,8))
# points(B,rep(1.1,n))

inconsistency1 <- function(A,B) {
  return(sum(outer(A,B,function(x,y) ifelse(x>y, x-y, 0)))/((mean(B)-mean(A))*length(A)*length(B)))
}

inconsistency2 <- function(A,B) {
  pairs <- expand.grid(A,B)
  diffs <- pairs[,1]-pairs[,2]
  Iab <- 1/(length(A)*length(B))*sum(diffs[diffs>0])
  return(Iab/(mean(B)-mean(A)))
}

cat("inconsistency1 = ", inconsistency1(A1,B1), "\n")
cat("inconsistency2 = ", inconsistency2(A1,B1), "\n")

print(system.time(inconsistency1(A1,B1)))
print(system.time(inconsistency2(A1,B1)))

