## Read data, prepare matrices
C <- as.matrix(read.table("bluefin_catage.dat", header=TRUE, check.names=FALSE, row.names=1)) / 1000
T <- nrow(C)
A <- ncol(C)
N <- F <- Z <- matrix(NA_real_, nrow=T, ncol=A, dimnames=dimnames(C))

## Assume F in terminal year=0.1, M=0.14
F[T,] <- 0.1
M <- 0.14
Z <- F + M

## Calculate N in terminal year
N <- C*Z / (F*(1-exp(-Z)))

## Calculate N and F up to terminal year,
## assuming F[oldest] = avg(5 preceding ages)
for(t in (T-1):1)
{
  for(a in 1:(A-1))
  {
    N[t,a] <- N[t+1,a+1] * exp(M) + C[t,a] * exp(M/2)
    F[t,a] <- log(N[t,a] / N[t+1,a+1]) - M
  }
  F[t,A] <- mean(F[t,A-(1:5)])
  Z[t,] <- F[t,] + M
  N[t,A] <- C[t,A]*Z[t,A] / (F[t,A]*(1-exp(-Z[t,A])))
}
