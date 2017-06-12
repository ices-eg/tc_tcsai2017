source("09_sca_function.R")

C <- as.matrix(read.table("nscod_catage.dat", header=TRUE, check.names=FALSE, row.names=1))
I <- as.matrix(read.table("nscod_survey.dat", header=TRUE, check.names=FALSE, row.names=1))
M <- as.matrix(read.table("nscod_natmort.dat", header=TRUE, check.names=FALSE, row.names=1))
data <- list(C=C, I=I, M=M)

logNa <- rep(8, ncol(C))
logNt <- rep(8, nrow(C))
logFa <- rep(0, ncol(C)-1)
logFt <- rep(0, nrow(C))
logQ <- rep(-5, ncol(I))
par <- c(logNa=logNa, logNt=logNt, logFa=logFa, logFt=logFt, logQ=logQ)

################################################################################

sca(par, data, full=TRUE)
sca(par, data)

optim(f=sca, par=par, data=data)
optim(f=sca, par=par, data=data, method="BFGS")
optim(f=sca, par=par, data=data, method="BFGS", control=list(maxit=1000))

## Excel solver: -289.9066
