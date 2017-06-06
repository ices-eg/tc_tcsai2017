# SP_Steve - Biomass dynamics (aka Surplus Production)
# This script shows the steps followed to fit an biomass dynamic model to albacore

# Copyright 2011-12 Iago Mosqueira & Ernesto Jardim, EC JRC

#==============================================================================
# Load Albacore data from Polacheck, 1993.
#==============================================================================
alb <- read.table('R-demo/albacore.dat', sep='\t', header=TRUE)
attach(alb)

#==============================================================================
# Auxiliary functions
#==============================================================================

#------------------------------------------------------------------------------
# sp.sc
#   This function applies a Schaefer surplus production model and returns the log SS2
#   Estimated parameters are r, K, q (renamed Q for safety) and b0 (virgin biomass).
#------------------------------------------------------------------------------

sp.sc <- function(params, index=index){
  r <- params[1]
  K <- params[2]
  b0 <- params[3]
  Q <- params[4]

  biomass <- rep(b0, length(catch))

  for(y in seq(2, length(catch))){
    biomass[y] <- max(biomass[y-1] + biomass[y-1] * r - (r/K) * biomass[y-1]^2 - catch[y-1], 1e-4)
  }
  return(sum(log(index/(Q*biomass))^2))
}

#------------------------------------------------------------------------------
# getBiomass
#   This function computes biomass from catch considering a Schaefer surplus
#   production model
#------------------------------------------------------------------------------

getBiomass <- function(r, K, b0, catch){
  biomass <- rep(b0, length(catch))

  for(y in seq(2, length(catch))){
    biomass[y] <- max(biomass[y-1] + biomass[y-1] * r - (r/K) * biomass[y-1]^2 - catch[y-1], 1e-4)
  }
  return(biomass)
}


#==============================================================================
# Modeling
#==============================================================================

#------------------------------------------------------------------------------
# We call optim(), using the 'L-BFGS-B' methods, that allows bounds to be defined.
#   starting values: r=0.5, K = max(catch) * 10, B0 = max(catch) * 10, q = 0.25.
#   bounds: 1e-8 > r < 1, max(catch) > K < Inf, max(catch) > Q < Inf, 1e-8 < q > 1e5
#------------------------------------------------------------------------------

res <- optim(c(0.5, max(catch)*10, max(catch)*10, 0.25), sp.sc,
  method="L-BFGS-B",
  lower=c(1e-8, max(catch), max(catch), 1e-8),
  upper=c(1, Inf, Inf, 1e5),
  control=list(trace=1, parscale=c(0.5, max(catch)*10, max(catch)*10, 0.25)),
  index=index)

# results
r <- res$par[1]
K <- res$par[2]
b0 <- res$par[3]
Q <- res$par[4]

biomass <- getBiomass(r, K, b0, catch)

#------------------------------------------------------------------------------
# diagnostics
#------------------------------------------------------------------------------

# plot CPUE fit
plot(year, log(index), pch=19)
lines(year, log(Q*biomass))

effort <- catch/index
harvest <- Q*effort

Fmsy <- r/2
Bmsy <- K/2
MSY <- Fmsy * Bmsy

# plot fishing mortality
plot(year, harvest, type='b', pch=19)
  abline(Fmsy, 0, lty=2)

# plot catch vs. MSY
plot(year, catch, type='b', pch=19)
  abline(MSY, 0, lty=2)

resid <- log(index/(Q*biomass))

#==============================================================================
# Bootstrap - compute confidence intervals
#==============================================================================

# number of iterations
iter <- 100

# matrix for bootstraped residuals
boot <- matrix(NA, nrow=iter, ncol=length(resid))
for(i in 1:iter)
  # fill up each row by resampling with replacement
  boot[i,] <- sample(resid, replace=TRUE)

# calculate indices as exp(x) + Q * biomass
boot <- apply(boot, 1, function(x) exp(x) + Q * biomass)

# matrix for estimated parameters
params <- matrix(NA, ncol=4, nrow=iter,
  dimnames=list(iter=1:iter, params=c('r', 'K', 'B0', 'q')))

# matrix for biomass
bioms <- matrix(NA, ncol=23, nrow=iter,
  dimnames=list(iter=1:iter, year=1:23))

# fit models with bootstraped indices
for(i in 1:iter)
{
  cat("------ iter: ", i, "------\n")
  params[i,] <- optim(c(0.5, max(catch)*10, max(catch)*10, 0.25), sp.sc,
  method="L-BFGS-B", lower=c(1e-8, max(catch), max(catch), 1e-8),
  upper=c(1, Inf, Inf, 1e5), control=list(trace=1, parscale=c(0.5, max(catch)*10,
  max(catch)*10, 0.25)), index=boot[,i])$par

  # and calculate biomasses
  bioms[i,] <- getBiomass(params[i,1],params[i,2],params[i,3],catch)
}

# plot boostrap results
plot(year, log(index), pch=19)
lines(year, apply(log(Q*bioms), 2, quantile, 0.025), lty=2)
lines(year, apply(log(Q*bioms), 2, median), lwd=2)
lines(year, apply(log(Q*bioms), 2, quantile, 0.975), lty=2)
lines(year, log(Q*biomass), col=2)

# B/Bmsy
BoBmsy <- apply(bioms,2,function(x)x/(params[,"K"]/2))
boxplot(BoBmsy)
abline(1,0, lty=2, col="red")

# projection
newcatch <- c(catch, rep(13.2,10))
newbioms <- apply(params, 1, function(x) getBiomass(x[1],x[2],x[3],newcatch))
newbioms <- t(newbioms)
sum(newbioms[,33]/(params[,"K"]/2) > 1)/1000
boxplot(t(newbioms), ylim=c(0, 500))

