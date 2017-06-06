# SP_Steve - Biomass dynamics (aka Surplus Production)
# This script shows the steps followed to fit an biomass dynamic model to albacore

# Copyright 2011-12 Iago Mosqueira & Ernesto Jardim, EC JRC
#                   with additons from Colin Millar, ICES (2107)

#==============================================================================
# Load Albacore data from Polacheck, 1993.
#==============================================================================
alb <- read.table('R-demo/albacore.dat', sep='\t', header=TRUE)

#==============================================================================
# Auxiliary functions
#==============================================================================

#------------------------------------------------------------------------------
# getBiomass
#   This function computes biomass from catch considering a Schaefer surplus
#   production model
#------------------------------------------------------------------------------

getBiomass <- function(r, K, b0, catch) {
  # initialise biomass time series
  biomass <- numeric(length(catch))

  # set first year biomass to b0
  biomass[1] <- b0

  # loop over the remaining time steps, calculating the biomass from the
  # previous years biomass, growth and catches
  for(y in seq_along(biomass)[-1]) {
    biomass[y] <-
      max(
        biomass[y-1] + biomass[y-1] * r - (r/K) * biomass[y-1]^2 - catch[y-1],
        0.001
        )
  }

  biomass
}



#------------------------------------------------------------------------------
# surplus.sc
#   This function applies a Schaefer surplus production model and returns the log SS2
#   Estimated parameters are r, K, q (renamed Q for safety) and b0 (virgin biomass).
#------------------------------------------------------------------------------

surplus.sc <- function(params, catch, index){
  # separate out mode parameters
  r <- params[1]
  K <- params[2]
  b0 <- params[3]
  Q <- params[4]

  # get the biomass time series implied by the model paramters
  biomass <- getBiomass(r = r, K = K, b0 = b0, catch = catch)

  # calculate CPUE implied by model parameters
  cpue <- Q * biomass

  # calculate residuals: log observed index - log predicted index (cpue)
  resids <- log(index) -  log(cpue)

  # calulate sums of squared residuals
  ssq_resids <- sum(resids^2)

  # return
  ssq_resids
}


#==============================================================================
# Modeling
#==============================================================================

#------------------------------------------------------------------------------
# We call optim(), using the 'L-BFGS-B' methods, that allows bounds to be defined.
#   starting values: r=0.5, K = max(catch) * 10, B0 = max(catch) * 10, q = 0.25.
#   bounds: 1e-8 > r < 1, max(catch) > K < Inf, max(catch) > Q < Inf, 1e-8 < q > 1e5
#------------------------------------------------------------------------------
# note:
#  r <- params[1]
#  K <- params[2]
#  b0 <- params[3]
#  Q <- params[4]
par0 <- c(0.5, 200, 100, 0.5)

# test starting values
surplus.sc(par0, index = alb$index, catch = alb$catch)

res <-
  optim(par0, surplus.sc,
        method = "L-BFGS-B",
        lower = c(1e-8, max(alb$catch), max(alb$catch), 1e-8),
        upper = c(1, Inf, Inf, 1e5),
        control = list(trace=1, parscale=c(0.5, max(alb$catch)*10, max(alb$catch)*10, 0.25)),
        index = alb$index,
        catch = alb$catch)

# results
r <- res$par[1]
K <- res$par[2]
b0 <- res$par[3]
Q <- res$par[4]

biomass <- getBiomass(r, K, b0, alb$catch)

#------------------------------------------------------------------------------
# diagnostics
#------------------------------------------------------------------------------

# plot predicted CPUE
plot(alb$year, log(alb$index), pch=19,
     ylim = range(log(alb$index), log(Q*biomass)),
     ylab = "", xlab = "Year",
     main = "Predicted CPUE",
     las = 1) # make y axis labels horizontal
lines(alb$year, log(Q*biomass))

# calculate the harvest rate (F)
effort <- alb$catch/ alb$index
harvest <- Q*effort

# we can calculate theoretical reference points
Fmsy <- r/2
Bmsy <- K/2
MSY <- Fmsy * Bmsy

# plot fishing mortality and show Fmsy
plot(alb$year, harvest, type='b', pch=19,
     ylim = c(0, max(harvest)),
     las = 1)
abline(Fmsy, 0, lty=2)

# plot catch vs. MSY
plot(alb$year, alb$catch, type='b', pch=19,
     ylim = c(0, max(alb$catch)),
     las = 1)
abline(MSY, 0, lty=2)

# inspect residuals
resid <- log(alb$index/(Q*biomass))

acf(resid)

#==============================================================================
# Bootstrap - compute confidence intervals
#==============================================================================

# number of iterations
iter <- 1000

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
for(i in 1:iter) {
  if (i %% 10 == 0) cat(rep(" ", 40), "\r------ iter: ", i, "of", iter, "------")
  params[i,] <-
    optim(par0, surplus.sc,
          method="L-BFGS-B",
          lower = c(1e-8, max(alb$catch), max(alb$catch), 1e-8),
          upper = c(1, Inf, Inf, 1e5),
          control = list(trace=0, parscale=c(0.5, max(alb$catch)*10, max(alb$catch)*10, 0.25)),
          index = boot[,i],
          catch = alb$catch
          )$par

  # and calculate biomasses
  bioms[i,] <- getBiomass(params[i,1],params[i,2],params[i,3],alb$catch)
}

# plot boostrap results
with(alb, {
  plot(year, log(index), pch=19)
  lines(year, apply(log(Q*bioms), 2, quantile, 0.025), lty=2)
  lines(year, apply(log(Q*bioms), 2, median), lwd=2)
  lines(year, apply(log(Q*bioms), 2, quantile, 0.975), lty=2)
  lines(year, log(Q*biomass), col=2)
})

# B/Bmsy
BoBmsy <- apply(bioms,2,function(x)x/(params[,"K"]/2))
boxplot(BoBmsy)
abline(1,0, lty=2, col="red")

# projection
newcatch <- c(alb$catch, rep(13.2, 10))
newyears <- c(alb$year, 1:10 + tail(alb$year,1))
newbioms <- apply(params, 1, function(x) getBiomass(x[1], x[2], x[3], newcatch))
#sum(newbioms[,33]/(params[,"K"]/2) > 1)/1000
matplot(newyears,
        newbioms, type = "l", lty = 1, col = grey(0.5),
        ylim = c(0, 300),
        xlab = "Year", ylab = "Biomass", las = 1,
        main = "Projected biomass with boostrap error")
abline(v = tail(alb$year,1), lty = 2)


