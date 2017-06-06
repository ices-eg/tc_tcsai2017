# Fitting Models to Data in R

# Colin Millar, modified from Iago Mosqueira and Ernesto Jardim

# This script shows the steps followed to fit a stock-recruitment model to
# in the file 'WBFT_S-R.csv'

# clear workspace before we start
rm(list = ls())


#==============================================================================
# Load and explore data
#==============================================================================

# load data from comma-separated file to data.frame
tuna <- read.csv(file = 'WBFT_S-R.csv', header = TRUE)

# take a look at what we have
head(tuna) # this looks at the first 6 rows
str(tuna) # this lets us inspect what the columns contain

# lets rename some columns because I am lazy and want the code to be
# readable and easier to type
names(tuna)
names(tuna) <- c("yc", "ssb", "rec")
head(tuna)

# to access the diffrent columns use '$' i.e to see the SSB values:
tuna$ssb
# and to see observed recruitment
tuna$rec

# initial plot of SSB vs. recruits
plot(rec ~ ssb,
     data = tuna, # look in tuna for x and y values
     xlab = 'Spawning Stock Biomass (tonnes)',
     ylab = 'Age-1 Recruitment')


# probably better to set x and y limits to start at zero
plot(rec ~ ssb,
     data = tuna, # look in tuna for x and y values
     xlim = c(0, max(ssb)), # set x limits
     ylim = c(0, max(rec)), # set y limits
     xlab = 'Spawning Stock Biomass (tonnes)',
     ylab = 'Age-1 Recruitment')


###############################################################################
# We are now going to demonstrate the same techniques employed in the spreadsheet
# solution to the assignment
###############################################################################

#==============================================================================
# Beverton and holt recruitmet model R=b1*S/(b2+S)
#==============================================================================


#------------------------------------------------------------------------------
# (1) Calculate predicted R for each year
# Rpred = b1 * S / (b2 + S)
#------------------------------------------------------------------------------

# starting values for b1 and b2
b1 <- 1000000
b2 <- 300000

# set up the other variables (i.e. S)
S <- tuna$ssb

Rpred <- b1 * S / (b2 + S)

#------------------------------------------------------------------------------
# (2) Calculate log residuals, Ln(obs/pred)
#------------------------------------------------------------------------------

# assigne observed recruitment
Robs <- tuna$rec

resids <- log(Robs / Rpred) # = log(Robs) - log(Rpred)

# note that in R, log is the natural log:
?log # see the help file for the log function
log(exp(1))
log(exp(10))

#------------------------------------------------------------------------------
# (3) Calculate sum of squared residuals
#------------------------------------------------------------------------------

?sum # see the help file for sum
ssq_resids <- sum(resids^2)

#------------------------------------------------------------------------------
# (4) Minimize sum-of-squares with solver by adjusting b1 and b2
#------------------------------------------------------------------------------

# to do this, we need to set up a function that takes
# b1 and b2 as input, and returns the sums of squared residuals

# in R a function is a collection of steps: i.e.
add <- function(b1, b2) {
  b1 + b2
}
add(1, 2)
# 3

# the sums of squares function is collection of the previous 3 steps:
ssq <- function(b1, b2) {
  # 1. Calculate predicted R for each year
  Rpred <- b1 * S / (b2 + S)
  # 2. Calculate log residuals, Ln(obs/pred)
  resids <- log(Robs / Rpred)
  # 3. Calculate sum of squared residuals
  ssq_resids <- sum(resids^2)

  # return
  ssq_resids
}

# lets test this out:
ssq(b1, b2)
ssq(1000000, 300000)
ssq(1000000, 200000)

# now we need to search over lots of values for b1 and b2 to
# find the minimum.
# There are lots of ways to do this, we will first look at the optim function.
# the help file for optim is:
?optim

ssq_optim <- function(par) {
  b1 <- par[1]
  b2 <- par[2]

  ssq(b1, b2)
}

# use c to combine the starting values into a vector
?c
par0 <- c(1000000, 300000)

# lets test the new ssq funciton
ssq_optim(par0)

# lets run it..
opt <- optim(par0, ssq_optim)

opt


#------------------------------------------------------------------------------
# (5) Plot observed and predicted R
#------------------------------------------------------------------------------

# get the parameter estimates from the optimisation
b1 <- opt$par[1]
b2 <- opt$par[2]

# predict recruitment
Rpred <- b1 * S / (b2 + S)

# plot
plot(Robs ~ S,
     xlim = c(0, max(S)), # set x limits
     ylim = c(0, max(Robs)), # set y limits
     xlab = 'Spawning Stock Biomass (tonnes)',
     ylab = 'Age-1 Recruitment')

# add predictions to the plot
points(Rpred ~ S, col = "red", pch = 2)


#------------------------------------------------------------------------------
# (6) Plot residuals
#------------------------------------------------------------------------------

# calculate residuals
resids <- log(Robs / Rpred)

# plot them
plot(resids ~ S)
# add in a reference line
abline(h = 0, lty = 2)





















###############################################################################
# We are now going to demonstrate the same solution, but taking advantage of
# the tools provided by a programming / scripting language
###############################################################################

#==============================================================================
# Beverton and holt recruitmet model R=b1*S/(b2+S)
#==============================================================================

#------------------------------------------------------------------------------
# (1) Calculate predicted R for each year
# Rpred = b1 * S / (b2 + S)
#------------------------------------------------------------------------------

# this time we will write a function to do this called bevholt
#  to be safe we will also pass in S
#  this way we know for sure wha values of S are being used

bevholt <- function(b, S) {
  b[1] * S / (b[2] + S)
}

# compute R at the starting values for b1 and b2
Rpred <- bevholt(c(1000000, 300000), S = tuna$ssb)

# lets jump to step 4 ...

#------------------------------------------------------------------------------
# (4) Minimize sum-of-squares with solver by adjusting b1 and b2
#------------------------------------------------------------------------------

# now lets modify the ssq function to accept S and Robs,
# and use the function bevholt

# the sums of squares function is collection of the previous 3 steps:
ssq <- function(b, S, Robs) {
  # 1. Calculate predicted R for each year
  Rpred <- bevholt(b, S)
  # 2. Calculate log residuals, Ln(obs/pred)
  resids <- log(Robs / Rpred)
  # 3. Calculate sum of squared residuals
  ssq_resids <- sum(resids^2)

  # return
  ssq_resids
}

# lets test this out:
ssq(c(b1, b2), tuna$ssb, tuna$rec) # what to you notice this time?
ssq(c(1000000, 300000), tuna$ssb, tuna$rec)
ssq(c(1000000, 200000), tuna$ssb, tuna$rec)

# now we need to search over lots of values for b1 and b2 to
# find the minimum.

ssq_optim <- function(par, S, Robs) {
  b <- exp(par)

  ssq(b, S, Robs)
}

# use c to combine the starting values into a vector
par0 <- log(c(1000000, 300000))

# lets test the new ssq funciton
ssq_optim(par0, S = tuna$ssb, Robs = tuna$rec)

# lets run it..
opt <- optim(par0, ssq_optim, S = tuna$ssb, Robs = tuna$rec)

opt


#------------------------------------------------------------------------------
# (5) Plot observed and predicted R
#------------------------------------------------------------------------------

# predict recruitment over the full S range
Spred <- seq(0, max(tuna$ssb), length.out = 100)
Rpred <- bevholt(exp(opt$par), S = Spred)

# plot
plot(rec ~ ssb,
     data = tuna, # pass in data this time
     xlim = c(0, max(S)), # set x limits
     ylim = c(0, max(Robs)), # set y limits
     xlab = 'Spawning Stock Biomass (tonnes)',
     ylab = 'Age-1 Recruitment')

# add predictions to the plot as a line
lines(Rpred ~ Spred, col = "red", pch = 2)











###############################################################################
# We are now going to demonstrate a techniques for calculating confidence
# intervals
###############################################################################

# Bootstrapping is so called because it is like you are acheieving something
# from nothing.
#
# but in fact it is taking advantage of the fact that your samle of data
# contains information about how it varies...
#
# this can be seen from the residuals:

# lets run the fit again
fit <- optim(par0, ssq_optim, S = tuna$ssb, Robs = tuna$rec)

# and calculate the residuals
Rpred <- bevholt(exp(fit$par), tuna$ssb)
resids <- log( tuna$rec / Rpred)

# and plot a histogram
hist(resids, nclass = 20)

# the mean of the residuals is:
mean(resids)

# but is there not error in this?

# resample from this as if the resuduals are random and reclaculate the mean
r_star <- sample(resids,  replace = TRUE)
mean(r_star)

# do it again
r_star <- sample(resids, replace = TRUE)
mean(r_star)

# do it lots of times!
rmean_star <-
  replicate(10000, {
    r_star <- sample(resids, replace = TRUE)
    mean(r_star)
  })

hist(rmean_star)

#------------------------------------------------------------------------------
# so we are able to access the error inherent in the model fit?
#
# And we can propagate this through to the parameter estimates?
#------------------------------------------------------------------------------

# resample from the residuals as if the resuduals are random and reestimate the
# parameters
r_star <- sample(resids, replace = TRUE)
opt <- optim(par0, ssq_optim, S = tuna$ssb, Robs = Rpred + r_star)
opt$par

# do it again
r_star <- sample(resids, replace = TRUE)
opt <- optim(par0, ssq_optim, S = tuna$ssb, Robs = Rpred + r_star)
opt$par


# do it lots of times!
par_star <-
  replicate(10000, {
    r_star <- sample(resids, replace = TRUE)
    opt <- optim(par0, ssq_optim, S = tuna$ssb, Robs = Rpred + r_star,
                 method = "BFGS")
    opt$par
  })

# separate b1 and b2 bootstrap simulations for ease of inspection
b1_star <- exp(par_star[1,])
b2_star <- exp(par_star[2,])

# plot histograms of simulations
hist(b1_star, nclass = 50)
# add confidence intervals
abline(v = quantile(b1_star, c(0.025, 0.975)), col = "red")
quantile(b1_star, c(0.025, 0.975))

# what does the 2D bootstrap simulation look like?
plot(b1_star, b1_star, pch = ".", col = grey(.5, alpha = 0.5))

# a colourful 2d densty plot
image(MASS::kde2d(b1_star, b2_star, n = 400))





## residuals
