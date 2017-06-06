# Biological Production
# Shows how to run the examples used during the lecture
# about biological production

# Copyright 2011-12 Iago Mosqueira & Ernesto Jardim, EC JRC

library(lattice)

#==============================================================================
# MORTALITY 
#==============================================================================

# initial population
pop <- rep(NA, 15)

# selectivity, full from age 3
sel <- c(0, 0, rep(1, 13))

# Initial recruits
pop[1] <- 1000

# Natural mortality for all ages
M <- 0.2

# Overall F
F <- 0.3

# N_a+1 = N_a * e ^ -(F * P_a + M)
pop[2] <- pop[1] * exp(-(F * sel[1] + M))
pop[3] <- pop[2] * exp(-(F * sel[2] + M))
pop[4] <- pop[3] * exp(-(F * sel[3] + M))
pop[5] <- pop[4] * exp(-(F * sel[4] + M))
pop[6] <- pop[5] * exp(-(F * sel[5] + M))
pop[7] <- pop[6] * exp(-(F * sel[6] + M))
#...

# possible to use a loop
for(a in 2:15)
{
  pop[a] <- pop[a-1] * exp(-(F * sel[a-1] + M))
}

# or alternatively vector arithmetic
pop[-1] <- pop[-15]*exp(-(F*sel[-15]+M))

# How many survive to age 15?
pop[15]

# What is the proportional survival (and mortality) at each age?
for(a in 2:15)
  print(pop[a] / pop[a-1])

pop[2:15] / pop[1:14]

# Plot abundance at age (bar chart)
barplot(pop, names.arg=1:15)

# regression of Ln abundance on age
lm(log(pop) ~ age, data.frame(pop=pop, age=1:15))

#==============================================================================
# GROWTH
#==============================================================================

# vonBertallanfy parameters
Winf <- 10
k <- 0.5
t0 <- -0.1
b <- 3

# weight at age vector
wei <- rep(NA, 15)

# W_t = W_inf * (1 - exp(-k*(t-t_0))) ^ 3
for(a in 1:15)
  wei[a] <- Winf * (1- exp(-k*(a-t0)))^3

wei <- Winf * (1- exp(-k*((1:15)-t0)))^3

# Plot weight by age
plot(1:15, wei, pch=19)
  abline(Winf, 0, lty=2)

# Biomass at age
bio <- pop * wei

# Total biomass
totbio <- sum(pop * wei)

# Plot biomass at age
plot(1:15, pop*wei, pch=19, type='b')

#==============================================================================
# RECRUITMENT
#==============================================================================

# population matrix
popn <- matrix(NA, nrow=25, ncol=15, dimnames=list(year=1:25, age=1:15), byrow=TRUE)
# first year
popn[1,] <- pop

# B&H parameters
b1 <- 1000
b2 <- 1000

# Go over years
for(y in 2:25)
{
  # ssb
  ssb <- sum(popn[y-1, 3:15] * wei[3:15])

  # recruitment
  popn[y,1] <- (b1 * ssb) / (b2 + ssb)
  
  # other ages
  for(a in 2:15)
    popn[y,a] <- popn[y-1, a-1] * exp(-(F * sel[a-1] + M))
}

# recruitment
plot(1:25, popn[,1], ylim=c(0, 1000))

# ssb
ssb <- popn[,3:15] %*% wei[3:15]
plot(1:25, ssb, pch=19, type='b', ylim=c(0, max(ssb)))

# what about the effect of F ?

# Feff
Feff <- function(F){

	# population matrix
	popn <- matrix(NA, nrow=25, ncol=15, dimnames=list(year=1:25, age=1:15), byrow=TRUE)
	
  # first year
	popn[1,] <- pop

	# B&H parameters
	b1 <- 1000
	b2 <- 1000

	# Go over years
	for(y in 2:25)
	{
	  # ssb
	  ssb <- sum(popn[y-1, 3:15] * wei[3:15])

	  # recruitment
	  popn[y,1] <- (b1 * ssb) / (b2 + ssb)
	  
	  # other ages
	  for(a in 2:15)
	    popn[y,a] <- popn[y-1, a-1] * exp(-(F * sel[a-1] + M))
	}
	data.frame(year=1:25, F=F, rec=popn[,1], ssb=ssb)
}


Fs <- seq(0,3,0.1)
Feff.res <- lapply(split(Fs,Fs), function(x){Feff(x)})
Feff.res <- do.call("rbind", Feff.res)

xyplot(rec~year, groups=F, data=Feff.res, type=c("g","l"), auto.key=list(space="right"))

#==============================================================================
# RECRUITMENT VARIABILITY
#==============================================================================

popv <- popn
v <- 1000

plot(1:25, ssb, ylim=c(0,max(ssb)*2))

for (i in 1:50)
{

  for(y in 2:25)
  {
    # recruitment
    popv[y,1] <- (b1 * ssb[y-1]) / (b2 + ssb[y-1]) + (runif(1) - 0.5) * v

    for(a in 2:15)
      popv[y,a] <- 
        popv[y-1,a-1] * exp(- (F*sel[a-1] + M))
  
    # SSB
    ssb[y] <- sum(wei[3:15] * popv[y, 3:15])
  
  lines(1:25, ssb, lty=1, col='red')
  }
}

