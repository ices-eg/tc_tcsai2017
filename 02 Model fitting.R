# Fitting Models to Data in R

# Colin Millar, modified from Iago Mosqueira and Ernesto Jardim

# This script shows the steps followed to fit a stock-recruitment model to
# in the file 'WBFT_S-R.csv'

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
plot(tuna$ssb, tuna$rec,
     xlab = 'Spawning Stock Biomass (tonnes)',
     ylab = 'Age-1 Recruitment')

# A fancier plot
# see:
#   http://www.statmethods.net/advgraphs/parameters.html
plotTunaSR <- function() {
  # for some handy info on plotting parameters
  par(mar = c(4, 5, 1, 1)) # modify margins
  # base plot using formula this time
  plot(rec ~ ssb,
       data = tuna, # look in tuna for x and y values
       xlim = c(0, max(ssb)), # set x limits
       ylim = c(0, max(rec)), # set y limits
       pch = 19, col = "blue", cex = 0.7, # set plotting character (pch), color and size (cex)
       axes = FALSE, ann = FALSE) # don;t plot axes and no annotation

  # x axis
  axis(1, mgp = c(2, 0.4, 0), tck = -0.01)
  # x label with positioning settings (mgp)
  title(xlab = 'Spawning Stock Biomass (tonnes)', font.lab = 2, mgp = c(2, 0.5, 0))

  # y axis with formated labels
  at.y <- pretty(c(0, max(tuna$rec)))
  axis(2, at = at.y, labels = sprintf("%i", at.y), tck = -0.01, las = 2, mgp = c(4, 0.4, 0))
  # y label with positioning settings (mgp)
  title(ylab = 'Age-1 Recruitment', font.lab = 2, mgp = c(3.7, 0.4, 0))

  # but an "L" shaped box rounf the plot
  box(bty = "l")
  # finally reset old par settings
}

plotTunaSR()

###############################################################################
# We are now going to demonstrate the same techniques employed in the spreadsheet
# tutorial for estimating SR models
###############################################################################

#==============================================================================
# Beverton and holt recruitmet model R=b1*S/(b2+S)
#==============================================================================


#------------------------------------------------------------------------------
# (x) using glm with the inverse link and gamma errors
#------------------------------------------------------------------------------

# here we are fitting:
#         R = b1*S / (b2 + S)
# i.e.  1/R = S / b1*S  + b2 / (b1*S)
#           = 1/b2 + b2/b1 * 1/S
#           = a1 + a2 * 1/S
#
# where a1 = 1/b1 and a2 = b2/b1

g1 <- glm(rec ~ I(1/ssb), data = tuna, family = Gamma("inverse"))
g1

plotTunaSR()
points(tuna$ssb, fitted(g1), col = "red")

# plot predictions with CIs
# first get predictions and confidence intervals
tunapred <- data.frame(ssb = seq(0.1, max(tuna$ssb), length.out = 100))
tunapred <- cbind(tunapred, predict(g1, newdata = tunapred, se.fit = TRUE))
tunapred$pred <- 1/tunapred$fit
tunapred$cil <- 1/(tunapred$fit + 2*tunapred$se.fit)
tunapred$ciu <- 1/(tunapred$fit - 2*tunapred$se.fit)

# now plot over the base plot
plotTunaSR()
lines(tunapred$ssb, tunapred$pred, col = "red")
lines(tunapred$ssb, tunapred$cil, col = "red", lty = 2)
lines(tunapred$ssb, tunapred$ciu, col = "red", lty = 2)

# we can transform to get b parameters if we want
a1 <- coef(g1)[1]; a2 <- coef(g1)[2]
b1 <- 1/a1
b2 <- a2 * b1
c(b1, b2)
lines(tunapred$ssb, b1 * tunapred$ssb/(b2 + tunapred$ssb), col = "purple", lwd = 2)

b1 <- 344687.4212
b2 <- 42477.9468
lines(tunapred$ssb, b1 * tunapred$ssb/(b2 + tunapred$ssb), col = "green", lwd = 2)


#------------------------------------------------------------------------------
# (2) grid solution
# create a grid of possible values for the estimated parameter and compute the SS
# for each of them
#------------------------------------------------------------------------------

# first set up functions to calculate the quantities we need
predictR <- function(b) {
  b[1] * tuna$ssb / (b[2] + tuna$ssb)
}

RSS <- function(b) {
  pred <- predictR(b)
  obs <- tuna$rec

  lnResidual <- log(obs / pred)
  # output
  sum(lnResidual^2)
}

# what does this function look like?
# make a grid of b values

b1s <- seq(2, 2e6, length = 100)
b2s <- seq(2, 4e5, length = 100)

RSSgrid <- matrix(NA, length(b1s), length(b2s))
bs <- cbind(b1s[row(RSSgrid)], b2s[col(RSSgrid)])
RSSgrid[] <- apply(bs, 1, RSS)


# find minimum value
xlocmin <- which.min(apply(RSSgrid, 1, min))
ylocmin <- which.min(apply(RSSgrid, 2, min))

# this is our esstimate
b1est <- b1s[xlocmin]
b2est <- b2s[ylocmin]

# check
RSS(c(b1est, b2est))
min(RSSgrid)

plot(b1s, apply(RSSgrid, 1, min), type = "l", ylim = c(min(RSSgrid), 5))

plot(b2s, apply(RSSgrid, 2, min), type = "l", ylim = c(min(RSSgrid), 5))

# now plot the profiled minimum
image(b1s, b2s, RSSgrid, col = rainbow(100), las = 1, zlim = c(min(RSSgrid), 4.8))
contour(b1s, b2s, RSSgrid, add = TRUE, levels = c(4.3, 5, 10, 20, 40, 60, 80, 100))
points(b1est, b2est, pch = 3, col = "blue")
lines(b1s, b2s[apply(RSSgrid, 1, which.min)], col = "white", lwd = 2)





# first set up functions to calculate the quantities we need
predictR <- function(a) {
  1/(a[1] + a[2]/tuna$ssb)
}

RSS <- function(b) {
  pred <- predictR(b)
  obs <- tuna$rec

  lnResidual <- log(obs / pred)
  # output
  sum(lnResidual^2)
}

# what does this function look like?
# make a grid of b values

b1s <- seq(1e-7, 7e-6, length = 100)
b2s <- seq(0.05, 0.2, length = 100)

RSSgrid <- matrix(NA, length(b1s), length(b2s))
bs <- cbind(b1s[row(RSSgrid)], b2s[col(RSSgrid)])
RSSgrid[] <- apply(bs, 1, RSS)


# find minimum value
xlocmin <- which.min(apply(RSSgrid, 1, min))
ylocmin <- which.min(apply(RSSgrid, 2, min))

# this is our esstimate
b1est <- b1s[xlocmin]
b2est <- b2s[ylocmin]

# check
RSS(c(b1est, b2est))
min(RSSgrid)

plot(b1s, apply(RSSgrid, 1, min), type = "l")

plot(b2s, apply(RSSgrid, 2, min), type = "l")

# now plot the profiled minimum
image(b1s, b2s, RSSgrid, col = rainbow(100), las = 1, zlim = c(min(RSSgrid), 5))
contour(b1s, b2s, RSSgrid, add = TRUE, levels = c(4.3, 5, 10, 20, 40, 60, 80, 100))
points(b1est, b2est, pch = 3, col = "blue")














# plot as a contour
z <- RSSgrid
z[z>10] <- 10
x <- b1s
y <- b2s
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue", "green", "red", "yellow") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- rev(jet.colors(nbcol))
# Compute the z-value at the facet centres
zfacet <- (z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz])/4
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
#persp(x, y, z, col = color[facetcol], phi = 30, theta = -30, axes=T, ticktype='detailed')

persp(x, y, z, col = color[facetcol], theta = 140, phi = 20, expand = 0.5,
      shade = 0.15, border = NA, box = FALSE)





llfunc <- function(logb1, logb2, logsigma) {
  # the pdf is https://en.wikipedia.org/wiki/Log-normal_distribution
  # 1/(x * sigma * 2 * pi) * exp(-0.5 * ((log(x) - mu)/sigma)^2)

  x <- tuna$rec
  mu <- log(predictR(exp(b1), exp(b2)))
  sigma <- exp(sigma)
  -1 * sum(-log(x * sigma * 2 * pi) -  0.5 * ((log(x) - mu)/sigma)^2)
}

# test it out
# create vector of possible values for b1
b1s <- seq(1, 1e4, length = 50)



# calculate likelihood for each sigma
lls <- sapply(b1s, function(x) llfunc(x, 300000, 0.2))

# plot SS profile
plot(b1s, lls, type='l', lwd=2, col='red')
  abline(min(lls), 0, lty=2)



# plot residual patterns in time
plot(year, grid[,names(ss)[ss==min(ss)]], type='b', pch=19)
  abline(0, 0, lty=2)

b <- names(ss)[ss == min(ss)]


