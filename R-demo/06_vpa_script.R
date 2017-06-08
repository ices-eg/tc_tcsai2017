source("R-demo/06_vpa_function.R")

## Read catch at age, change units
catage <- read.table("R-demo/bluefin_catage.dat", header=TRUE, check.names=FALSE, row.names=1)
catage <- catage / 1000

## Run model
model <- vpa(catage, 0.14, 0.1, 5)

## View results
par(mfrow=c(2,2))

## 1 Population
round(model$N)
Year <- as.integer(rownames(model$N))
plot(apply(model$N, 2, median), ylim=c(0,120), yaxs="i", type="l", lty=3,
     main="Population in 2013 (points) vs.\n median population (line)", xlab="Age", ylab="Individuals")
points(c(tail(model$N, 1)))

## 2 Recruitment
barplot(model$N[,1], ylab="Individuals at age 1", main="Recruitment")

## 3 Selectivity
round(model$F, 2)
plot(colMeans(model$F)/max(colMeans(model$F)), ylim=c(0,1.05), yaxs="i", type="l",
     main="Selectivity", xlab="Age", ylab="Average F at age")
segments(c(10,14), c(0.3,0.3), c(10,14), c(0.45,0.45), lty=3)

## 4 Fbar
plot(Year, model$F[,15], ylim=c(0,0.12), yaxs="i", main="Fbar (10-14)", ylab="Average F at ages 10-14", type="l")
