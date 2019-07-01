######################################
# Exemple de modelisation incertaine #
######################################

# Variables and screen cleaning:
graphics.off()   # clear plots
cat("\014")      # clear the console
rm(list = ls())  # clear memory
options(warn=-1) # turns off warnings

# Directory path:
path <- paste0("C:/...")
setwd(path)
source("myLib.R")

# Exemple de modélisation incertaine:
# x1 <- runif(5, min = 30, max = 100)
# x2 <- x1 + abs(rnorm(length(x1), mean = 0, sd = 20))
x1    <- c(55, 90, 75)
x2    <- c(95, 92, 90)
rho   <- 0.7
delta <- abs(x1-x2)
sigma <- sd(delta)
K     <- 5 * sigma
beta  <- K/(rho+delta+1)
a     <- x1 - beta
d     <- x2 + beta

digits <- 1
intervals      <- paste0("[", round(a, digits), ", ", round(d, digits), "]")
possibilities  <- paste0("(", round(a, digits),  ", ", round(x1, digits), ", ", 
                         round(x2, digits), ", ", round(d, digits), ")")
beliefFunction <- paste0("[m({", round(x1, digits), "})=" , round(rho/2, digits+1), "),",
                          "m({",round(x2, digits), "})=", round(rho/2, digits+1), ", ",
                          "m(", intervals, ")=", round(1-rho, digits+1), "]")
uncertainData  <- data.frame(x1 = x1, x2 = x2, delta = delta, beta = beta, intervals = intervals, 
                             possibilities = possibilities, beliefFunction = beliefFunction)
print.data.frame(uncertainData, digits = digits+2)

preciseData <- as.data.frame(cbind(x1, x2))
dfInterval  <- as.data.frame(cbind(uncertainData$x1 - uncertainData$beta, uncertainData$x2 + uncertainData$beta))
dfTrapeze   <- as.data.frame(cbind(uncertainData$x1 - uncertainData$beta, uncertainData$x1, 
                                   uncertainData$x2, uncertainData$x2 + uncertainData$beta))
desterckeStraussTest(dfInterval, dfInterval+5)

Fmin1 <- min(df1[, 1], df1[, 2])
Fmax1 <- max(df1[, 1], df1[, 2])
Fmin2 <- min(df2[, 1], df2[, 2])
Fmax2 <- max(df2[, 1], df2[, 2])
