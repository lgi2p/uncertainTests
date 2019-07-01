#########################################
# Verification des codes par simulation #
#########################################

# Variables and screen cleaning:
graphics.off()   # clear plots
cat("\014")      # clear the console
rm(list = ls())  # clear memory
options(warn=-1) # turns off warnings

# Directory path:
path <- paste0("C:/...")
setwd(path)
source("myLib.R")
library(ggplot2)
library(latex2exp)

#INOPUTS:
nRun          <- 100
mean1         <- 25
mean2         <- 43
sd            <- 10
minNoiseLevel <- 0
maxNoiseLevel <- 15
noiseGran     <- 0.2
testName      <- "perolat" # "desterckeStrauss", "perolat", "vaidyanathan", "hesaminanChachi", "hesamianTaheri", "taheriHesamian"

if (testName == "vaidyanathan"){
  meanPVSameDistr <- c(); meanPVDiffDistr <- c()
} else {
  if(testName == "taheriHesamian"){
    meanPVSameDistrA <- c(); meanPVSameDistrB <- c(); meanPVSameDistrC <- c(); meanPVSameDistrD <- c()
    meanPVDiffDistrA <- c(); meanPVDiffDistrB <- c(); meanPVDiffDistrC <- c(); meanPVDiffDistrD <- c()
  } else {
    meanPVSameDistrlow <- c(); meanPVSameDistrhigh <- c()
    meanPVDiffDistrlow <- c(); meanPVDiffDistrhigh <- c()
  }
}
noiseLevels <- seq(from=minNoiseLevel, to=maxNoiseLevel, by=noiseGran)
for (noiseLevel in noiseLevels){
  cat(noiseLevel, " ")
  if (testName == "vaidyanathan"){
    pvSameDistr1noiseLevel <- c(); pvDiffDistr1noiseLevel <- c()
  } else {
    if (testName == "taheriHesamian"){
      pvSameDistrA1noiseLevel <- c(); pvSameDistrB1noiseLevel <- c(); pvSameDistrC1noiseLevel <- c(); pvSameDistrD1noiseLevel <- c()
      pvDiffDistrA1noiseLevel <- c(); pvDiffDistrB1noiseLevel <- c(); pvDiffDistrC1noiseLevel <- c(); pvDiffDistrD1noiseLevel <- c()
    } else {
      pvSameDistrLow1noiseLevel <- c(); pvSameDistrHigh1noiseLevel <- c()
      pvDiffDistrLow1noiseLevel <- c(); pvDiffDistrHigh1noiseLevel <- c()
    }
  }
  
  for (iRun in 1 : nRun){
    x1       <- rnorm(100, mean = mean1, sd = sd)     
    x1bis    <- rnorm(100, mean = mean1, sd = sd) 
    x2       <- rnorm(100, mean = mean2, sd = sd)
    epsilon1 <- rnorm(100, 0, noiseLevel) # noise vector
    epsilon2 <- rnorm(100, 0, noiseLevel) # generation
    if (testName %in% c("vaidyanathan", "taheriHesamian")){ # trapezes
      df1        <- data.frame(a = x1 - 2 * abs(epsilon1), b = x1 - abs(epsilon1), c = x1 + abs(epsilon1), d = x1 + 2 * abs(epsilon1))
      df1bis     <- data.frame(a = x1bis - 2 * abs(epsilon1), b = x1bis - abs(epsilon1), c = x1bis + abs(epsilon1), d = x1bis + 2 * abs(epsilon1))
      df2        <- data.frame(a = x2 - 2 * abs(epsilon2), b = x2 - abs(epsilon2), c = x2 + abs(epsilon2), d = x2 + 2 * abs(epsilon2))
      if(testName == "vaidyanathan"){
        pvSameDistr <- vaidyanathanTest(df1, df1bis)
        pvDiffDistr <- vaidyanathanTest(df1, df2)
      } else {
        if (testName == "taheriHesamian"){
          pvSameDistr <- taheriHesamianTest(df1, df1bis)
          pvDiffDistr <- taheriHesamianTest(df1, df2)
        }
      }
    } else {
      if (testName %in% c("hesaminanChachi", "hesamianTaheri")){ # triangles
        df1        <- data.frame(a = x1 - abs(epsilon1), b = x1, c = x1 + abs(epsilon1))
        df1bis     <- data.frame(a = x1bis - abs(epsilon1), b = x1bis, c = x1bis + abs(epsilon1))
        df2        <- data.frame(a = x2 - abs(epsilon2), b = x2, c = x2 + abs(epsilon2))
        if (testName == "hesaminanChachi"){
          pvSameDistr <- hesaminanChachiTest(df1, df1bis)
          pvDiffDistr <- hesaminanChachiTest(df1, df2)
        } else {
          if (testName == "hesamianTaheri"){
            pvSameDistr <- hesamianTaheriTest(df1, df1bis)
            pvDiffDistr <- hesamianTaheriTest(df1, df2)
          }
        }
      } else { # intervalles
        df1        <- data.frame(a = x1 - epsilon1, b = x1 + epsilon1) # noise injection 
        df1bis     <- data.frame(a = x1bis - epsilon1, b = x1bis + epsilon1)
        df2        <- data.frame(a = x2 - epsilon2, b = x2 + epsilon2) 
        df1        <- data.frame(a = pmin(df1$a, df1$b), b = pmax(df1$a, df1$b))
        df1bis     <- data.frame(a = pmin(df1bis$a, df1bis$b), b = pmax(df1bis$a, df1bis$b))
        df2        <- data.frame(a = pmin(df2$a, df2$b), b = pmax(df2$a, df2$b))
      }
      if (testName == "desterckeStrauss"){
        pvSameDistr <- desterckeStraussTest(df1, df1bis)
        pvDiffDistr <- desterckeStraussTest(df1, df2)
      } else {
        if (testName == "perolat"){
          pvSameDistr <- perolatTest(df1, df1bis)
          pvDiffDistr <- perolatTest(df1, df2)
        }
      }
    }
    
    if (testName == "vaidyanathan"){
      pvSameDistr1noiseLevel  <- c(pvSameDistr1noiseLevel,  pvSameDistr)
      pvDiffDistr1noiseLevel <- c(pvDiffDistr1noiseLevel, pvDiffDistr)
    } else {
      pvSameDistrLow1noiseLevel  <- c(pvSameDistrLow1noiseLevel,  pvSameDistr$low)
      pvSameDistrHigh1noiseLevel <- c(pvSameDistrHigh1noiseLevel, pvSameDistr$high)
      pvDiffDistrLow1noiseLevel  <- c(pvDiffDistrLow1noiseLevel,  pvDiffDistr$low)
      pvDiffDistrHigh1noiseLevel <- c(pvDiffDistrHigh1noiseLevel, pvDiffDistr$high)
    }
    
  }
  if (testName == "vaidyanathan"){
    meanPVSameDistr  <- c(meanPVSameDistr, mean(pvSameDistr1noiseLevel))
    meanPVDiffDistr  <- c(meanPVDiffDistr, mean(pvDiffDistr1noiseLevel))
  } else {
    meanPVSameDistrlow  <- c(meanPVSameDistrlow, mean(pvSameDistrLow1noiseLevel))
    meanPVSameDistrhigh <- c(meanPVSameDistrhigh, mean(pvSameDistrHigh1noiseLevel))
    meanPVDiffDistrlow  <- c(meanPVDiffDistrlow, mean(pvDiffDistrLow1noiseLevel))
    meanPVDiffDistrhigh <- c(meanPVDiffDistrhigh, mean(pvDiffDistrHigh1noiseLevel))
  }
}
if (testName == "vaidyanathan"){
  df <- data.frame(distribution = c(rep("same", length(noiseLevels)),
                                    rep("different", length(noiseLevels))),
                   pv = c(meanPVSameDistr, meanPVDiffDistr),
                   noiseLevel = rep(noiseLevels, 4))
  p <- ggplot(data=df, aes(x=noiseLevel, y=pv, colour=distribution))
} else {
  df <- data.frame(distribution = c(rep("same", 2 * length(noiseLevels)),
                                    rep("different", 2 * length(noiseLevels))),
                   pvBoundary = rep(c(rep("low", length(noiseLevels)), rep("high", length(noiseLevels))), 2), 
                   pv = c(meanPVSameDistrlow, meanPVSameDistrhigh, 
                          meanPVDiffDistrlow, meanPVDiffDistrhigh),
                   noiseLevel = rep(noiseLevels, 4))
  p <- ggplot(data=df, aes(x=noiseLevel, y=pv, colour=distribution, linetype = pvBoundary))
}

p <- p +  geom_line(size=1.5) 
p <- p + geom_hline(yintercept=0.05, linetype="dotted", color = "red", size=1)
p <- p + scale_y_continuous(breaks = c(0.05, 0.25, 0.5, 0.75, 1), labels = c(TeX("$\\alpha$"), 0.25, 0.5, 0.75, 1))
p + theme(text = element_text(size=30), legend.text = element_text(size=20), axis.text = element_text(size=20),
          legend.title = element_text(size=20), legend.position = c(0.9, 0.3))
