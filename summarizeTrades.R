

# libraries
setwd("C:/Users/Jason/git/ibkr")
library(quantmod)
library(calendar)
library(oaColors)
library(lubridate)
source("functions.R")


# read data
filename <- list.files("./Data", pattern = "U5155755")
data <- loadTradeData(filename, removeColumns = FALSE) 


tradeClosers <- which(data$Realized.P.L != 0)

closeData <- data[tradeClosers, ]
rownames(closeData) <- 1:nrow(closeData)
closeData$plPerc <- rep(0, nrow(data))
closeData$plPerc <- 100 * (closeData$Realized.P.L / abs(closeData$Basis))

closeData$Quantity <- -1 * as.numeric(closeData$Quantity)
closeData$Basis <- abs(closeData$Basis)

closeData[, c("Symbol", "day", "time", "Quantity", "Realized.P.L", "plPerc", "Basis")]
mean(closeData$plPerc)

weightedPerc <- sum(closeData$plPerc * closeData$Basis) / sum(closeData$Basis)
weightedPerc




# look at subset
# tmp <- closeData[14:nrow(closeData), ]
# sum(tmp$plPerc * tmp$Basis) / sum(tmp$Basis)