# libraries
setwd("C:/Users/Jason/git/ibkr")
options(xts_check_TZ = FALSE)
library(quantmod)
library(calendar)
library(oaColors)
library(lubridate)
library(dplyr)
library(oaPlots)
library(tidyverse)


source("./functions/plotFunctions.R")
source("./functions/loadingFunctions.R")
source("./functions/calcFunctions.R")

# source("loadFromYahoo.R")
#load("data1min.RData")
load("./data/data2min.RData")

################# read in the data and preprocess #######################3
# read data

filename <- list.files("./Data", pattern = "U5155755")
data <- loadTradeData(filename) 

# remove trades which don't fit current trading strategy
remTickers <- c("AAPL", "GME", "AAPL", "MSFT", "NVDA", "QCOM", "CMG", "CXAI", 
		"ASTS", "LQDA", "LUNR")
remDates <- c("2024-08-01", "2024-08-01", "2024-08-02", "2024-08-02", "2024-08-02", 
		"2024-08-12", "2024-08-13", "2024-08-13", "2024-08-16", "2024-08-19", 
		"2024-08-20")

for(i in 1:length(remTickers)) {
	idxRem <- which(data$Symbol == remTickers[i] & data$day == remDates[i])
	if(length(idxRem) > 0)
		data <- data[-idxRem, ]
}

# reset the plot number 
data$plotNum <- as.numeric(as.factor(data$plotNum))

pdf("./plots/selectedTrades.pdf", width = 10, height = 8)
for(plotNum in 1:max(data$plotNum)) {  # plotNum <- 10
	vizTradeAndStrategy(data = data, dataList = dataList, 
			fullDataList = fullDataList,
			plotNum = plotNum, 
			stopLossMult = 2.5, profitTakeMult = 2.5, 
#			omitTimepoints = c(1:5, 191:195), 
			includeADX = TRUE, 
			dataType = "normal")
}
dev.off()



pdf("./plots/singleTrades.pdf", width = 10, height = 8)
for(plotNum in 1:max(data$plotNum)) {  # plotNum <- 11
	vizTradeAndStrategy(data = data, dataList = dataList, 
			fullDataList = fullDataList,
			plotNum = plotNum, 
			stopLossMult = 2.5, profitTakeMult = 2.5, 
			timepointsBefore = 60, ending = "tradeEnd")
}
dev.off()



pdf("./plots/tradeSetups.pdf", width = 10, height = 8)
for(plotNum in 1:max(data$plotNum)) {  # plotNum <- 11
	vizTradeAndStrategy(data = data, dataList = dataList, 
			fullDataList = fullDataList,
			plotNum = plotNum, 
			stopLossMult = 2.5, profitTakeMult = 2.5, 
			timepointsBefore = 60, ending = "tradeStart", 
			includeADX = TRUE)
}
dev.off()


pdf("./plots/progressiveStops.pdf", width = 10, height = 8)
plotNum <- 17
for(numTimepoints in seq(10, 100, by = 2)) {
	omit <- numTimepoints
	vizTradeAndStrategy(data = data, dataList = dataList, 
			fullDataList = fullDataList,
			plotNum = plotNum, 
			stopLossMult = 4, profitTakeMult = 3.5, 
			omitTimepoints = c(numTimepoints:195), 
			includeADX = TRUE, 
			dataType = "normal")
}
dev.off()


# broad stop loss exploration
stopValues <- seq(0.5, 10, by = 0.5)
profitTakes <- seq(2, 14, by = 0.5)
plList <- list()

for(plotNum in 1:max(data$plotNum)) {  # plotNum <- 1
	plList[[plotNum]] <- calcPLStrategy(data, plotNum = plotNum, 
			stopValues = stopValues, profitTakes = profitTakes)
}

sum_matrix <- Reduce(`+`, plList)
round(sum_matrix/length(plList), 4)





# focused stop loss exploration
stopValues <- seq(3, 8, by = 0.5)
profitTakes <- seq(2.5, 4, by = 0.1)
plList <- list()

for(plotNum in 1:max(data$plotNum)) {  # plotNum <- 1
	plList[[plotNum]] <- calcPLStrategy(data, plotNum = plotNum, 
			stopValues = stopValues, profitTakes = profitTakes)
}

sum_matrix <- Reduce(`+`, plList)
propEdge <- round(sum_matrix/length(plList), 4)
propEdge  # proportion profit per trade. 0.01 = 1% edge

std_matrix <- apply(simplify2array(plList), c(1, 2), sd)
se_matrix <- std_matrix / sqrt(length(plList))
propEdge / se_matrix   # z values










#######################
# remove bad entries  #
remTickers <- c("PLTR", "DNUT", "UAA", "HD", "KEY",
		"SBUX", "SE", "CAH", "GOOG", "K",
		"PGR", "HRB", "AMD", "EBS", "MCD",
		"RDFN", "ZIM", "AMD", "COIN", "FN")
remDates <- c("2024-08-06", "2024-08-08", "2024-08-08", "2024-08-12", "2024-08-12", 
		"2024-08-13", "2024-08-13", "2024-08-14", "2024-08-14", "2024-08-14", 
		"2024-08-14", "2024-08-16", "2024-08-19", "2024-08-19", "2024-08-19",
		"2024-08-19", "2024-08-19", "2024-08-20", "2024-08-20", "2024-08-20")
dataNoBadEntries <- data

for(i in 1:length(remTickers)) {
	idxRem <- which(dataNoBadEntries$Symbol == remTickers[i] 
					& dataNoBadEntries$day == remDates[i])
	if(length(idxRem) > 0)
		dataNoBadEntries <- dataNoBadEntries[-idxRem, ]
}

dataNoBadEntries$plotNum <- as.numeric(as.factor(dataNoBadEntries$plotNum))



# focused stop loss exploration
stopValues <- seq(2, 3, by = 0.1)
profitTakes <- seq(2, 3, by = 0.1)
plList <- list()

for(plotNum in 1:max(dataNoBadEntries$plotNum)) {  # plotNum <- 1
	plList[[plotNum]] <- calcPLStrategy(dataNoBadEntries, plotNum = plotNum, 
			stopValues = stopValues, profitTakes = profitTakes)
}

sum_matrix <- Reduce(`+`, plList)
propEdge <- round(sum_matrix/length(plList), 4)
round(propEdge, 4)  # proportion profit per trade. 0.01 = 1% edge

std_matrix <- apply(simplify2array(plList), c(1, 2), sd)
se_matrix <- std_matrix / sqrt(length(plList))
round(propEdge / se_matrix, 2)   # z values


pdf("./plots/selectedGoodTrades.pdf", width = 10, height = 8)
for(plotNum in 1:max(dataNoBadEntries$plotNum)) {  # plotNum <- 1
	vizTradeAndStrategy(data = dataNoBadEntries, dataList = dataList, 
			fullDataList = fullDataList,
			plotNum = plotNum, 
			stopLossMult = 2.5, profitTakeMult = 2.5, 
			omitTimepoints = c(1:5, 191:195), 
			includeADX = TRUE, 
			dataType = "normal")
}
dev.off()
