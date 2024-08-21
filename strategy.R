# libraries
setwd("C:/Users/Jason/git/ibkr")
library(quantmod)
library(calendar)
library(oaColors)
library(lubridate)
source("functions.R")

################# read in the data and preprocess #######################3
# read data

filename <- list.files("./Data", pattern = "U5155755")
data <- loadTradeData(filename) 

# remove trades which don't fit current trading strategy
remTickers <- c("AAPL", "GME", "AAPL", "MSFT", "NVDA", "QCOM", "CMG", "CXAI", 
		"ASTS", "LQDA")
remDates <- c("2024-08-01", "2024-08-01", "2024-08-02", "2024-08-02", "2024-08-02", 
		"2024-08-12", "2024-08-13", "2024-08-13", "2024-08-16", "2024-08-19")

for(i in 1:length(remTickers)) {
	idxRem <- which(data$Symbol == remTickers[i] & data$day == remDates[i])
	if(length(idxRem) > 0)
		data <- data[-idxRem, ]
}

# reset the plot number 
data$plotNum <- as.numeric(as.factor(data$plotNum))

pdf("selectedTrades.pdf", width = 10, height = 8)
for(plotNum in 1:max(data$plotNum)) {  # plotNum <- 2
	vizTradeAndStrategy(data, plotNum = plotNum, 
			stopLossMult = 4, profitTakeMult = 3.5)
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

