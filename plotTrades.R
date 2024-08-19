
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


# save plot to .pdf
pdf("trades.pdf", width = 10, height = 8)
for(jPlot in 1:max(data$plotNum)) {  # jPlot <- 2
	vizTrade(data, plotNum = jPlot, 
			atrMultiplier = 4, showATR = TRUE)
}
dev.off()














