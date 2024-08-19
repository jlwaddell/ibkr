# libraries
setwd("C:/Users/Jason/.architect/workspace/ibkr")
library(quantmod)
library(calendar)
library(oaColors)
library(lubridate)
source("functions.R")

################# read in the data and preprocess #######################3
# read data

filename <- list.files("./Data", pattern = "U5155755")
data <- loadTradeData(filename) 


# list entries to remove here TODO   remove ones with time before 9:45 also

stopLossMult <- 4
profitTakeMult <- 4

plotNum <- 2

for(jPlot in 1:max(data$plotNum)) {  # jPlot <- 1  # TODO fix
	
	

	
	
	
}











