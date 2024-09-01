
# libraries
setwd("C:/Users/Jason/git/ibkr")
options(xts_check_TZ = FALSE)
library(quantmod)
library(calendar)
library(oaColors)
library(lubridate)
library(dplyr)
library(oaPlots)
library(readxl)
library(tidyquant)

# alpha vantage API key
api_key <- "V7X7XDUO4IOM111P"
av_api_key(api_key)
print(av_api_key())


source("./functions/plotFunctions.R")
source("./functions/loadingFunctions.R")
source("./functions/calcFunctions.R")
source("./functions/getSymbolsAV.R")

ticker <- "EURJPY"

startFromScratch <- FALSE
if(startFromScratch) {
	dataList <- list()
} else {
	load(paste0("./data/data", ticker, ".RData"))
}

periodicity <- "1min"
desiredPeriodicity <- "2min"

monthVec <- paste0(rep(2022:2023, each = 12), "-", 
		c(paste0("0", 1:9), 10:12))



################# load forex data for a given set of months #######################
# read data

for(jMonth in 1:length(monthVec)) { # jMonth <- 1
	month <- monthVec[jMonth]
	
	if(!(month %in% names(dataList))) {
		
		x <- getSymbolsAV(ticker, src="av", 
				api.key = api_key,
				interval = periodicity,
				periodicity = "intraday", 
				month = month)
		
		if(x == ticker) {
#			cat("yay")
			
			# grab the month data and find out which rows correspond to our 'from' day
			monthData <- as.data.frame(get(x))
			
			# fill in missing values
			missingIdx <- which(is.na(dayData[, 1]))
			if(length(missingIdx) > 0) {
				for(jIdx in 1:length(missingIdx)) {
					
					tmp <- dayData
					tmp[missingIdx[jIdx], ] <- tmp[missingIdx[jIdx] - 1, ]
					assign(x,  value =  tmp)
				}
			}
			
			colnames(dayData) <- c("Open", "High", "Low", "Close", "Volume")
			
			if(desiredPeriodicity == "2min")
				dayData <- convert_to_2min(dayData)
			
			dataList[[length(dataList)+1]] <- dayData
			names(dataList)[length(dataList)] <- plotName
		}
		
		
	}
	
}


names(dataList)
length(dataList)




save(dataList, file = paste0("./data/data", ticker, ".RData"))











