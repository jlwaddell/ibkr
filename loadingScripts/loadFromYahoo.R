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

functionFiles <- list.files("./functions")
for(iFile in 1:length(functionFiles)) {
	source(paste0("./functions/", functionFiles[iFile]))
}

startFromScratch <- FALSE
if(startFromScratch) {
	dataList <- list()
} else {
	load("./data/fullDataList.RData")
	load("./data/data2min.RData")
}

periodicity <- "2 minutes" # TODO think about this
# note: yahoo finance data only goes back 60 days


################# read in the trade data and preprocess #######################3
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


for(plotNum in 1:max(data$plotNum)) { # plotNum <- 1
	tmpData <- data[which(data$plotNum == plotNum), ]
	ticker <- tmpData$Symbol[1]
	from <- tmpData$day[1]
	to <- ymd(from) + duration(1, "days")
	
	plotName <- paste0(ticker,  " ", from)
	if(!(plotName %in% names(dataList))) {
		
		x <- getSymbols(ticker, src="yahoo", 
				periodicity = periodicity, 
				from = from, to = to)
		
		# fill in missing values # TODO improve
		idx <- which(is.na(get(x)[, 1]))
		if(length(idx) > 0) {
			for(jIdx in 1:length(idx)) {
				
				tmp <- get(x)
				tmp[idx[jIdx], ] <- tmp[idx[jIdx] - 1, ]
				assign(x,  value =  tmp)
			}
		}
		
		if(ticker == "LOW")
			colnames(LOW) <- c("x.Open", "x.High", "x.Low", "x.Close", "x.Volume")
		
		rawData <- get(x)
		rawData <- rawData[-nrow(rawData), ]
		dataList[[length(dataList)+1]] <- rawData
		names(dataList)[length(dataList)] <- plotName
		
		fullData <- formatFullData(input = rawData)
		fullDataList[[length(fullDataList)+1]] <- fullData
		names(fullDataList)[length(fullDataList)] <- plotName
	}
	
}




################# read in train data #######################
# read data

filename <- list.files("./data", pattern = "Stock Train")
data <- as.data.frame(read_excel(paste0("./data/", filename)))

data <- data[-which(as.Date(data$Date) %in% 
						c(as.Date("2024-07-26"), as.Date("2024-07-11"), 
								as.Date("2024-07-12"))),
		]
data$plotNum <- 1:nrow(data)

for(plotNum in 1:max(data$plotNum)) { # plotNum <- 1
	tmpData <- data[which(data$plotNum == plotNum), ]
	ticker <- tmpData$Ticker[1]
	from <- tmpData$Date[1]
	from <- ymd(from) + duration(0, "days")
	to <- ymd(from) + duration(1, "days")
	
	plotName <- paste0(ticker,  " ", from)
	if(!(plotName %in% names(dataList))) {
		
		x <- tryCatch(getSymbols(ticker, src="yahoo", 
				periodicity = periodicity, 
				from = from, to = to), error = function(e) {
			# Catch block: handle the error
			return(e$message)  # Return the error message
		})

		if(x == ticker) {
			cat("yay")
			# fill in missing values # TODO improve
			idx <- which(is.na(get(x)[, 1]))
			if(length(idx) > 0) {
				for(jIdx in 1:length(idx)) {
					
					tmp <- get(x)
					tmp[idx[jIdx], ] <- tmp[idx[jIdx] - 1, ]
					assign(x,  value =  tmp)
				}
			}
			
			if(ticker == "LOW")
				colnames(LOW) <- c("x.Open", "x.High", "x.Low", "x.Close", "x.Volume")
			
			rawData <- get(x)
			rawData <- rawData[-nrow(rawData), ]
			dataList[[length(dataList)+1]] <- rawData
			names(dataList)[length(dataList)] <- plotName
		}
		
		
	}
	
}



#save(dataList, file = "./data/data2min.RData")
#save(fullDataList, file = "./data/fullDataList.RData")






