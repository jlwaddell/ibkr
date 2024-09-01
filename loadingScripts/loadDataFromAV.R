
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

startFromScratch <- FALSE
if(startFromScratch) {
	dataList <- list()
} else {
	load("./data/data2min.RData")
}

periodicity <- "1min"
desiredPeriodicity <- "2min"
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
		missingIdx <- which(is.na(get(x)[, 1]))
		if(length(missingIdx) > 0) {
			for(jIdx in 1:length(missingIdx)) {
				
				tmp <- get(x)
				tmp[missingIdx[jIdx], ] <- tmp[missingIdx[jIdx] - 1, ]
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




################# read in train data #######################
# read data

filename <- list.files("./data", pattern = "Stock Train")
data <- as.data.frame(read_excel(paste0("./data/", filename)))
data$plotNum <- 1:nrow(data)

for(plotNum in 1:max(data$plotNum)) { # plotNum <- 1
	tmpData <- data[which(data$plotNum == plotNum), ]
	ticker <- tmpData$Ticker[1]
	from <- tmpData$Date[1]
	from <- ymd(from) + duration(0, "days")
	to <- ymd(from) + duration(1, "days")
	
	plotName <- paste0(ticker,  " ", from)
	if(!(plotName %in% names(dataList))) {
		
		x <- getSymbolsAV(ticker, src="av", 
				api.key = api_key,
				interval = periodicity,
				periodicity = "intraday", 
				month = substr(from, 1, 7))
		
		if(x == ticker) {
#			cat("yay")
			
			# grab the month data and find out which rows correspond to our 'from' day
			monthData <- as.data.frame(get(x))
			dateTimes <- unlist(strsplit(rownames(monthData), " "))
			days <- dateTimes[which((1:length(dateTimes) %% 2) == 1)]
			times <- dateTimes[which((1:length(dateTimes) %% 2) == 0)]
			matchingDayIdx <- which(days == from)
			
			# subset to the desired day
			dayData <- monthData[matchingDayIdx, ]
			dateTimes <- unlist(strsplit(rownames(dayData), " "))
			times <- dateTimes[which((1:length(dateTimes) %% 2) == 0)]
			
			# subset to 9:30 - 16:00 (regular trading hours)
			time_vector_posix <- as.POSIXct(times, format = "%H:%M:%S", tz = "UTC")
			start_time <- as.POSIXct("09:30:00", format = "%H:%M:%S", tz = "UTC")
			end_time <- as.POSIXct("15:59:00", format = "%H:%M:%S", tz = "UTC")
			filtered_times <- times[time_vector_posix >= start_time & time_vector_posix <= end_time]
			dayData <- dayData[which(times %in% filtered_times), ]
			
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




save(dataList, file = "./data/data2min.RData")











