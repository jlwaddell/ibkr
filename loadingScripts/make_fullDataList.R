

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

source("./functions/plotFunctions.R")
source("./functions/loadingFunctions.R")
source("./functions/calcFunctions.R")



load("./data/data2min.RData")

fullDataList <- list()
for(i in 1:length(dataList)) {
	
	rawData <- dataList[[i]]
	fullData <- formatFullData(input = rawData)
	fullDataList[[i]] <- fullData
	names(fullDataList)[i] <- names(dataList)[i]
	
}

save(fullDataList, file = "./data/fullDataList.RData")








