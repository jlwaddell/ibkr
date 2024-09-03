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

# source("loadFromYahoo.R")
load("./data/data2min.RData")

################# read in the data and preprocess #######################3
# read data

filename <- list.files("./data", pattern = "Stock Train")
data <- as.data.frame(read_excel(paste0("./data/", filename) ))

# data <- data[-(98:100), ]

data$plotNum <- 1:nrow(data)

pdf("./plots/trains.pdf", width = 10, height = 8)
for(plotNum in 1:max(data$plotNum)) {  # plotNum <- 3
	vizTradeAndStrategy(data = data, dataList = dataList, 
			fullDataList = fullDataList,
			plotNum = plotNum, 
			omitTimepoints = c(1:10, 380:390), 
			includeADX = TRUE, 
			dataType = "train")
}
dev.off()


#pdf("trainHours.pdf", width = 10, height = 8)
#for(plotNum in 1:max(data$plotNum)) {  # plotNum <- 3
#	for(jHour in 1:6) {
#	
#		if(jHour == 1) omit <- c(1:30, 90:390)
#		if(jHour == 2) omit <- c(1:90, 150:390)
#		if(jHour == 3) omit <- c(1:150, 210:390)
#		if(jHour == 4) omit <- c(1:210, 270:390)
#		if(jHour == 5) omit <- c(1:270, 330:390)
#		if(jHour == 6) omit <- c(1:330)
#		
#		vizTradeAndStrategy(data = data, dataList = dataList, 
#				plotNum = plotNum, 
#				omitTimepoints = omit, 
#				includeADX = TRUE, 
#				dataType = "train")
#		
#	}
#}
#dev.off()















