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


source("./functions/plotFunctions.R")
source("./functions/loadingFunctions.R")
source("./functions/calcFunctions.R")

################# read in the data and preprocess #######################3
# read data

filename <- list.files("./data", pattern = "Stock Train")
data <- as.data.frame(read_excel("./data/Stock Train Data.xlsx"))


# reset the plot number 
data$plotNum <- 1:nrow(data)

pdf("trains.pdf", width = 10, height = 8)
for(plotNum in 1:max(data$plotNum)) {  # plotNum <- 3
	vizTradeAndStrategy(data, plotNum = plotNum, 
			omitTimepoints = c(1:10, 380:390), 
			periodicity = "1 minutes", 
			dataType = "train")
}
dev.off()