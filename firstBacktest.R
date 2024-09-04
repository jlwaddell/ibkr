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

load("./data/data2min.RData")
load("./data/fullDataList.RData")

functionFiles <- list.files("./functions")
for(iFile in 1:length(functionFiles)) {
	source(paste0("./functions/", functionFiles[iFile]))
}


stopLossMult = 3.1
profitTakeMult = 5
r2Thresh <- 0.92   # .9
chopThresh <- 0.52   # 0.382
distanceFromTrendlineThreshold <- 1
prevDistFromTrendlineThresholdMult <- 2.25

prevDistFromTrendlineThreshold <- distanceFromTrendlineThreshold * 
		prevDistFromTrendlineThresholdMult # 2

stopLossType = "fixed"
breakevenTriggerMult = 1
setBreakeven <- FALSE
lookbackWindow <- 20
minimumTimepoint <- 21

plVec <- numeric()
k <- 1

# TODO
# walk through the plots manually starting with #1, AMD
#                                                 LNG 2024-08-09
#                                                 NKE 2024-08-13

# maybe look for trade confirmation. (to prevent immediate reversals)
# does it make sense to sell after trendline broken  (that's another backtest)
# I'm not sure Rsquared and Choppiness are ideal. Maybe find something that looks for avg distance from support line or something

pdf("./plots/backtest.pdf", width = 10, height = 8)
for(i in 1:length(dataList)) {   # i <- 1
#for(i in 1:5) {
	
	cat(i)
	fullData <- fullDataList[[i]]
	
	# find the streaks and take the 2nd true
	tfVec <- (fullData$choppinessBest < chopThresh) & (fullData$rsquaredBest > r2Thresh)
	tfVec[which(is.na(tfVec))] <- FALSE
  
	matchedIdxVec <- which(tfVec == TRUE)
	if(length(matchedIdxVec) > 0)
		matchedIdxVec <- matchedIdxVec[matchedIdxVec >= minimumTimepoint]
	
	while(jMatch <= length(matchedIdxVec)) {  # jMatch <- 1
		
		matchedIdx <- matchedIdxVec[jMatch]
		makeTrade <- TRUE
		
		# create fake data
		ticker <- strsplit(names(dataList)[i], " ")[[1]][1]
		date <- strsplit(names(dataList)[i], " ")[[1]][2]
		time <- fullData$time[matchedIdx]
		
		
		# BOT
		if(     
				fullData$stillMonotonic[matchedIdx] & 
				(fullData$Close[matchedIdx] > fullData$Close[matchedIdx - lookbackWindow]) & 
				!(is.na(fullData$slope[matchedIdx])) 
				) {
			
			# don't make the trade if the current Close is too far from the trendline (or under the trendline)
			if(fullData$distanceAboveTrendline[matchedIdx] > distanceFromTrendlineThreshold | 
					fullData$distanceAboveTrendline[matchedIdx] < 0)
				makeTrade <- FALSE
			
			# don't make the trade if the previous Close values were too far from the trendline
			if(fullData$precedingDistMax[matchedIdx] > prevDistFromTrendlineThreshold | 
					fullData$precedingDistMin[matchedIdx] < -0.5)
				makeTrade <- FALSE
			
			
			
			if(makeTrade) {
				
				if(stopLossType == "fixed") {
					pl <- compute_trade_result_fixed(fullData = fullData, matchedIdx = matchedIdx,
							stopLossMult = stopLossMult,
							profitTakeMult = profitTakeMult,
							action = "BOT")
					
					if(setBreakeven) {
						breakevenHappens <- checkBreakeven(fullData = fullData, matchedIdx = matchedIdx, 
								stopLossMult = stopLossMult,
								profitTakeMult = profitTakeMult,
								breakevenTriggerMult = breakevenTriggerMult,
								action = "BOT") 
						if(breakevenHappens)
							pl <- 0
					}
				} else if(stopLossType == "trailing") {
					pl <- compute_trade_result_trailing(fullData = fullData, matchedIdx = matchedIdx,
							stopLossMult = stopLossMult,
							profitTakeMult = profitTakeMult,
							action = "BOT")
				}
				
			}
			
			
		} else if(
				fullData$stillMonotonic[matchedIdx] & 
				(fullData$Close[matchedIdx] < fullData$Close[matchedIdx - lookbackWindow]) & 
				!(is.na(fullData$slope[matchedIdx]))   # trendline exists
				){  # SLD
			
			makeTrade <- TRUE
			quantity <- -1
			
			# don't make the trade if the current Close is too far from the trendline (or under the trendline)
			if(fullData$distanceAboveTrendline[matchedIdx] < (-1 * distanceFromTrendlineThreshold) |
					fullData$distanceAboveTrendline[matchedIdx] > 0)
				makeTrade <- FALSE
			
			# don't make the trade if the previous Close values were too far from the trendline
			if(fullData$precedingDistMin[matchedIdx] < (-1 * prevDistFromTrendlineThreshold) | 
					fullData$precedingDistMax[matchedIdx] > 0.5)
				makeTrade <- FALSE
			
			
			if(makeTrade) {
				
				if(stopLossType == "fixed") {
					pl <- compute_trade_result_fixed(fullData = fullData, 
							matchedIdx = matchedIdx,
							stopLossMult = stopLossMult,
							profitTakeMult = profitTakeMult,
							action = "SLD")
					
					if(setBreakeven) {
						breakevenHappens <- checkBreakeven(fullData = fullData,
								matchedIdx = matchedIdx, 
								stopLossMult = stopLossMult,
								profitTakeMult = profitTakeMult,
								breakevenTriggerMult = breakevenTriggerMult,
								action = "SLD") 
						if(breakevenHappens)
							pl <- 0
					}
				} else if(stopLossType == "trailing") {
					pl <- compute_trade_result_trailing(fullData = fullData, matchedIdx = matchedIdx,
							stopLossMult = stopLossMult,
							profitTakeMult = profitTakeMult,
							action = "SLD")
					
				}
				
			}			
		}  # close SLD bracket  
		
		if(makeTrade) {
			fakeData <- data.frame(Symbol = ticker, Quantity = quantity, 
					T..Price = fullData$Close[matchedIdx], day = date, time = time, plotNum = 1)
			vizTradeAndStrategy(data = fakeData, 
					dataList = dataList, 
					fullDataList = fullDataList, 
					plotNum = 1, 
					stopLossMult = stopLossMult, profitTakeMult = profitTakeMult, 
					omitTimepoints = NULL, 
					includeADX = TRUE, 
					dataType = "normal", 
#					ending = "tradeStart",
					titleAddendum = paste0(", pl = ", round(pl, 3))
			)
			
			nextJMatch <- which(matchedIdxVec > matchedIdx + 20)
			if(length(nextJMatch) == 0) {
				jMatch <- length(matchedIdxVec) + 1
			} else 
				jMatch <- nextJMatch[1]
		} else 
			jMatch <- jMatch + 1
		
		
	}
	
	
	
}
dev.off()


mean(plVec)
sd(plVec)
length(plVec)

mean(plVec) / (sd(plVec) / sqrt(length(plVec)))

# hist(plVec, breaks = 50)

round(plVec, 3)





