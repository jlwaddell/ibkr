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

source("./functions/plotFunctions.R")
source("./functions/loadingFunctions.R")
source("./functions/calcFunctions.R")
source("./functions/trendlines.R")


lookbackWindow <- 20
minimumTimepoint <- 21
stopLossMult = 2
profitTakeMult = 6
breakevenTriggerMult = 1
setBreakeven <- FALSE
stopLossType = "fixed"

r2Thresh <- 0.8   # .9
chopThresh <- 0.5   # 0.382
distanceFromTrendlineThreshold <- 0.6
distanceFromPvtThreshold <- 200   # TODO will work on this next

plVec <- numeric()
k <- 1

# TODO
# walk through the plots manually starting with #1, AMD
  #                                                 LNG 2024-08-09
 #                                                 NKE 2024-08-13
  #  SBUX 08-12 for still monotonic not working
# figure out a way to do volume properly
# maybe look for trade confirmation. (to prevent immediate reversals)
# I'm not sure Rsquared and Choppiness are ideal. Maybe find something that looks for avd distance from support line or something

pdf("./plots/backtest.pdf", width = 10, height = 8)
for(i in 1:length(dataList)) {   # i <- 1
	
	cat(i)
	fullData <- fullDataList[[i]]
	
	# find the streaks and take the 2nd true
	tfVec <- (fullData$choppinessBest < chopThresh) & (fullData$rsquaredBest > r2Thresh)
	tfVec[which(is.na(tfVec))] <- FALSE
	streakVec <- count_consecutive_true(tfVec)  
	
	matchedIdxVec <- which(tfVec == TRUE)
#	matchedIdxVec <- which(streakVec == 3)   # TODO improve this logic
	if(length(matchedIdxVec) > 0)
		matchedIdxVec <- matchedIdxVec[matchedIdxVec >= minimumTimepoint]
	
	for(jMatch in seq_along(matchedIdxVec)) {  # jMatch <- 1
		
		matchedIdx <- matchedIdxVec[jMatch]
		makeTrade <- FALSE
		
		# create fake data
		ticker <- strsplit(names(dataList)[i], " ")[[1]][1]
		date <- strsplit(names(dataList)[i], " ")[[1]][2]
		time <- fullData$time[matchedIdx]
		
		
		# BOT
		if(     
				fullData$stillMonotonic[matchedIdx] & 
				(fullData$Close[matchedIdx] > fullData$Close[matchedIdx - lookbackWindow]) & 
				(fullData$Close[matchedIdx] - distanceFromPvtThreshold * fullData$atr[matchedIdx]  <
					fullData$slidingScaledPVT[matchedIdx]  )
				) {
			
			
			makeTrade <- TRUE
			quantity <- 1
			
			# find the best trendline and calculate the distance to it
#			if(matchedIdx > 30) {  # TODO testing
			{
				bestSupport <- calcSupportLine(fullData, type = "rising", 
						matchedIdx = matchedIdx)
				
				if(nrow(bestSupport) > 0) {
					distanceAboveTrendline <- (fullData$Close[matchedIdx] - (bestSupport$intercept + bestSupport$slope * matchedIdx)) / 
							fullData$atr[matchedIdx]
					if(distanceAboveTrendline > distanceFromTrendlineThreshold | distanceAboveTrendline < 0)
						makeTrade <- FALSE
				}
			}
		
			
#			}
			
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
				(fullData$Close[matchedIdx] + distanceFromPvtThreshold * fullData$atr[matchedIdx] >
					fullData$slidingScaledPVT[matchedIdx] )
				){  # SLD
			
			makeTrade <- TRUE
			quantity <- -1
			
			# find the best trendline and calculate the distance to it
#			if(matchedIdx > 30) { # TODO testing
			
			bestSupport <- calcSupportLine(fullData, type = "falling", 
					matchedIdx = matchedIdx)
			
#			x = bestSupport$slope
#			y = bestSupport$intercept
#			plot(fullData$index[1:matchedIdx], fullData$High[1:matchedIdx])
#			segments(x0 = 1, 
#					x1 = matchedIdx, 
#					y0 = y + x * 1, 
#					y1 = y + x * matchedIdx, 
#					lwd = 2)
			
			if(nrow(bestSupport) > 0) {
				distanceBelowTrendline <- (fullData$Close[matchedIdx] - 
							(bestSupport$intercept + bestSupport$slope * matchedIdx)) / 
						fullData$atr[matchedIdx]
				if(distanceBelowTrendline < (-1 * distanceFromTrendlineThreshold) | distanceBelowTrendline > 0)
					makeTrade <- FALSE
			}
#			}
			
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
		}  
		
		if(makeTrade) {
			fakeData <- data.frame(Symbol = ticker, Quantity = quantity, 
					T..Price = fullData$Close[matchedIdx], day = date, time = time, plotNum = 1)
			vizTradeAndStrategy(data = fakeData, 
					dataList = dataList, 
					fullDataList = fullDataList, 
					plotNum = 1, 
					stopLossMult = stopLossMult, profitTakeMult = profitTakeMult, 
#				omitTimepoints = c(1:5, 191:195), 
					includeADX = TRUE, 
					dataType = "normal", 
#					ending = "tradeStart"
			)
			
			
			plVec[k] <- pl
			k <- k + 1
			break
		}
		
		
	}
	
	
	
}
dev.off()


mean(plVec)
sd(plVec)
length(plVec)

mean(plVec) / (sd(plVec) / sqrt(length(plVec)))

# hist(plVec, breaks = 50)

round(plVec, 3)





