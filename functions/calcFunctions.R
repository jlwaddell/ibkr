# TODO: Add comment
# 
# Author: Jason
###############################################################################



calcTradeEndTime <- function(tmpData, fullData, rowNum = 1, 
		stopLossMult, profitTakeMult) {
	
	matchedIdx <- which(fullData$time == tmpData$time[rowNum])
	
	if(fullData$atr[matchedIdx] != 0) {
		# BOT
		if(tmpData$Quantity[rowNum] > 0) {
			
			stopLoss <- tmpData$T..Price[rowNum] - stopLossMult * fullData$atr[matchedIdx]
			profitTake <- tmpData$T..Price[rowNum] + profitTakeMult * fullData$atr[matchedIdx]
			
			# take the stop loss indices that are greater than our trade start (matchedIdx)
			stopIdx <- which(fullData$Close <= stopLoss)
			if(length(stopIdx) > 0) stopIdx <- stopIdx[which(stopIdx > matchedIdx)]
			
			# get the profit take indices
			profIdx <- which(fullData$Close >= profitTake)
			if(length(profIdx) > 0) profIdx <- profIdx[which(profIdx > matchedIdx)]
			
			if(length(stopIdx) == 0 & length(profIdx) > 0) {
				endIdx <- profIdx[1] # winner
			} else if(length(stopIdx) > 0 & length(profIdx) == 0) {
				endIdx <- stopIdx[1]  # loser
			} else if(length(stopIdx) == 0 & length(profIdx) == 0) {
				endIdx <- nrow(fullData) - 1 # end of day
			} else if(profIdx[1] < stopIdx[1]) {
				endIdx <- profIdx[1] # winner
			} else 
				endIdx <- stopIdx[1]  # loser
			
		} else {  # SLD
			
			stopLoss <- tmpData$T..Price[rowNum] + stopLossMult * fullData$atr[matchedIdx]
			profitTake <- tmpData$T..Price[rowNum] - profitTakeMult * fullData$atr[matchedIdx]
			
			# take the stop loss indices that are greater than our trade start (matchedIdx)
			stopIdx <- which(fullData$Close >= stopLoss)
			if(length(stopIdx) > 0) stopIdx <- stopIdx[which(stopIdx > matchedIdx)]
			
			# get the profit take indices
			profIdx <- which(fullData$Close <= profitTake)
			if(length(profIdx) > 0) profIdx <- profIdx[which(profIdx > matchedIdx)]
			
			if(length(stopIdx) == 0 & length(profIdx) > 0) {
				endIdx <- profIdx[1] # winner
			} else if(length(stopIdx) > 0 & length(profIdx) == 0) {
				endIdx <- stopIdx[1]  # loser
			} else if(length(stopIdx) == 0 & length(profIdx) == 0) {
				endIdx <- nrow(fullData) - 1 # end of day
			} else if(profIdx[1] < stopIdx[1]) {
				endIdx <- profIdx[1] # winner
			} else 
				endIdx <- stopIdx[1]  # loser
		}  
	} else {
		endIdx <- nrow(fullData) - 1 # end of day
	}
	
	return(endIdx)
	
}



computeStats <- function(data, 
		stopLossMult = stopLossMult, 
		profitTakeMult = profitTakeMult, 
		minutesBefore = 90) {
	
	data$rsquared <- rep(0, nrow(data))
	data$tradeResult <- rep(NA, nrow(data))
	
	for(iPlotNum in unique(data$plotNum)) {
		
		
		tmpData <- formatTmpData(data = data, plotNum = iPlotNum)
		
		fullData <- loadData(tmpData = tmpData, plotNum = iPlotNum)$full
		
		# match the idx of the trade
		matchedIdx <- which(fullData$time == tmpData$time[1])
		
		# calculate the start  times
		startIdx <- max(c(1, matchedIdx - minutesBefore))
		
		# regression
		regressionData <- fullData[startIdx:(matchedIdx-1), ]
		regModel <- lm(Close ~ index, data = regressionData)
		data$rsquared[iPlotNum] <- summary(regModel)$r.squared
		
		data$tradeResult[iPlotNum] <- calcTradeResult(tmpData, fullData, 
				rowNum = 1, stopLossMult = stopLossMult, 
				profitTakeMult = profitTakeMult) 
		
	}
	
	return(data)
}




calcTradeResult <- function(tmpData, fullData, rowNum = 1, 
		stopLossMult, profitTakeMult) {
	
	matchedIdx <- which(fullData$time == tmpData$time[rowNum])
	
	if(fullData$atr[matchedIdx] != 0) {
		# BOT
		if(tmpData$Quantity[rowNum] > 0) {
			
			stopLoss <- tmpData$T..Price[rowNum] - stopLossMult * fullData$atr[matchedIdx]
			profitTake <- tmpData$T..Price[rowNum] + profitTakeMult * fullData$atr[matchedIdx]
			
			# take the stop loss indices that are greater than our trade start (matchedIdx)
			stopIdx <- which(fullData$Close <= stopLoss)
			if(length(stopIdx) > 0) stopIdx <- stopIdx[which(stopIdx > matchedIdx)]
			
			# get the profit take indices
			profIdx <- which(fullData$Close >= profitTake)
			if(length(profIdx) > 0) profIdx <- profIdx[which(profIdx > matchedIdx)]
			
			if(length(stopIdx) == 0 & length(profIdx) > 0) {
				tradeResult <- 1 # winner
			} else if(length(stopIdx) > 0 & length(profIdx) == 0) {
				tradeResult <- 0  # loser
			} else if(length(stopIdx) == 0 & length(profIdx) == 0) {
				if(fullData$Close[nrow(fullData)] > tmpData$T..Price[rowNum]) {
					tradeResult <- 1 # winner
				} else {
					tradeResult <- 0 # loser
				}
			} else if(profIdx[1] < stopIdx[1]) {
				tradeResult <- 1 # winner
			} else 
				tradeResult <- 0  # loser
			
		} else {  # SLD
			
			stopLoss <- tmpData$T..Price[rowNum] + stopLossMult * fullData$atr[matchedIdx]
			profitTake <- tmpData$T..Price[rowNum] - profitTakeMult * fullData$atr[matchedIdx]
			
			# take the stop loss indices that are greater than our trade start (matchedIdx)
			stopIdx <- which(fullData$Close >= stopLoss)
			if(length(stopIdx) > 0) stopIdx <- stopIdx[which(stopIdx > matchedIdx)]
			
			# get the profit take indices
			profIdx <- which(fullData$Close <= profitTake)
			if(length(profIdx) > 0) profIdx <- profIdx[which(profIdx > matchedIdx)]
			
			if(length(stopIdx) == 0 & length(profIdx) > 0) {
				tradeResult <- 1 # winner
			} else if(length(stopIdx) > 0 & length(profIdx) == 0) {
				tradeResult <- 0  # loser
			} else if(length(stopIdx) == 0 & length(profIdx) == 0) {
				
				if(fullData$Close[nrow(fullData)] < tmpData$T..Price[rowNum]) {
					tradeResult <- 1 # winner
				} else {
					tradeResult <- 0 # loser
				}
				
			} else if(profIdx[1] < stopIdx[1]) {
				tradeResult <- 1 # winner
			} else 
				tradeResult <- 0  # loser
		}  
	} else {
		tradeResult <- NA
	}
	
	return(tradeResult)
	
}






calcPLStrategy <- function(data, plotNum, 
		stopValues, profitTakes) {
	tmpData <- data[which(data$plotNum == plotNum), ]
	
	from <- tmpData$day[1]
	to <- ymd(from) + duration(1, "days")
	ticker <- tmpData$Symbol[1]
	
	x <- getSymbols(ticker, src="yahoo", 
			periodicity = "1 minutes", 
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
	
	# define the fullData object
	fullData <- formatFullData(input = get(x))
	
	# match trade data index to fullData index
	substr(tmpData$time, 7, 8) <- "00"
	
	matchedIdx <- which(fullData$time == tmpData$time[1])
	
	# define the profit-loss matrix
	plMatrix <- matrix(0, nrow = length(stopValues), ncol = length(profitTakes))
	rownames(plMatrix) <- stopValues
	colnames(plMatrix) <- profitTakes
	
	for(kStop in 1:length(stopValues)) {  
		for(jProf in 1:length(profitTakes)) {
			
			stopLossMult <- stopValues[kStop]
			profitTakeMult <- profitTakes[jProf]
			
			# BOT
			if(tmpData$Quantity[1] > 0) {
				
				stopLoss <- tmpData$T..Price[1] - stopLossMult * fullData$atr[matchedIdx]
				profitTake <- tmpData$T..Price[1] + profitTakeMult * fullData$atr[matchedIdx]
				
				# take the stop loss indices that are greater than our trade start (matchedIdx)
				stopIdx <- which(fullData$Close <= stopLoss)
				if(length(stopIdx) > 0) stopIdx <- stopIdx[which(stopIdx > matchedIdx)]
				
				# get the profit take indices
				profIdx <- which(fullData$Close >= profitTake)
				if(length(profIdx) > 0) profIdx <- profIdx[which(profIdx > matchedIdx)]
				
				if(length(stopIdx) == 0 & length(profIdx) > 0) {
					pl <- (profitTake - tmpData$T..Price[1]) / tmpData$T..Price[1] # winner
				} else if(length(stopIdx) > 0 & length(profIdx) == 0) {
					pl <- (stopLoss - tmpData$T..Price[1]) / tmpData$T..Price[1]  # loser
				} else if(length(stopIdx) == 0 & length(profIdx) == 0) {
					pl <- (fullData$Close[nrow(fullData)] - tmpData$T..Price[1]) / tmpData$T..Price[1] # end of day
				} else if(profIdx[1] < stopIdx[1]) {
					pl <- (profitTake - tmpData$T..Price[1]) / tmpData$T..Price[1] # winner
				} else 
					pl <- (stopLoss - tmpData$T..Price[1]) / tmpData$T..Price[1]  # loser
				
			} else {  # SLD
				
				stopLoss <- tmpData$T..Price[1] + stopLossMult * fullData$atr[matchedIdx]
				profitTake <- tmpData$T..Price[1] - profitTakeMult * fullData$atr[matchedIdx]
				
				# take the stop loss indices that are greater than our trade start (matchedIdx)
				stopIdx <- which(fullData$Close >= stopLoss)
				if(length(stopIdx) > 0) stopIdx <- stopIdx[which(stopIdx > matchedIdx)]
				
				# get the profit take indices
				profIdx <- which(fullData$Close <= profitTake)
				if(length(profIdx) > 0) profIdx <- profIdx[which(profIdx > matchedIdx)]
				
				if(length(stopIdx) == 0 & length(profIdx) > 0) {
					pl <- (tmpData$T..Price[1] - profitTake) / tmpData$T..Price[1] # winner
				} else if(length(stopIdx) > 0 & length(profIdx) == 0) {
					pl <- (tmpData$T..Price[1] - stopLoss) / tmpData$T..Price[1]  # loser
				} else if(length(stopIdx) == 0 & length(profIdx) == 0) {
					pl <- (tmpData$T..Price[1] - fullData$Close[nrow(fullData)]) / tmpData$T..Price[1] # end of day
				} else if(profIdx[1] < stopIdx[1]) {
					pl <- (tmpData$T..Price[1] - profitTake) / tmpData$T..Price[1] # winner
				} else 
					pl <- (tmpData$T..Price[1] - stopLoss) / tmpData$T..Price[1]  # loser
				
			}  
			
			plMatrix[kStop, jProf] <- pl
			
		}
	}
	
	
	return(plMatrix)
}




calcPVT <- function(close, volume) {
	
	pvt <- rep(0, length(close))
	for(j in 2:length(close)) {
		pvt[j] <- pvt[j-1] + ((close[j] - close[j-1]) / close[j-1]) * volume[j]
	}
	
	return(pvt)
}



