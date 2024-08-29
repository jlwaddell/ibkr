

load("./data/data2min.RData")

choppinessWindow <- 20
rsquaredWindow <- 20
stopLossMult = 2
profitTakeMult = 6
breakevenTriggerMult = 1
setBreakeven <- TRUE
timeOfDayLimit <- 75

r2Thresh <- 0.91
chopThresh <- 0.382
pvtATR <- 1

plVec <- numeric()
k <- 1

# TODO
# stop losses
# something about monotonic-ness. want more / and less V
# start along the trendline

pdf("./plots/backtest.pdf", width = 10, height = 8)
for(i in 1:length(dataList)) {
	
	cat(i)
	fullData <- formatFullData(input = dataList[[i]])
	
	# find the streaks and take the 2nd true
	tfVec <- (fullData$choppiness < chopThresh) & (fullData$rsquared > r2Thresh)
	tfVec[which(is.na(tfVec))] <- FALSE
	streakVec <- count_consecutive_true(tfVec)
	
	matchedIdxVec <- which(streakVec == 2)
	matchedIdxVec <- matchedIdxVec[which(matchedIdxVec < 75)] # TODO consider
	                                                      # this is a shortcut for the monotonicness
	
	for(jMatch in seq_along(matchedIdxVec)) {  # jMatch <- 1
		
		matchedIdx <- matchedIdxVec[jMatch]
		makeTrade <- FALSE
		
		# create fake data
		ticker <- strsplit(names(dataList)[i], " ")[[1]][1]
		date <- strsplit(names(dataList)[i], " ")[[1]][2]
		time <- fullData$time[matchedIdx]
		
		# TODO maybe compare the slopes
		
		# BOT
		if(
				(fullData$Close[matchedIdx] > fullData$Close[matchedIdx - choppinessWindow]) & 
				(fullData$Close[matchedIdx] - 0.5 * fullData$atr[matchedIdx]  <
				fullData$slidingScaledPVT[matchedIdx]  )
				) {
			
			makeTrade <- TRUE
			quantity <- 1
			
			stopLoss <- fullData$Close[matchedIdx] - stopLossMult * fullData$atr[matchedIdx]
			profitTake <- fullData$Close[matchedIdx] + profitTakeMult * fullData$atr[matchedIdx]
			
			# take the stop loss indices that are greater than our trade start (matchedIdx)
			stopIdx <- which(fullData$Close <= stopLoss)
			if(length(stopIdx) > 0) stopIdx <- stopIdx[which(stopIdx > matchedIdx)]
			
			# get the profit take indices
			profIdx <- which(fullData$Close >= profitTake)
			if(length(profIdx) > 0) profIdx <- profIdx[which(profIdx > matchedIdx)]
			
			if(length(stopIdx) == 0 & length(profIdx) > 0) {
				pl <- (profitTake - fullData$Close[matchedIdx]) / fullData$Close[matchedIdx] # winner
			} else if(length(stopIdx) > 0 & length(profIdx) == 0) {
				pl <- (stopLoss - fullData$Close[matchedIdx]) / fullData$Close[matchedIdx]  # loser
			} else if(length(stopIdx) == 0 & length(profIdx) == 0) {
				pl <- (fullData$Close[nrow(fullData)] - fullData$Close[matchedIdx]) / fullData$Close[matchedIdx] # end of day
			} else if(profIdx[1] < stopIdx[1]) {
				pl <- (profitTake - fullData$Close[matchedIdx]) / fullData$Close[matchedIdx] # winner
			} else 
				pl <- (stopLoss - fullData$Close[matchedIdx]) / fullData$Close[matchedIdx]  # loser
			
			if(setBreakeven) {
				breakevenTriggerPoint <- fullData$Close[matchedIdx] + breakevenTriggerMult * fullData$atr[matchedIdx]
				breakevenIdx <- which(fullData$Close >= breakevenTriggerPoint)
				if(length(breakevenIdx) > 0) breakevenIdx <- breakevenIdx[which(breakevenIdx > matchedIdx)]
				
				zeroPoint <- which(fullData$Close <= fullData$Close[matchedIdx])
				if(length(zeroPoint) > 0) zeroPoint <- zeroPoint[which(zeroPoint > breakevenIdx[1])]
				
				if(length(zeroPoint) > 0) {
					if(length(stopIdx) == 0) stopIdx <- nrow(fullData)
					if(length(profIdx) == 0) profIdx <- nrow(fullData)
					
					if( (zeroPoint[1] < stopIdx[1]) & (zeroPoint[1] < profIdx[1]) )
						pl <- 0
				}
				
			}
			
		} else if(
				(fullData$Close[matchedIdx] < fullData$Close[matchedIdx - choppinessWindow]) & 
				(fullData$Close[matchedIdx] + 0.5 * fullData$atr[matchedIdx] >
					fullData$slidingScaledPVT[matchedIdx] )
				){  # SLD
			
			makeTrade <- TRUE
			quantity <- -1
			
			stopLoss <- fullData$Close[matchedIdx] + stopLossMult * fullData$atr[matchedIdx]
			profitTake <- fullData$Close[matchedIdx] - profitTakeMult * fullData$atr[matchedIdx]
			
			# take the stop loss indices that are greater than our trade start (matchedIdx)
			stopIdx <- which(fullData$Close >= stopLoss)
			if(length(stopIdx) > 0) stopIdx <- stopIdx[which(stopIdx > matchedIdx)]
			
			# get the profit take indices
			profIdx <- which(fullData$Close <= profitTake)
			if(length(profIdx) > 0) profIdx <- profIdx[which(profIdx > matchedIdx)]
			
			if(length(stopIdx) == 0 & length(profIdx) > 0) {
				pl <- (fullData$Close[matchedIdx] - profitTake) / fullData$Close[matchedIdx] # winner
			} else if(length(stopIdx) > 0 & length(profIdx) == 0) {
				pl <- (fullData$Close[matchedIdx] - stopLoss) / fullData$Close[matchedIdx]  # loser
			} else if(length(stopIdx) == 0 & length(profIdx) == 0) {
				pl <- (fullData$Close[matchedIdx] - fullData$Close[nrow(fullData)]) / fullData$Close[matchedIdx] # end of day
			} else if(profIdx[1] < stopIdx[1]) {
				pl <- (fullData$Close[matchedIdx] - profitTake) / fullData$Close[matchedIdx] # winner
			} else 
				pl <- (fullData$Close[matchedIdx] - stopLoss) / fullData$Close[matchedIdx]  # loser
			
			if(setBreakeven) {
				breakevenTriggerPoint <- fullData$Close[matchedIdx] - breakevenTriggerMult * fullData$atr[matchedIdx]
				breakevenIdx <- which(fullData$Close <= breakevenTriggerPoint)
				if(length(breakevenIdx) > 0) breakevenIdx <- breakevenIdx[which(breakevenIdx > matchedIdx)]
				
				zeroPoint <- which(fullData$Close >= fullData$Close[matchedIdx])
				if(length(zeroPoint) > 0) zeroPoint <- zeroPoint[which(zeroPoint > breakevenIdx[1])]
				
				if(length(zeroPoint) > 0) {
					if(length(stopIdx) == 0) stopIdx <- nrow(fullData)
					if(length(profIdx) == 0) profIdx <- nrow(fullData)
					
					if( (zeroPoint[1] < stopIdx[1]) & (zeroPoint[1] < profIdx[1]) )
						pl <- 0
				}
				 
			}
			
		}  
		
		if(makeTrade) {
			fakeData <- data.frame(Symbol = ticker, Quantity = quantity, 
					T..Price = fullData$Close[matchedIdx], day = date, time = time, plotNum = 1)
			vizTradeAndStrategy(data = fakeData, dataList = dataList, 
					plotNum = 1, 
					stopLossMult = stopLossMult, profitTakeMult = profitTakeMult, 
#				omitTimepoints = c(1:5, 191:195), 
					includeADX = TRUE, 
					dataType = "normal", 
#					ending = "tradeStart"
			)
			
			
			plVec[k] <- pl
			k <- k + 1
		}

		
	}
	
	
	
}
dev.off()


mean(plVec)
sd(plVec)


mean(plVec) / (sd(plVec) / sqrt(length(plVec)))









