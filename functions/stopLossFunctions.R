

# Function to compute percent profit or loss for a fixed stop loss and take profit
compute_trade_result_fixed <- function(fullData, matchedIdx, 
		stopLossMult, profitTakeMult, 
		action = "BOT") {

	start_price <- fullData$Close[matchedIdx]
	
	# Extract price data after the start time
	trade_prices <- fullData[fullData$index >= matchedIdx, ]
	
	# Iterate over price data to determine exit condition
	
	if(action == "BOT") {
		
		# define stop loss and profit takes
		stopLossPrice <- fullData$Close[matchedIdx] - stopLossMult * fullData$atr[matchedIdx]
		takeProfitPrice <- fullData$Close[matchedIdx] + profitTakeMult * fullData$atr[matchedIdx]
		
		for (i in 1:nrow(trade_prices)) {
			current_price <- trade_prices$Close[i]
			
			# Check for stop loss or take profit
			if (current_price <= stopLossPrice) {
				percent_profit_loss <- ((current_price - start_price) / start_price) 
				return(percent_profit_loss)
			} else if (current_price >= takeProfitPrice) {
				percent_profit_loss <- ((current_price - start_price) / start_price) 
				return(percent_profit_loss)
			}
		}
	} else {   # SLD
		
		# define stop loss and profit takes
		stopLossPrice <- fullData$Close[matchedIdx] + stopLossMult * fullData$atr[matchedIdx]
		takeProfitPrice <- fullData$Close[matchedIdx] - profitTakeMult * fullData$atr[matchedIdx]
		
		for(i in 1:nrow(trade_prices)) {
			current_price <- trade_prices$Close[i]
			
			# Check for stop loss or take profit
			if (current_price >= stopLossPrice) {
				percent_profit_loss <- ((start_price - current_price) / start_price) 
				return(percent_profit_loss)
			} else if (current_price <= takeProfitPrice) {
				percent_profit_loss <- ((start_price - current_price) / start_price) 
				return(percent_profit_loss)
			}
		}
	}

	
	# If no exit condition is met, return the result at the end of the data
	final_price <- trade_prices$Close[nrow(trade_prices)]
	if(action == "BOT") {
		percent_profit_loss <- ((final_price - start_price) / start_price) 
	} else 
		percent_profit_loss <- ((start_price - final_price) / start_price) 
	return(percent_profit_loss)
}


checkBreakeven <- function(fullData, matchedIdx, 
		stopLossMult, profitTakeMult, breakevenTriggerMult, action = "BOT") {
	

	
	if(action == "BOT") {
		# define stop loss and profit takes
		stopLoss <- fullData$Close[matchedIdx] - stopLossMult * fullData$atr[matchedIdx]
		profitTake <- fullData$Close[matchedIdx] + profitTakeMult * fullData$atr[matchedIdx]
		
		# take the stop loss indices that are greater than our trade start (matchedIdx)
		stopIdx <- which(fullData$Close <= stopLoss)
		if(length(stopIdx) > 0) stopIdx <- stopIdx[which(stopIdx > matchedIdx)]
		
		# get the profit take indices
		profIdx <- which(fullData$Close >= profitTake)
		if(length(profIdx) > 0) profIdx <- profIdx[which(profIdx > matchedIdx)]
		
		breakevenTriggerPoint <- fullData$Close[matchedIdx] + breakevenTriggerMult * fullData$atr[matchedIdx]
		breakevenIdx <- which(fullData$Close >= breakevenTriggerPoint)
		if(length(breakevenIdx) > 0) breakevenIdx <- breakevenIdx[which(breakevenIdx > matchedIdx)]
		
		zeroPoint <- which(fullData$Close <= fullData$Close[matchedIdx])
		if(length(zeroPoint) > 0) zeroPoint <- zeroPoint[which(zeroPoint > breakevenIdx[1])]
		
		if(length(zeroPoint) > 0) {
			if(length(stopIdx) == 0) stopIdx <- nrow(fullData)
			if(length(profIdx) == 0) profIdx <- nrow(fullData)
			
			if( (zeroPoint[1] < stopIdx[1]) & (zeroPoint[1] < profIdx[1]) )
				return(TRUE)
		}
		
	} else {
		
		stopLoss <- fullData$Close[matchedIdx] + stopLossMult * fullData$atr[matchedIdx]
		profitTake <- fullData$Close[matchedIdx] - profitTakeMult * fullData$atr[matchedIdx]
		
		# take the stop loss indices that are greater than our trade start (matchedIdx)
		stopIdx <- which(fullData$Close >= stopLoss)
		if(length(stopIdx) > 0) stopIdx <- stopIdx[which(stopIdx > matchedIdx)]
		
		# get the profit take indices
		profIdx <- which(fullData$Close <= profitTake)
		if(length(profIdx) > 0) profIdx <- profIdx[which(profIdx > matchedIdx)]
		
		breakevenTriggerPoint <- fullData$Close[matchedIdx] - breakevenTriggerMult * fullData$atr[matchedIdx]
		breakevenIdx <- which(fullData$Close <= breakevenTriggerPoint)
		if(length(breakevenIdx) > 0) breakevenIdx <- breakevenIdx[which(breakevenIdx > matchedIdx)]
		
		zeroPoint <- which(fullData$Close >= fullData$Close[matchedIdx])
		if(length(zeroPoint) > 0) zeroPoint <- zeroPoint[which(zeroPoint > breakevenIdx[1])]
		
		if(length(zeroPoint) > 0) {
			if(length(stopIdx) == 0) stopIdx <- nrow(fullData)
			if(length(profIdx) == 0) profIdx <- nrow(fullData)
			
			if( (zeroPoint[1] < stopIdx[1]) & (zeroPoint[1] < profIdx[1]) )
				return(TRUE)
		}
	}
	
	return(FALSE)
	
}


# Function to compute percent profit or loss with trailing stop loss and fixed take profit
compute_trade_result_trailing <- function(fullData, matchedIdx, 
		stopLossMult, profitTakeMult, 
		action = "BOT") {

	
	start_price <- fullData$Close[matchedIdx]
	
	# Extract price data after the start time
	trade_prices <- fullData[fullData$index >= matchedIdx, ]
	
	# Initialize maximum and minimum prices seen since the trade start
	max_price_seen <- min_price_seen <- start_price
	
	if(action == "BOT") {
	
		# define stop loss and profit takes
		trailingStopPrice <- fullData$Close[matchedIdx] - stopLossMult * fullData$atr[matchedIdx]
		takeProfitPrice <- fullData$Close[matchedIdx] + profitTakeMult * fullData$atr[matchedIdx]
		
		# Iterate over price data to determine exit condition
		for (i in 1:nrow(trade_prices)) {
			current_price <- trade_prices$Close[i]
			currentHigh <- trade_prices$High[i]
			
			# Update maximum price seen
			if (currentHigh > max_price_seen) {
				max_price_seen <- currentHigh
				# Update trailing stop loss based on the new maximum price
				trailingStopPrice <- max_price_seen - stopLossMult * fullData$atr[matchedIdx]
			}
			
			# Check for trailing stop or take profit
			if (current_price <= trailingStopPrice) {
				percent_profit_loss <- ((current_price - start_price) / start_price) 
				return(percent_profit_loss)
			} else if (current_price >= takeProfitPrice) {
				percent_profit_loss <- ((current_price - start_price) / start_price) 
				return(percent_profit_loss)
			}
		}
	} else {
		
		# define stop loss and profit takes
		trailingStopPrice <- fullData$Close[matchedIdx] + stopLossMult * fullData$atr[matchedIdx]
		takeProfitPrice <- fullData$Close[matchedIdx] - profitTakeMult * fullData$atr[matchedIdx]
		
		# Iterate over price data to determine exit condition
		for (i in 1:nrow(trade_prices)) {
			current_price <- trade_prices$Close[i]
			currentLow <- trade_prices$Low[i]
			
			# Update maximum price seen
			if (currentLow < min_price_seen) {
				min_price_seen <- currentLow
				# Update trailing stop loss based on the new maximum price
				trailingStopPrice <- min_price_seen + stopLossMult * fullData$atr[matchedIdx]
			}
			
			# Check for trailing stop or take profit
			if (current_price >= trailingStopPrice) {
				percent_profit_loss <- ((start_price - current_price) / start_price) 
				return(percent_profit_loss)
			} else if (current_price <= takeProfitPrice) {
				percent_profit_loss <- ((start_price - current_price) / start_price) 
				return(percent_profit_loss)
			}
		}
		
		
	}
	
	
	# If no exit condition is met, return the result at the end of the data
	final_price <- trade_prices$Close[nrow(trade_prices)]
	percent_profit_loss <- ((final_price - start_price) / start_price) 
	return(percent_profit_loss)
}