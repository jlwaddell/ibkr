

calcTradeEndTime <- function(tmpData, fullData, rowNum = 1, 
		stopLossMult, profitTakeMult) {
	
	matchedIdx <- which(fullData$time == tmpData$time[rowNum])
	if(length(matchedIdx) == 0) {
		timeDiffs <- abs(as.numeric(hms(fullData$time) - hms(tmpData$time[rowNum])))
		matchedIdx <- which(timeDiffs == min(timeDiffs))[1]
	}
	
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
	tmpData$xValue <- rep(0, nrow(tmpData))
	tmpData$tradeColor <- rep("", nrow(tmpData))
	substr(tmpData$time, 7, 8) <- "00"
	tmpData <- tmpData[1:2, ] # TODO fix this part
	ticker <- tmpData$Symbol[1]
	from <- tmpData$day[1]
	
	# define the fullData object
	rawIdx <- which(names(dataList) == paste0(ticker, " ", from))
	rawData <- dataList[[rawIdx]]
	fullData <- formatFullData(input = rawData)
	
	# match trade data index to fullData index
	substr(tmpData$time, 7, 8) <- "00"
	
	# match the idx of the trade
	matchedIdx <- which(fullData$time == tmpData$time[1])
	if(length(matchedIdx) == 0) {
		timeDiffs <- abs(as.numeric(hms(fullData$time) - hms(tmpData$time[1])))
		matchedIdx <- which(timeDiffs == min(timeDiffs))[1]
	}
	
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




compute_r_squared_moving_window <- function(prices, window_size) {
	# Create a vector to store R-squared values
	r_squared_values <- rep(NA, length(prices))
	
	# Loop over the data to compute R-squared for each window
	for (i in window_size:length(prices)) {
		# Extract the window data
		window_prices <- prices[(i - window_size + 1):i]
		time <- 1:window_size
		
		# Fit linear regression model
		model <- lm(window_prices ~ time)
		
		# Get the R-squared value
		r_squared <- summary(model)$r.squared
		
		# Store the R-squared value
		r_squared_values[i] <- r_squared
	}
	
	# Return the vector of R-squared values
	return(r_squared_values)
}


# Function to compute the Choppiness Index
compute_choppiness_index <- function(fullData, period = 14) {
	# Create a data frame with the OHLC data
	ohlc <- fullData[, c("Open", "High", "Low", "Close")]
	
	# Calculate the Choppiness Index
	choppiness_index <- rep(NA, nrow(ohlc))  # Initialize the result vector
	
	for (i in period:nrow(ohlc)) {
		# Subset the data for the current period
		high_period <- ohlc$High[(i-period+1):i]
		low_period <- ohlc$Low[(i-period+1):i]
		
		# Calculate the numerator and denominator
		sum_range <- sum(high_period - low_period)
		max_high <- max(high_period)
		min_low <- min(low_period)
		
		# Choppiness Index formula
		chop_value <- 100 * log10(sum_range / (max_high - min_low)) / log10(period)
		
		# Store the value
		choppiness_index[i] <- chop_value
	}
	
	# Return the computed Choppiness Index
	return(choppiness_index/100)
}


# Function to compute the number of consecutive TRUE values
count_consecutive_true <- function(x) {
	# Initialize a vector to store the counts
	count_vector <- integer(length(x))
	
	# Initialize the counter
	counter <- 0
	
	# Loop through the vector
	for (i in seq_along(x)) {
		if (x[i]) {
			# Increment the counter if the value is TRUE
			counter <- counter + 1
		} else {
			# Reset the counter if the value is FALSE
			counter <- 0
		}
		# Store the count
		count_vector[i] <- counter
	}
	
	return(count_vector)
}


convert_to_2min <- function(dayData) {
	# Check if the input data frame has the necessary columns
	if(!all(c("Open", "High", "Low", "Close", "Volume") %in% colnames(dayData))) {
		stop("Data frame must have columns: Open, High, Low, Close, and Volume")
	}
	
	dayData <- fill_datetime_gaps(dayData)
	
	origRownames <- rownames(dayData)
	endRownames <- origRownames[which((1:length(origRownames) %% 2) == 1)]
	
	# Use dplyr to group every two rows
	data_2min <- dayData %>%
			mutate(group = rep(1:(n()/2), each = 2, length.out = n())) %>%
			group_by(group) %>%
			summarise(
					Open = first(Open),                  # Open price of the first row in the group
					High = max(High),                    # Max high price in the group
					Low = min(Low),                      # Min low price in the group
					Close = last(Close),                 # Close price of the last row in the group
					Volume = sum(Volume, na.rm = TRUE)   # Sum of volume in the group
			) %>%
			ungroup() %>%
			select(-group)  # Remove the temporary group column
	
	data_2min <- as.data.frame(data_2min)
	rownames(data_2min) <- endRownames
	
	
	return(data_2min)
}



# Function to detect gaps and fill them
fill_datetime_gaps <- function(dayData) {
	
	# make the datetime colum
	dayData$datetime <- rownames(dayData)
	
	# Ensure the datetime column is of POSIXct type
	dayData$datetime <- as.POSIXct(dayData$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
	
	# Create a complete sequence of datetime values with 1-minute intervals
	full_sequence <- seq(from = min(dayData$datetime), to = max(dayData$datetime), by = "1 min")
	
	# Create a new data frame with the complete sequence
	complete_data <- data.frame(datetime = full_sequence)
	
	# Merge the complete sequence with the original data
	data_filled <- left_join(complete_data, dayData, by = "datetime")
	
	# Fill missing rows by copying values from the previous row
	data_filled <- data_filled %>% 
			tidyr::fill(everything(), .direction = "down")
	rownames(data_filled) <- data_filled$datetime
	data_filled <- data_filled[, c("Open", "High", "Low", "Close", "Volume")]
	
	return(data_filled)
}



still_monotonic <- function(close_values, lookback = 20) {
	# Initialize a logical vector to store "still monotonic" status
	is_still_monotonic <- rep(TRUE, length(close_values))
	
	# Iterate over each time point starting from the (lookback + 1)th time point
	for (i in (lookback + 1):length(close_values)) {
		# Get the past 'lookback' close values
		past_values <- close_values[1:(i - lookback)]
		
		# Check if there are any past values that are both higher and lower than the current close value
		if (any(past_values > close_values[i]) & any(past_values < close_values[i])) {
			# Set "still monotonic" to FALSE for the current time point
			is_still_monotonic[i] <- FALSE
		}
	}
	
	return(is_still_monotonic)
}