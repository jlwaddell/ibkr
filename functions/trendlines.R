# Function to find the best support line based on the lowest sum of squared differences
find_best_support_line <- function(stockWindowData, candidate_lines, type = "rising") {
	# Initialize a vector to store SSD values for each candidate line
	ssds <- numeric(length(candidate_lines$slope))
	
	# Loop through each candidate line
	for(i in seq_along(candidate_lines$slope)) {
		# Extract the slope and intercept for the current line
		slope <- candidate_lines$slope[i]
		intercept <- candidate_lines$intercept[i]
		
		# Calculate the y-values of the candidate line for each x (index)
		fittedValues <- intercept + slope * stockWindowData$index
		
		weights <- rep(1, length(fittedValues)) # TODO think about
		
		# Compute the sum of squared differences between the "Low" prices and the line
		if(type == "rising")
			ssds[i] <- sum((stockWindowData$Low - fittedValues) ^ 2 * weights)
		if(type == "falling")
			ssds[i] <- sum((stockWindowData$High - fittedValues) ^ 2 * weights)
	}
	
	# Find the index of the line with the minimum SSD
	best_line_index <- which.min(ssds)
	
	# Return the best line index
	best_line_index
}



proposeRisingTrendlines <- function(stockWindowData) {
	
	# Filter prices data for lows that fall on the convex hull
	lows <- find_local_minmaxs(stockWindowData = stockWindowData, 
			type = "rising")
	if(nrow(lows) < 2)
		lows <- stockWindowData[chull(stockWindowData[c("index", "Low")]), ]
#	%>%
#				filter(index < max(index))
	
	# Find all unique possible combinations of two lows
	# (and all unique possible combinations of their associated dates)
	xCols <- as.data.frame(t(combn(lows$index, m=2, simplify=TRUE)))
	colnames(xCols) <- c("X1", "X2")
	yCols <- as.data.frame(t(combn(lows$Low, m=2, simplify=TRUE)))
	colnames(yCols) <- c("Y1", "Y2")
	all_lowcombos <- bind_cols(xCols, yCols)
	colnames(all_lowcombos) <- c("X1", "X2", "Y1", "Y2")
	
	# Generate a trend line for every combination of points
	n <- seq(1:nrow(all_lowcombos))
	low_trendfinder <- function(n, all_lowcombos) {
		model <- lm(c(all_lowcombos$Y1[n], all_lowcombos$Y2[n]) ~ 
						c(all_lowcombos$X1[n], all_lowcombos$X2[n]))
		data.frame(intercept = model$coefficients[1], 
				slope = model$coefficients[2])
	}
	risingTrendlines <- map_dfr(n,low_trendfinder,
			all_lowcombos = all_lowcombos)
	
	risingTrendlines <- risingTrendlines[risingTrendlines$slope > 0, ]
	
	
#	x = risingTrendlines$slope[4]
#	y = risingTrendlines$intercept[4]
#	plot(stockWindowData$index, stockWindowData$Low)
#	points(x = lows$index, y = lows$Low, pch = 19)
#	segments(x0 = min(stockWindowData$index), 
#			x1 = max(stockWindowData$index), 
#			y0 = y + x * min(stockWindowData$index), 
#			y1 = y + x * max(stockWindowData$index), 
#			lwd = 2)
	
	
	# For each low_trendline, check if any low in the prices dataframe falls below the line
	# Also make sure the trendline wouldn't be less than half the current price for today's date
	low_trendline_test <- function(x, y, stockWindowData){
		closeBelowTrendline <- stockWindowData$Low + 0.01 < 
				x * as.numeric(stockWindowData$index) + y 
		sum(closeBelowTrendline) <= 3
	}
	fewBelow <- map2(.x = risingTrendlines$slope, 
			.y = risingTrendlines$intercept, .f = low_trendline_test,
			stockWindowData = stockWindowData)
	fewBelow <- unlist(fewBelow)
	risingTrendlines <- risingTrendlines[fewBelow, ]
	
	return(risingTrendlines)
}


proposeFallingTrendlines <- function(stockWindowData) {
	# Filter prices data for lows that fall on the convex hull

	highs <- find_local_minmaxs(stockWindowData = stockWindowData, 
			type = "falling")
	if(nrow(highs) < 2)
		highs <- stockWindowData[chull(stockWindowData[c("index", "High")]), ] 
#	%>%
#				filter(index < max(index))  # TODO maybe uncomment this later
	
	# Find all unique possible combinations of two lows
	# (and all unique possible combinations of their associated dates)	f
	xCols <- as.data.frame(t(combn(highs$index, m=2, simplify=TRUE)))
	colnames(xCols) <- c("X1", "X2")
	yCols <- as.data.frame(t(combn(highs$High, m=2, simplify=TRUE)))
	colnames(yCols) <- c("Y1", "Y2")
	all_highcombos <- bind_cols(xCols, yCols)
	colnames(all_highcombos) <- c("X1", "X2", "Y1", "Y2")
	
	# Generate a trend line for every combination of points
	n <- seq(1:nrow(all_highcombos))
	high_trendfinder <- function(n, all_highcombos) {
		model <- lm(c(all_highcombos$Y1[n], all_highcombos$Y2[n]) ~ 
						c(all_highcombos$X1[n], all_highcombos$X2[n]))
		data.frame(intercept = model$coefficients[1], 
				slope = model$coefficients[2])
	}
	fallingTrendlines <- map_dfr(n, high_trendfinder,
			all_highcombos = all_highcombos)
	fallingTrendlines <- fallingTrendlines[fallingTrendlines$slope < 0, ]
	
#	x = fallingTrendlines$slope[10]
#	y = fallingTrendlines$intercept[10]
#	plot(stockWindowData$index, stockWindowData$High)
#	points(x = highs$index, y = highs$High, pch = 19)
#	segments(x0 = min(stockWindowData$index), 
#			x1 = max(stockWindowData$index), 
#			y0 = y + x * min(stockWindowData$index), 
#			y1 = y + x * max(stockWindowData$index), 
#			lwd = 2)
	
	# For each low_trendline, check how many High values fall above the trendline
	high_trendline_test <- function(x, y, stockWindowData){
		closeAboveTrendline <- !(x * as.numeric(stockWindowData$index) +
				y > stockWindowData$High - 0.01)
		sum(closeAboveTrendline) <= 3 # TODO hardcoded 
	}
	few_above <- map2(.x = fallingTrendlines$slope, 
			.y = fallingTrendlines$intercept, .f = high_trendline_test,
			stockWindowData = stockWindowData)
	few_above <- unlist(few_above)
	fallingTrendlines <- fallingTrendlines[few_above, ]
	
	return(fallingTrendlines)
}


calcSupportLine <- function(fullData, optimizationRange = c(60, 5), 
		type = "rising", matchedIdx, minimumSupportTime = 6) {
		
	# calculate all possible trendlines
	selectedRows <- (matchedIdx - optimizationRange[1]):(matchedIdx - optimizationRange[2])
	selectedRows <- selectedRows[selectedRows >= minimumSupportTime]
	stockWindowData <- fullData[selectedRows, ]
	
	if(length(selectedRows) >= 10) {
		if(type == "rising") {
			allTrendlines <- proposeRisingTrendlines(stockWindowData = fullData[selectedRows, ])
		} else {
			allTrendlines <- proposeFallingTrendlines(stockWindowData = fullData[selectedRows, ])
		}
		
		# pick the best one (least-squares)
		bestSupportIdx <- find_best_support_line(stockWindowData = fullData[selectedRows, ], 
				candidate_lines = allTrendlines, type = type)
		bestSupport <- allTrendlines[bestSupportIdx, ]
		
		return(bestSupport)
	}
	
}



# Function to find local minima in stock price data
find_local_minmaxs <- function(stockWindowData, type = "rising") {
	# Initialize an empty data frame to store local minima
	local_minima <- data.frame()
	
	if(type == "rising") {
		# Loop through the price data from the second to the second-to-last row
		for (kRow in 2:(nrow(stockWindowData) - 1)) {
			# Check if the current price is less than the previous and next prices
			if (stockWindowData$Low[kRow] <= stockWindowData$Low[kRow - 1] && 
					stockWindowData$Low[kRow] <= stockWindowData$Low[kRow + 1]) {
				# If a local minimum is found, add the row to the local_minima data frame
				local_minima <- rbind(local_minima, stockWindowData[kRow, ])
			}
		}
	}

	if(type == "falling") {
		# Loop through the price data from the second to the second-to-last row
		for (kRow in 2:(nrow(stockWindowData) - 1)) {
			# Check if the current price is less than the previous and next prices
			if (stockWindowData$High[kRow] >= stockWindowData$High[kRow - 1] && 
					stockWindowData$High[kRow] >= stockWindowData$High[kRow + 1]) {
				# If a local minimum is found, add the row to the local_minima data frame
				local_minima <- rbind(local_minima, stockWindowData[kRow, ])
			}
		}
	}
	
	return(local_minima)
}





