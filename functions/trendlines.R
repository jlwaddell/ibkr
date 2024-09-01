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
		
		weights <- (stockWindowData$index - min(stockWindowData$index))  # TODO think about
		
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
	lows <- stockWindowData[chull(stockWindowData[c("index", "Low")]), ] %>%
			filter(index < max(index))
	
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
	low_trendlines <- map_dfr(n,low_trendfinder,
			all_lowcombos = all_lowcombos)
	
	# x = low_trendlines$slope[1]
	# y = low_trendlines$intercept[1]
	
	# For each low_trendline, check if any low in the prices dataframe falls below the line
	# Also make sure the trendline wouldn't be less than half the current price for today's date
	low_trendline_test <- function(x, y, prices){
		!any(x * as.numeric(prices$index) + y > prices$Low + 0.01) &    # TRUE
				!(x*max(prices$index) + y < 0.5*prices$Close[nrow(prices)])  # TRUE
	}
	none_below <- map2(.x = low_trendlines$slope, 
			.y = low_trendlines$intercept, .f = low_trendline_test,
			prices = stockWindowData)
	none_below <- unlist(none_below)
	low_trendlines <- low_trendlines[none_below, ]
	
	return(low_trendlines)
}


proposeFallingTrendlines <- function(stockWindowData) {
	# Filter prices data for lows that fall on the convex hull
	highs <- stockWindowData[chull(stockWindowData[c("index", "High")]), ] %>%
			filter(index < max(index))
	
	# Find all unique possible combinations of two lows
	# (and all unique possible combinations of their associated dates)	
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
	high_trendlines <- map_dfr(n, high_trendfinder,
			all_highcombos = all_highcombos)
	
	# x = high_trendlines$slope[8]
	# y = high_trendlines$intercept[8]
	
	# For each low_trendline, check if any low in the prices dataframe falls below the line
	# Keep only trendlines for which this is FALSE
	# Also make sure the trendline wouldn't be less than half the current price for today's date
	high_trendline_test <- function(x, y, prices){
		!any(x * as.numeric(prices$index) + y < prices$High - 0.01) 
	}
	none_above <- map2(.x = high_trendlines$slope, 
			.y = high_trendlines$intercept, .f = high_trendline_test,
			prices = stockWindowData)
	none_above <- unlist(none_above)
	high_trendlines <- high_trendlines[none_above, ]
	
	return(high_trendlines)
}


calcSupportLine <- function(fullData, trendlineLookback = 30, optimizationRange = c(25, 5), 
		type = "rising") {
	
	# calculate all possible trendlines
#	selectedRows <- (matchedIdx - trendlineLookback):(matchedIdx - 1)
	selectedRows <- (matchedIdx - optimizationRange[1]):(matchedIdx - optimizationRange[2])
	if(type == "rising") {
		allTrendlines <- proposeRisingTrendlines(stockWindowData = fullData[selectedRows, ])
	} else {
		allTrendlines <- proposeFallingTrendlines(stockWindowData = fullData[selectedRows, ])
	}
		
	# pick the best one (least-squares)
	selectedRows <- (matchedIdx - optimizationRange[1]):(matchedIdx - optimizationRange[2])
	bestSupportIdx <- find_best_support_line(stockWindowData = fullData[selectedRows, ],  # TODO fix
			candidate_lines = allTrendlines, type = type)
	bestSupport <- allTrendlines[bestSupportIdx, ]
	
	return(bestSupport)
}







