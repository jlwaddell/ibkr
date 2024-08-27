



# function definition
vizTrade <- function(data, plotNum = 1, 
		atrMultiplier = 4, showATR = TRUE) {
	
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
	
	
	# define the base chart
	myChart <- chart_Series(get(x)[-nrow(get(x)), ], name = paste0(ticker, ": ", tmpData$day[1],
					", ATR x ", atrMultiplier, 
					", Profit = ", round(sum(tmpData$Realized.P.L), 2)  ) )
	print(myChart)
	
	# match trade data index to fullData index
	tmpData$xValue <- rep(0, nrow(tmpData))
	tmpData$tradeColor <- rep("", nrow(tmpData))
	substr(tmpData$time, 7, 8) <- "00"
	for(kTmp in 1:nrow(tmpData)) { # kTmp <- 1
		matchedIdx <- which(fullData$time == tmpData$time[kTmp])
		tmpData$xValue[kTmp] <- matchedIdx
		tmpData$tradeColor[kTmp] <- ifelse(tmpData$Quantity[kTmp] > 0, oaColors("blue"), oaColors("red"))
		
		# plot ATR
		if(showATR & tmpData$Quantity[kTmp] > 0 & (kTmp %% 2) == 1) {
			points(x = tmpData$xValue[kTmp], y = tmpData$T..Price[kTmp] - atrMultiplier * fullData$atr[matchedIdx], 
					pch = 19, cex = 1)
		} else if(showATR & tmpData$Quantity[kTmp] < 0 & (kTmp %% 2) == 1) {
			points(x = tmpData$xValue[kTmp], y = tmpData$T..Price[kTmp] + atrMultiplier * fullData$atr[matchedIdx], 
					pch = 19, cex = 1)
		}
	}
	
	
	# plot the buys and sells as points
	points(x = tmpData$xValue, y = tmpData$T..Price, col = tmpData$tradeColor, 
			pch = 19, cex = 1.5)
	
	# legend (can probably remove later)
	legend("topright", legend = c("BOT", "SLD"), col = c(oaColors("blue"), oaColors("red")), 
			pch = 19)
	
}




# function definition
vizTradeAndStrategy <- function(data, plotNum = 1, 
		stopLossMult = 4, profitTakeMult = 3.5, 
		omitTimepoints = NULL, includeADX = FALSE, 
		periodicity = "1 minutes", 
		dataType = "normal") {
	
	# prep the tmpData object
	if(dataType == "train") {
		tmpData <- data[which(data$plotNum == plotNum), ]
		ticker <- tmpData$Ticker[1]
		from <- tmpData$Date[1]
	} else {
		tmpData <- data[which(data$plotNum == plotNum), ]
		tmpData$xValue <- rep(0, nrow(tmpData))
		tmpData$tradeColor <- rep("", nrow(tmpData))
		substr(tmpData$time, 7, 8) <- "00"
		ticker <- tmpData$Symbol[1]
		from <- tmpData$day[1]
	}
	
	# define the fullData object
	loadedData <- loadData(tmpData = tmpData, periodicity = periodicity, 
			dataType = dataType)
	fullData <- loadedData$full
	rawData <- loadedData$raw
	
	colnames(rawData)[1:5] <- c("x.Open", "x.High", "x.Low", "x.Close", "x.Volume")
	
	# remove omitted timepoints
	# TODO think about the interaction between removing timepoints and OBV
	if(!is.null(omitTimepoints)) {
		fullData <- fullData[-omitTimepoints, ]
		rawData <- rawData[-omitTimepoints, ]
		fullData$index <- 1:nrow(fullData)
	}
	
	
	# match the idx of the trade
	if(dataType == "normal")
		matchedIdx <- which(fullData$time == tmpData$time[1])
	
	if(includeADX) {
		layout.matrix <- matrix(c(1, 2), nrow = 2, ncol = 1)
		layout(mat = layout.matrix, 
				heights = c(2.2, 1))
	}
	
	
	# define plot titles
	if(dataType == "train") {
		plotTitle <- paste0(ticker, ": ", from)
	} else {
		plotTitle <- paste0(ticker, ": ", from, ", stopLossMult = ",
				stopLossMult, ", profitTakeMult = ", profitTakeMult, 
				", Profit = ", round(sum(tmpData$Realized.P.L), 2)  )
	}
	
	# define the base chart
	myChart <- chart_Series(rawData, name = plotTitle)
	print(myChart)
	
	
	# add volume
	fullData$volAdj <- fullData$Volume
	volThresh <- quantile(fullData$Volume, 0.99)
	fullData$volAdj[which(fullData$volAdj > volThresh)] <- volThresh
	
	volMinY <- par("usr")[3]
	volMaxY <- par("usr")[3] + 0.08 * (par("usr")[4] - par("usr")[3])
	volMaxHeight <- volMaxY - volMinY
	
	rect(xleft = fullData$index, xright = fullData$index + 1, 
			ybottom = volMinY, 
			ytop = volMinY + (fullData$volAdj / volThresh) * volMaxHeight)
	
	# match trade data index to fullData index
	if(dataType == "normal") {
		tmpData$xValue <- rep(0, nrow(tmpData))
		tmpData$tradeColor <- rep("", nrow(tmpData))
		substr(tmpData$time, 7, 8) <- "00"
		for(kTmp in 1:nrow(tmpData)) { # kTmp <- 1
			matchedIdx <- which(fullData$time == tmpData$time[kTmp]) # starting x-value of the trade
			tmpData$xValue[kTmp] <- matchedIdx
			tmpData$tradeColor[kTmp] <- ifelse(tmpData$Quantity[kTmp] > 0, oaColors("blue"), oaColors("red"))
			
			# calculate when the trade ends
			xright <- calcTradeEndTime(tmpData, fullData = fullData, rowNum = kTmp, 
					stopLossMult = stopLossMult, profitTakeMult = profitTakeMult)
			
			# plot ATR
			if(tmpData$Quantity[kTmp] > 0 & (kTmp %% 2) == 1) {
				
				rect(xleft = tmpData$xValue[kTmp], 
						xright = xright, 
						ybottom = tmpData$T..Price[kTmp], 
						ytop = tmpData$T..Price[kTmp] - stopLossMult * fullData$atr[matchedIdx],
						col = oaColors("red", alpha = 0.3), border = NA)
				rect(xleft = tmpData$xValue[kTmp], 
						xright = xright, 
						ybottom = tmpData$T..Price[kTmp], 
						ytop = tmpData$T..Price[kTmp] + profitTakeMult * fullData$atr[matchedIdx],
						col = oaColors("green", alpha = 0.3), border = NA)
				
			} else if(tmpData$Quantity[kTmp] < 0 & (kTmp %% 2) == 1) {
				rect(xleft = tmpData$xValue[kTmp], 
						xright = xright, 
						ybottom = tmpData$T..Price[kTmp], 
						ytop = tmpData$T..Price[kTmp] + stopLossMult * fullData$atr[matchedIdx],
						col = oaColors("red", alpha = 0.3), border = NA)
				rect(xleft = tmpData$xValue[kTmp], 
						xright = xright, 
						ybottom = tmpData$T..Price[kTmp], 
						ytop = tmpData$T..Price[kTmp] - profitTakeMult * fullData$atr[matchedIdx],
						col = oaColors("green", alpha = 0.3), border = NA)
			}
		}
		
		# plot the buys and sells as points
		points(x = tmpData$xValue, y = tmpData$T..Price, col = tmpData$tradeColor, 
				pch = 19, cex = 1.5)
	}
	
	# plot OBV
	scaledOBV <- (fullData$OBV - min(fullData$OBV)) / diff(range(fullData$OBV))
	scaledOBV <- min(fullData$Close) + scaledOBV * abs(diff(range(fullData$Close)))
	lines(x = 1:nrow(fullData), y = scaledOBV, lwd = 1.5)
	
	# pvt
	pvt <- calcPVT(close = fullData$Close, volume = fullData$Volume)
	scaledPVT <- (pvt - min(pvt)) / diff(range(pvt))
	scaledPVT <- min(fullData$Close) + scaledPVT * abs(diff(range(fullData$Close)))
	lines(x = 1:nrow(fullData), y = scaledPVT, lwd = 1.5, col = "orange")
	
	# SAR
#	points(x = fullData$index, y = fullData$sar, pch = 19, cex = 0.5)
	
	# legend (can probably remove later)
	if(dataType == "normal")
		legend("topright", legend = c("BOT", "SLD"), col = c(oaColors("blue"), oaColors("red")), 
				pch = 19)
	
	# add ADX plot
	if(includeADX) {
		
		yRange <- par("usr")[3:4]
		adxRange <- c(0, 50)
		diRange <- range(c(fullData$DIn, fullData$DIp), na.rm = TRUE)
		
		segments(x0 = 0, x1 = nrow(fullData), y0 = yRange[1] - 0.4 * diff(yRange))
		segments(x0 = 0, x1 = nrow(fullData), y0 = yRange[1] - 0.3 * diff(yRange), 
				col = gray(0.7))
		segments(x0 = 0, x1 = nrow(fullData), y0 = yRange[1] - 0.32 * diff(yRange), 
				col = gray(0.7))
		text(x = 0, y = yRange[1] - 0.29 * diff(yRange), 
				label = "ADX = 25", adj = c(0, 0), col = gray(0.7))
		points(x = fullData$index, y = yRange[1] - 0.4 * diff(yRange)  + 
						(fullData$DX / max(adxRange)) * 0.2 * diff(yRange), pch = 19)
		lines(x = fullData$index, y = yRange[1] - 0.4 * diff(yRange)  + 
						(fullData$DIp / max(diRange)) * 0.2 * diff(yRange), lwd = 1, col = oaColors("green"))
		lines(x = fullData$index, y = yRange[1] - 0.4 * diff(yRange)  + 
						(fullData$DIn / max(diRange)) * 0.2 * diff(yRange), lwd = 1, col = oaColors("red"))
		
	}
	
}









vizSingleTrade <- function(data, plotNum = 1, 
		stopLossMult = 4, profitTakeMult = 3.5, 
		minutesBefore = 40) {
	
	tmpData <- data[which(data$plotNum == plotNum), ]
	tmpData$xValue <- rep(0, nrow(tmpData))
	tmpData$tradeColor <- rep("", nrow(tmpData))
	substr(tmpData$time, 7, 8) <- "00"
	tmpData <- tmpData[1:2, ] # TODO fix this part
	ticker <- tmpData$Symbol[1]
	
	# define the fullData object
	loadedData <- loadData(tmpData = tmpData)
	fullData <- loadedData$full
	rawData <- loadedData$raw
	
	colnames(rawData)[1:5] <- c("x.Open", "x.High", "x.Low", "x.Close", "x.Volume")
	
	# match the idx of the trade
	matchedIdx <- which(fullData$time == tmpData$time[1])
	
	# calculate the start and end times
	startIdx <- max(c(1, matchedIdx - minutesBefore))
	
	endIdx <- calcTradeEndTime(tmpData, fullData = fullData, rowNum = 1, 
			stopLossMult = stopLossMult, profitTakeMult = profitTakeMult) # TODO continue here
	
	
	# calc volume
	fullData$volAdj <- fullData$Volume
	volThresh <- quantile(fullData$Volume, 0.95)
	fullData$volAdj[which(fullData$volAdj > volThresh)] <- volThresh
	
	# subset fullData
	fullData <- fullData[startIdx:endIdx, ]
	
	# regression
#	regressionData <- fullData[1:minutesBefore, ]
#	regModel <- lm(Close ~ index, data = regressionData)
#	summary(regModel)$r.squared
#	
#	# TODO robust regression?
#	corVolClose <- round(cor(fullData$Close[1:minutesBefore], fullData$volAdj[1:minutesBefore]), 2)
	
	# define the base chart
	myChart <- chart_Series(rawData[startIdx:endIdx],
			name = paste0(ticker, ": stopLossMult = ",
					stopLossMult, ", profitTakeMult = ", profitTakeMult)  
	)   
	print(myChart)
	
	
	# plot volume
	volMinY <- par("usr")[3]
	volMaxY <- par("usr")[3] + 0.08 * (par("usr")[4] - par("usr")[3])
	volMaxHeight <- volMaxY - volMinY
	
	rect(xleft = fullData$index - startIdx + 0.5, xright = fullData$index - startIdx + 1.5, 
			ybottom = volMinY, 
			ytop = volMinY + (fullData$volAdj / volThresh) * volMaxHeight)
	
	# match trade data index to fullData index
	tmpData$xValue <- rep(0, nrow(tmpData))
	tmpData$tradeColor <- rep("", nrow(tmpData))
	substr(tmpData$time, 7, 8) <- "00"
	for(kTmp in 1:nrow(tmpData)) { # kTmp <- 1
		matchedIdx <- which(fullData$time == tmpData$time[kTmp]) # starting x-value of the trade
		if(length(matchedIdx) == 0)
			matchedIdx <- nrow(fullData)
		tmpData$xValue[kTmp] <- matchedIdx
		tmpData$tradeColor[kTmp] <- ifelse(tmpData$Quantity[kTmp] > 0, oaColors("blue"), oaColors("red"))
		
		# plot ATR
		if(tmpData$Quantity[kTmp] > 0 & (kTmp %% 2) == 1) {
			
			# TODO add code for computing xright better
			
			rect(xleft = tmpData$xValue[kTmp], 
					xright = nrow(fullData), 
					ybottom = tmpData$T..Price[kTmp], 
					ytop = tmpData$T..Price[kTmp] - stopLossMult * fullData$atr[matchedIdx],
					col = oaColors("red", alpha = 0.3), border = NA)
			rect(xleft = tmpData$xValue[kTmp], 
					xright = nrow(fullData), 
					ybottom = tmpData$T..Price[kTmp], 
					ytop = tmpData$T..Price[kTmp] + profitTakeMult * fullData$atr[matchedIdx],
					col = oaColors("green", alpha = 0.3), border = NA)
			
		} else if(tmpData$Quantity[kTmp] < 0 & (kTmp %% 2) == 1) {
			rect(xleft = tmpData$xValue[kTmp], 
					xright = nrow(fullData), 
					ybottom = tmpData$T..Price[kTmp], 
					ytop = tmpData$T..Price[kTmp] + stopLossMult * fullData$atr[matchedIdx],
					col = oaColors("red", alpha = 0.3), border = NA)
			rect(xleft = tmpData$xValue[kTmp], 
					xright = nrow(fullData), 
					ybottom = tmpData$T..Price[kTmp], 
					ytop = tmpData$T..Price[kTmp] - profitTakeMult * fullData$atr[matchedIdx],
					col = oaColors("green", alpha = 0.3), border = NA)
		}
	}
	
	
	# plot the buys and sells as points
	points(x = tmpData$xValue, y = tmpData$T..Price, col = tmpData$tradeColor, 
			pch = 19, cex = 1.5)
	
	# plot OBV
	scaledOBV <- (fullData$OBV - min(fullData$OBV)) / diff(range(fullData$OBV))
	scaledOBV <- min(fullData$Close) + scaledOBV * abs(diff(range(fullData$Close)))
	lines(x = 1:nrow(fullData), y = scaledOBV, lwd = 1.5)
	
	# pvt
	pvt <- calcPVT(close = fullData$Close, volume = fullData$Volume)
	scaledPVT <- (pvt - min(pvt)) / diff(range(pvt))
	scaledPVT <- min(fullData$Close) + scaledPVT * abs(diff(range(fullData$Close)))
	lines(x = 1:nrow(fullData), y = scaledPVT, lwd = 1.5, col = "orange")
	
	# legend (can probably remove later)
	legend("topright", legend = c("BOT", "SLD"), col = c(oaColors("blue"), oaColors("red")), 
			pch = 19)
	
}
