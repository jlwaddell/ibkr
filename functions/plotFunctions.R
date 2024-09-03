





# function definition
vizTradeAndStrategy <- function(data, 
		dataList, fullDataList, 
		plotNum = 1, 
		stopLossMult = 4, profitTakeMult = 3.5, 
		omitTimepoints = NULL, includeADX = FALSE, 
		dataType = "normal", 
		ending = "day", timepointsBefore = NULL) {
	
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
	rawIdx <- which(names(dataList) == paste0(ticker, " ", from))
	rawData <- dataList[[rawIdx]]
	fullData <- fullDataList[[rawIdx]]
	
	# remove omitted timepoints
	if(!is.null(omitTimepoints)) {
		fullData <- fullData[-omitTimepoints, ]
		rawData <- rawData[-omitTimepoints, ]
		fullData$index <- 1:nrow(fullData)
	}
	
	
	# match the idx of the trade
	if(dataType == "normal") {
		matchedIdx <- which(fullData$time == tmpData$time[1])
		if(length(matchedIdx) == 0) {
			timeDiffs <- abs(as.numeric(hms(fullData$time) - hms(tmpData$time[1])))
			matchedIdx <- which(timeDiffs == min(timeDiffs))[1]
		}
	}
	
	# calculate start time
	if(!is.null(timepointsBefore)) {
		startIdx <- max(c(1, matchedIdx - timepointsBefore))
	} else 
		startIdx <- 1
	
	# calc endtime
	if(ending == "tradeStart") {
		endIdx <- matchedIdx
	} else if(ending == "tradeEnd") {
		endIdx <- calcTradeEndTime(tmpData, fullData = fullData, rowNum = 1, 
				stopLossMult = stopLossMult, profitTakeMult = profitTakeMult) 
	} else
		endIdx <- nrow(fullData)
	
	
	# subset fullData
	fullData <- fullData[startIdx:endIdx, ]
	fullData$index <- 1:nrow(fullData)
	
	# layout for secondPlot
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
				stopLossMult, ", profitTakeMult = ", profitTakeMult)
#		, ", Profit = ", round(sum(tmpData$Realized.P.L), 2) 
	}
	
	if(class(rawData)[1] == "data.frame")
		rawData <- as.xts(rawData)
	
	# define the base chart
	myChart <- chart_Series(rawData[startIdx:endIdx, ], name = plotTitle)
	print(myChart)
	
	
	# add volume
	fullData$volAdj <- fullData$Volume
	volThresh <- quantile(fullData$Volume, 0.99)
	fullData$volAdj[which(fullData$volAdj > volThresh)] <- volThresh
	
	volMinY <- par("usr")[3]
	volMaxY <- par("usr")[3] + 0.08 * (par("usr")[4] - par("usr")[3])
	volMaxHeight <- volMaxY - volMinY
	
	colVecVolume <- rep("white", nrow(fullData))
	colVecVolume[which(fullData$Close < fullData$Open)] <- "red"
	
	rect(xleft = fullData$index - 0.5, xright = fullData$index + 0.5, 
			ybottom = volMinY, 
			ytop = volMinY + (fullData$Volume / volThresh) * volMaxHeight, 
			col = colVecVolume)
	
	
	
	# match trade data index to fullData index
	if(dataType == "normal") {
		tmpData$xValue <- rep(-1000, nrow(tmpData))
		tmpData$tradeColor <- rep("", nrow(tmpData))
		substr(tmpData$time, 7, 8) <- "00"
		for(kTmp in 1:nrow(tmpData)) { # kTmp <- 1
			
			# starting x-value of the trade
			matchedIdx <- which(fullData$time == tmpData$time[kTmp])
			if(length(matchedIdx) == 0) {
				timeDiffsRaw <- as.numeric(hms(fullData$time) - hms(tmpData$time[kTmp]))
				if(all(timeDiffsRaw < -120)) {  # TODO look into
					matchedIdx <- NA
				} else {
					timeDiffs <- abs(as.numeric(hms(fullData$time) - hms(tmpData$time[kTmp])))
					matchedIdx <- which(timeDiffs == min(timeDiffs))[1]
				}
			}
			
			tmpData$xValue[kTmp] <- matchedIdx
			tmpData$tradeColor[kTmp] <- ifelse(tmpData$Quantity[kTmp] > 0, 
					oaColors("blue"), oaColors("red"))
			
			# calculate when the trade ends
			xright <- calcTradeEndTime(tmpData, fullData = fullData, rowNum = kTmp, 
					stopLossMult = stopLossMult, profitTakeMult = profitTakeMult)
			
			# plot profit/loss rectangles
			if(tmpData$Quantity[kTmp] > 0 & (kTmp %% 2) == 1) {
				
				if(matchedIdx > 20) {
					bestSupport <- calcSupportLine(fullData, type = "rising", 
							matchedIdx = matchedIdx)
					
					if(nrow(bestSupport) > 0) {
						segments(x0 = matchedIdx - 40, x1 = matchedIdx, 
								y0 = bestSupport$intercept + bestSupport$slope * (matchedIdx - 40), 
								y1 = bestSupport$intercept + bestSupport$slope * (matchedIdx), 
								lwd = 2)
					}
				}
				
				
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
				
				if(matchedIdx > 20) {
					bestSupport <- calcSupportLine(fullData, type = "falling", 
							matchedIdx = matchedIdx)
					
					if(nrow(bestSupport) > 0) {
						segments(x0 = matchedIdx - 40, x1 = matchedIdx, 
								y0 = bestSupport$intercept + bestSupport$slope * (matchedIdx - 40), 
								y1 = bestSupport$intercept + bestSupport$slope * (matchedIdx), 
								lwd = 2)
					}
				}
				
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
	fullData$OBV <- OBV(price = fullData$Close, volume = fullData$volAdj) # TODO remove
	scaledOBV <- (fullData$OBV - min(fullData$OBV)) / diff(range(fullData$OBV))
	scaledOBV <- min(fullData$Close) + scaledOBV * abs(diff(range(fullData$Close)))
	lines(x = 1:nrow(fullData), y = scaledOBV, lwd = 1.5)
	
	# pvt
	pvt <- fullData$pvt
	scaledPVT <- (pvt - min(pvt, na.rm = TRUE)) / diff(range(pvt, na.rm = TRUE))
	scaledPVT <- min(fullData$Close) + scaledPVT * 
			abs(diff(range(fullData$Close, na.rm = TRUE)))
	lines(x = 1:nrow(fullData), y = scaledPVT, lwd = 1.5, col = "orange")
	lines(x = 1:nrow(fullData), y = fullData$slidingScaledPVT, lwd = 1, col = oaColors("green"))
	
	# SAR
#	points(x = fullData$index, y = fullData$sar, pch = 19, cex = 0.5)
	
	# legend (can probably remove later)
#	if(dataType == "normal")
#		legend("topright", legend = c("BOT", "SLD"), col = c(oaColors("blue"), oaColors("red")), 
#				pch = 19)
	
	# add ADX plot
	if(includeADX) {
		
		op <- par(no.readonly = TRUE) # the whole list of settable par's.
		## do lots of plotting and par(.) calls, then reset:
		
		par(mai = c(0, 0.117, 0, 0.125))
		par(xaxs = "i")	
		
		
		ylim = c(0, 1)
		xlim = range(fullData$index-0.5)
		
		blankPlot(xlim = par("usr")[1:2], ylim = ylim)
		lines(x = fullData$index, y = fullData$rsquared)
		lines(x = fullData$index, y = fullData$rsquared30, col = gray(0.9))
		lines(x = fullData$index, y = fullData$rsquaredBest, col = "red")
		lines(x = fullData$index, y = fullData$choppiness, col = "blue")
		lines(x = fullData$index, y = fullData$choppiness30, col = "purple")
		lines(x = fullData$index, y = fullData$choppinessBest, col = "pink")
		
		segments(x0 = 0.5, x1 = nrow(fullData)+0.5, lwd = 2, y0 = 1)
		segments(x0 = 0.5, x1 = nrow(fullData)+0.5, lwd = 2, y0 = 0)
		segments(x0 = 0.5, x1 = nrow(fullData)+0.5, lwd = 2, y0 = 0.382)
		segments(x0 = 0.5, x1 = nrow(fullData)+0.5, lwd = 2, y0 = 0.9)
		
		addYlab(xlim = xlim, ylim = ylim)	
		
		par(op)
	}
	
}


addYlab <- function(xlim, ylim) {
	
	ygrid <- pretty(ylim)
	ygrid <- ygrid[ygrid > min(ylim)]
	ygrid <- ygrid[ygrid < max(ylim)]
	segments(y0 = ygrid[1], y1 = ygrid[length(ygrid)], 
			x0 = xlim[1] - 0.005 * diff(range(xlim)),  
			lwd = 2)
	text(x = xlim[1] - 0.01 * diff(range(xlim)),
			y = ygrid, adj = c(1, 0.5), labels = ygrid, cex = 0.5)
}








