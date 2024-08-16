loadTradeData <- function(filename, removeColumns = TRUE) {
	
	# identify starting row
	lines <- readLines(paste0("./Data/", filename))
	numRowsToSkip <- which(grepl("^Trades", lines))[1] - 1
	
	# read the data
	rawData <- read.csv(paste0("./Data/", filename), skip =  numRowsToSkip, header = TRUE)  # TODO the skip has to be updated every time, find a way to automate
	data <- rawData
	
	# remove non-trading rows
	idxRem <- which(data[, 1] == "Interest Accruals")[1]
	data <- data[-(idxRem:nrow(data)), ]
	data <- data[which(data$Header == "Data"), ]
	data <- data[which(data$Asset.Category == "Stocks"), ]
	
	# format columns as numeric
	data$Realized.P.L <- as.numeric(data$Realized.P.L)
	data$Quantity <- gsub(",", "", data$Quantity)
	data$Quantity <- as.numeric(data$Quantity) # TODO this can be derived when missing
	data$T..Price <- as.numeric(data$T..Price)
	data$C..Price <- as.numeric(data$C..Price)
	data$Proceeds <- as.numeric(data$Proceeds)
	data$Comm.Fee <- as.numeric(data$Comm.Fee)
	data$Basis <- as.numeric(data$Basis)
	data$MTM.P.L <- as.numeric(data$MTM.P.L)	
	
	# separate date and time
	dateTimes <- unlist(strsplit(data$Date.Time, ", "))
	days <- dateTimes[which((1:length(dateTimes) %% 2) == 1)]
	times <- dateTimes[which((1:length(dateTimes) %% 2) == 0)]
	
	data$day <- days
	data$time <- times
	
	# ordering
	data <- data[order(data$day), ]
	
	# add plot number. each unique ticker-date combination gets its own plot number
	data$plotNum <- rep(0, nrow(data))
	k <- 1
	for(i in 1:nrow(data)) {
		
		if(data$plotNum[i] == 0) {
			idx <- which(data$Symbol == data$Symbol[i] & 
							data$day == data$day[i])
			data$plotNum[idx] <- k
			k <- k + 1
		}
	}
	
	# remove unneccessary columns
	if(removeColumns) {
		columnsToRemove <- c("DataDiscriminator", "Code", "C..Price", "Basis", "MTM.P.L")
		rawData <- rawData[, -which(colnames(rawData) %in% columnsToRemove)]
	}
	
	
	
	return(data)
	
}



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
	
	
	# add Mid and index to full data
	fullData <- as.data.frame(get(x))
	colnames(fullData) <- c("Open", "High", "Low", "Close", "Volume")
	fullData$Mid <- (fullData$Open + fullData$Close) / 2
	fullData$index <- 1:nrow(fullData)
	
	# add true range and ATR
	fullData$atr <- fullData$trueRange <- rep(0, nrow(fullData))
	if(nrow(fullData) > 14) {
		for(i in 2:nrow(fullData)) {
			fullData$trueRange[i] <- max(c(fullData$High[i] - fullData$Low[i], 
							abs(fullData$High[i] - fullData$Close[i-1]), 
							abs(fullData$Low[i] - fullData$Close[i-1])
					)
			)
		}
		for(i in 15:nrow(fullData)) {
			fullData$atr[i] <- mean(fullData$trueRange[(i-13):i])
		}
	}
		
	
	# extract the time for the full data
	dateTimes <- unlist(strsplit(rownames(fullData), " "))
	days <- dateTimes[which((1:length(dateTimes) %% 2) == 1)]
	fullData$time <- dateTimes[which((1:length(dateTimes) %% 2) == 0)]
	
	
	# define the base chart
	myChart <- chart_Series(get(x)[-nrow(get(x)), ], name = paste0(ticker, ": ", from, ", ATR x ", atrMultiplier, 
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
	

	
#	# plot the ATR points over time
#	if(showATR & tmpData$Quantity[1] > 0) {   # TODO maybe generalize for all trades
#		points(x = fullData$index, 
#				y = fullData$Mid - (atrMultiplier) * fullData$atr)
#	} else if(showATR & tmpData$Quantity[1] < 0) {
#		points(x = fullData$index, 
#				y = fullData$Mid + (atrMultiplier) * fullData$atr)
#	}
	
	# plot the buys and sells as points
	points(x = tmpData$xValue, y = tmpData$T..Price, col = tmpData$tradeColor, 
			pch = 19, cex = 1.5)
	
	# legend (can probably remove later)
	legend("topright", legend = c("BOT", "SLD"), col = c(oaColors("blue"), oaColors("red")), 
			pch = 19)
	
}
