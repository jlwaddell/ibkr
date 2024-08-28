
loadData <- function(tmpData, periodicity = "1 minutes", 
		dataType = "normal") {
	
	if(dataType == "train") {
		from <- tmpData$Date[1]
		from <- ymd(from) + duration(0, "days")
		to <- ymd(from) + duration(1, "days")
		ticker <- tmpData$Ticker[1]
	} else {
		from <- tmpData$day[1]
		to <- ymd(from) + duration(1, "days")
		ticker <- tmpData$Symbol[1]
	}
	
	x <- getSymbols(ticker, src="yahoo", 
			periodicity = periodicity, 
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
	
	if(ticker == "LOW")
		colnames(LOW) <- c("x.Open", "x.High", "x.Low", "x.Close", "x.Volume")
	
	fullData <- formatFullData(input = get(x))
	rawData <- get(x)
	rawData <- rawData[-nrow(rawData), ]
	
	return(list(full = fullData, raw = rawData))
}



formatTmpData <- function(data, plotNum) {
	tmpData <- data[which(data$plotNum == plotNum), ]
	tmpData$xValue <- rep(0, nrow(tmpData))
	tmpData$tradeColor <- rep("", nrow(tmpData))
	substr(tmpData$time, 7, 8) <- "00"
	
	return(tmpData)
}



formatFullData <- function(input) {
	
	# add Mid and index to full data
	fullData <- as.data.frame(input)
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
	
	# OBV
	fullData$OBV <- OBV(price = Cl(input), volume = Vo(input))
	
	# MACD
	macdObj <- MACD(x = Cl(input), maType = "EMA", percent = TRUE, 
			nFast = 6, nSlow = 13, nSig = 3) # TODO explore other numbers
	fullData$MACD <- macdObj$macd
	fullData$signal <- macdObj$signal
	
	# ADX
	fullData <- cbind.data.frame(fullData, ADX(input, maType = "EMA", n = 15))
	
	# Parabolic SAR
	fullData$sar <- SAR(fullData[, c("High", "Low")], accel = c(0.01, 0.1))$sar
	
	# sliding PVT
	fullData$slidingScaledPVT <- rep(NA, nrow(fullData))
	for(i in 10:nrow(fullData)) {
		# TODO need to find a real-time way of adjusting volume
		pvt <- calcPVT(close = fullData$Close[1:i], volume = fullData$Volume[1:i]) 
		scaledPVT <- (pvt - min(pvt)) / diff(range(pvt))
		scaledPVT <- min(fullData$Close[1:i]) + scaledPVT * abs(diff(range(fullData$Close[1:i])))
		fullData$slidingScaledPVT[i] <- scaledPVT[i]
	}
	
	# uniform PVT (PVT if volume was uniform), PVT, scaledPVT
    unifVolume <- rep(mean(fullData$Volume), nrow(fullData))
	fullData$pvt <- calcPVT(close = fullData$Close, volume = fullData$Volume)
	fullData$unifPvt <- calcPVT(close = fullData$Close, volume = unifVolume)
	
	# remove the weird last row
	fullData <- fullData[-nrow(fullData), ]

	
	return(fullData)
}


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