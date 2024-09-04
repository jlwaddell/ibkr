
i <- 35
matchedIdx <- 164




# create fake data
fullData <- fullDataList[[i]]
ticker <- strsplit(names(dataList)[i], " ")[[1]][1]
date <- strsplit(names(dataList)[i], " ")[[1]][2]
time <- fullData$time[matchedIdx]



fakeData <- data.frame(Symbol = ticker, Quantity = quantity, 
		T..Price = fullData$Close[matchedIdx], day = date, time = time, plotNum = 1)
vizTradeAndStrategy(data = fakeData, 
		dataList = dataList, 
		fullDataList = fullDataList, 
		plotNum = 1, 
		stopLossMult = stopLossMult, profitTakeMult = profitTakeMult, 
		omitTimepoints = 1, 
		includeADX = TRUE, 
		dataType = "normal", 
					ending = "tradeStart",
		timepointsBefore = 80,
		titleAddendum = paste0(", pl = ", round(pl, 3))
)





for(i in 1:50) {
	cat(i)
	if(i == 20)
		i <- i + 10
}


# Initialize 'i' for the for loop
for (i in 1:50) {
	cat("Current i:", i, "\n")
	
	# Simulate flipping a coin
	coin_flip <- sample(c("heads", "tails"), 1)
	cat("Coin flip result:", coin_flip, "\n")
	
	# If the coin flip is heads, increment i by 10 and use `next` to skip iterations
	if (coin_flip == "heads") {
		cat("Heads! Skipping ahead by 10.\n")
		
		# Increment 'i' by 10 manually
		i <- i + 10
		
		# Check if 'i' exceeds 50 and break out if needed
		if (i > 50) {
			break
		}
	}
}