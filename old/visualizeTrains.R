





library(quantmod)
library(calendar)
library(oaColors)

data <- read.csv("./Data/Stock Train Data.csv")




# functionalizing
ticker <- "TTD"
action <- "BOT"
from <- "2024-08-09"
atrMultiplier <- 4



vizTrain <- function(ticker, action = "BOT", 
		from, atrMultiplier) {
	
	to <- ymd(from) + duration(1, "days")
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
#			assign(x,  value =  get(x)[1:(idx[1] - 1), ])
	}
		
	
	
	fullData <- cbind.data.frame(get(x), ATR(get(x), n = 14)$atr)
	colnames(fullData) <- c("Open", "High", "Low", "Close", "Volume", "atr")
	fullData$Mid <- (fullData$Open + fullData$Close) / 2
	fullData$index <- 1:nrow(fullData)
	
	
	myChart <- chart_Series(get(x), name = paste0(ticker, ": ", from, ", ATR x ", atrMultiplier))
	
	print(myChart)
	abline(v = 31, col = grey(0.7))
	abline(v = 61, col = grey(0.7))
	
	if(action == "BOT") {
		points(x = fullData$index, 
				y = fullData$Mid - (atrMultiplier * 1.6) * fullData$atr)
	} else if(action == "SLD") {
		points(x = fullData$index, 
				y = fullData$Mid + (atrMultiplier * 1.6) * fullData$atr)
	}
	
}


#i <- 1


pdf("stopLossExploration.pdf", width = 10, height = 8)
for(i in 1:nrow(data)) {  # i <- 9
	vizTrain(ticker = data$Ticker[i], from = data$Date[i], action = data$Action[i], 
			atrMultiplier = 4)
}
dev.off()



vizTrain(ticker = "CRWD", from = "2024-07-30", action = "SLD", 
		atrMultiplier = 4)










