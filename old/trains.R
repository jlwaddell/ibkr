

library(quantmod)
library(calendar)
library(oaColors)


getSymbols("ASTS", src="yahoo", 
		periodicity = "1 minutes", 
		from = "2024-08-09", to = "2024-08-10")

ASTS
barChart(ASTS)
addATR()


as.data.frame(ASTS)
ATR(ASTS, n = 14)




# functionalizing
ticker <- "TTD"
action <- "BOT"
from <- "2024-08-09"
atrMultiplier <- 4
to <- ymd(date) + duration(1, "days")


x <- getSymbols(ticker, src="yahoo", 
		periodicity = "1 minutes", 
		from = from, to = to)

ATR(get(x), n = 14)

fullData <- cbind.data.frame(get(x), ATR(get(x), n = 14)$atr)
colnames(fullData) <- c("Open", "High", "Low", "Close", "Volume", "atr")
fullData$Mid <- (fullData$Open + fullData$Close) / 2
fullData$index <- 1:nrow(fullData)

#barChart(get(x), type = "candlesticks", TA = NULL, 
#		theme = chartTheme("white"))

myChart <- chart_Series(get(x))

# change ylim
#ylim <- myChart$get_ylim()
#ylim[[2]] <- structure(c(20, 25), fixed = TRUE)  # TODO generalize
#myChart$set_ylim(ylim)

myChart
abline(v = 31, col = grey(0.7))
abline(v = 61, col = grey(0.7))

if(action == "BOT") {
	points(x = fullData$index, 
			y = fullData$Mid - atrMultiplier * fullData$atr)
} else if(action == "SLD") {
	points(x = fullData$index, 
			y = fullData$Mid + atrMultiplier * fullData$atr)
}







