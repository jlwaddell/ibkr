library(tidyverse)
library(tidyquant)

stockData <- prices <- fullData[7:32, ]  # i = 1, AMD
stockData <- prices <- fullData[28:53, ]  # i = 9, UPST






# rising trendlines
risingTrendlines <- proposeRisingTrendlines(stockData = stockData)

bestSupportIdx <- find_best_support_line(stockData = stockData,  
		candidate_lines = risingTrendlines, type = "rising")
bestSupport <- risingTrendlines[bestSupportIdx, ]

prices %>%
		ggplot(aes(x = index, y = Close)) +
		geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
		geom_abline(intercept=risingTrendlines$intercept, slope=risingTrendlines$slope) +
		geom_abline(intercept=bestSupport$intercept, slope=bestSupport$slope, lwd = 2) +
		labs(title = paste(ticker, "Trendline Chart"), 
				y = "Price", x = "Date") 



# falling trendlines
fallingTrendlines <- proposeFallingTrendlines(stockData = stockData)

bestSupportIdx <- find_best_support_line(stockData = prices,  
		candidate_lines = fallingTrendlines, type = "falling")
bestSupport <- fallingTrendlines[bestSupportIdx, ]

prices %>%
		ggplot(aes(x = index, y = Close)) +
		geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
		geom_abline(intercept=fallingTrendlines$intercept, slope=fallingTrendlines$slope) +
		geom_abline(intercept=bestSupport$intercept, slope=bestSupport$slope, lwd = 2) +
		labs(title = paste(ticker, "Trendline Chart"), 
				y = "Price", x = "Date") 










