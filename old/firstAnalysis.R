library(IBrokers)

#install.packages("xts_0.13.1.tar.gz", type = "source")

## Not run:
IBrokersRef() # IBrokers Reference Card in PDF viewer
tws <- twsConnect() # make a new connection to the TWS





end_date <- "20240814 16:00:00"
contract <- twsEquity(symbol = "SBUX", exch = "SMART",
		primary = "NASDAQ",
		currency = "USD")

data <- reqHistoricalData(tws, 
		contract, 
		endDateTime = end_date, 
		duration = "1 D", 
		barSize = "1 min", 
		whatToShow = "TRADES", 
		useRTH = 1)


reqHistoricalData(tws, getContract("EUR.USD"), whatToShow="BID")

