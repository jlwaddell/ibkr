# TODO: Add comment
# 
# Author: Jason
###############################################################################


library(IBrokers)

# Connect to Interactive Brokers Trader Workstation (TWS)
tws <- twsConnect()


contract <- twsEquity('DELL','SMART','ISLAND')

# by default retreives 30 days of daily data
reqHistoricalData(tws, Contract=contract, 
		endDateTime = "2024-08-20 23:59:59")

# Define contract for the stock you want to get data for
contract <- twsEquity("AAPL", "SMART", "ISLAND")

# Get historical intraday data with 2-minute intervals
# Note: reqHistoricalData requires your TWS to be running and connected
intraday_data <- reqHistoricalData(tws, 
		Contract = contract, 
		endDateTime = Sys.time(), 
		duration = "1 D", 
		barSize = "1 day", 
		whatToShow = "TRADES")

# Print or analyze the intraday data
print(intraday_data)
