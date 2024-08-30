

# Install and load the httr package
#install.packages("httr")
library(httr)

# Set your Alpaca API key and secret
alpaca_key <- "your_alpaca_api_key"
alpaca_secret <- "your_alpaca_api_secret"

# Define the endpoint for the Alpaca API request
url <- "https://data.alpaca.markets/v2/stocks/AAPL/bars"

# Make an HTTP GET request to the Alpaca API
response <- GET(url, 
		add_headers(`APCA-API-KEY-ID` = alpaca_key, 
				`APCA-API-SECRET-KEY` = alpaca_secret),
		query = list(start = Sys.Date() - 1, 
				timeframe = "2Min"))

# Convert the response to a dataframe
intraday_data <- content(response, as = "parsed")
print(intraday_data)
