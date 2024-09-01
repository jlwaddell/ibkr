
# Function to find the support line for stock prices
find_support_line <- function(stockData, interval) {
	# Extract the lows over the interval
	lows <- zoo::rollapply(stockData$Low, interval, min, align = "right", fill = NA)
	
	# Find indices of local minima
	min_indices <- which(!is.na(lows) & stockData$Low == lows)
	
	# Get the x (time) and y (prices) values for the regression
	x <- min_indices
	y <- stockData$Low[min_indices]
	
	# Check if there are enough points to fit a line
	if (length(x) >= 2) {
		# Perform linear regression to find the support line
		model <- lm(y ~ x)
		
		# Get the coefficients for the line
		intercept <- coef(model)[1]
		slope <- coef(model)[2]
		
		# Generate the support line values
		support_line <- intercept + slope * seq_along(stockData$Low)
	} else {
		# Not enough points to fit a line
		support_line <- rep(NA, length(stockData$Low))
	}
	
	# Return the computed support line
	return(support_line)
}


library(zoo)



# Calculate the support line
support_line <- find_support_line(stockData, interval)

# Print the support line
print(support_line)

# Plotting the prices and support line
plot(stockData$index, stockData$Low, type = "l", col = "black", lwd = 2, ylab = "Prices", xlab = "Date")
lines(stockData$index, support_line, col = "blue", lwd = 2)
legend("topright", legend = c("Low Prices", "Support Line"), col = c("black", "blue"), lty = 1, lwd = 2)