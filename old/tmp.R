# Load required package
library(zoo)

# Function to find the support line for stock prices
find_support_line <- function(prices, interval) {
	# Extract the lows over the interval
	lows <- zoo::rollapply(prices$Low, interval, min, align = "right", fill = NA)
	
	# Find indices of local minima
	min_indices <- which(!is.na(lows) & prices$Low == lows)
	
	# Get the x (time) and y (prices) values for the regression
	x <- min_indices
	y <- prices$Low[min_indices]
	
	# Check if there are enough points to fit a line
	if (length(x) >= 2) {
		# Perform linear regression to find the support line
		model <- lm(y ~ x)
		
		# Get the coefficients for the line
		intercept <- coef(model)[1]
		slope <- coef(model)[2]
		
		# Generate the support line values
		support_line <- intercept + slope * seq_along(prices$Low)
	} else {
		# Not enough points to fit a line
		support_line <- rep(NA, length(prices$Low))
	}
	
	# Return the computed support line
	return(support_line)
}




# Example stock prices data
stock_data <- data.frame(
		Date = as.Date('2023-08-01') + 0:14,
		Open = c(100, 101, 102, 103, 104, 105, 104, 103, 102, 101, 100, 102, 101, 103, 105),
		High = c(101, 102, 103, 104, 105, 106, 105, 104, 103, 102, 101, 103, 102, 104, 106),
		Low = c(99, 100.5, 101.2, 101.8, 104, 103, 103.6, 102.4, 100.8, 100.4, 99, 101, 100, 102, 104),
		Close = c(100, 101, 102, 103, 104, 105, 104, 103, 102, 101, 100, 102, 101, 103, 105)
)

# Set the interval over which to find the support line
interval <- 5

# Calculate the support line
support_line <- find_support_line(stock_data, interval)

# Print the support line
print(support_line)

# Plotting the prices and support line
plot(stock_data$Date, stock_data$Low, pch = 19,
		col = "black", lwd = 2, ylab = "Prices", xlab = "Date")
lines(stock_data$Date, support_line, col = "blue", lwd = 2)
legend("topright", legend = c("Low Prices", "Support Line"), col = c("black", "blue"), lty = 1, lwd = 2)
