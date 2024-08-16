
library(quantmod)
library(calendar)
library(oaColors)
#library(alphavantager)

# Load necessary library
library(lubridate)

# Define the year
year <- 2023

# Create a sequence of dates for the entire year
all_dates <- seq(ymd(paste0(year, "-01-01")), ymd(paste0(year, "-12-31")), by = "days")

# Filter out the Mondays
mondays <- all_dates[wday(all_dates) == 2]
fridays <- all_dates[wday(all_dates) == 6]

iusa <- list()
snp <- list()
for(i in 1:length(mondays)) { 
	getSymbols("IUSA.AS", src="yahoo", 
			periodicity = "hourly", 
			from = mondays[i], to = fridays[i])
	snp[[i]] <- getSymbols("^GSPC", auto.assign = FALSE,
			periodicity = "hourly", 
			from = mondays[i], to = fridays[i])
	iusa[[i]] <- IUSA.AS
}

iusa <- do.call("rbind.data.frame", iusa)
snp <- do.call("rbind.data.frame", snp)

idxRem <- which(grepl(rownames(snp), pattern = "2024"))
snp <- snp[-idxRem, ]

snp$Mid <- (snp$GSPC.Open + snp$GSPC.Close)/2

barChart(iusa)
barChart(snp)

# convert times
original_time <- ymd_hms(rownames(snp), tz = "America/New_York")
rownames(snp) <- with_tz(original_time, tzone = "Europe/Amsterdam")

time_points <- as.POSIXct(rownames(snp), tz = "Europe/Amsterdam")
hourly_data <- xts(snp$Mid, order.by = time_points)

interpolate_30min <- function(hourly_data) {
	# Generate 30-minute time points within the range of the hourly data
	time_points_30min <- seq.POSIXt(from = start(hourly_data),
			to = end(hourly_data),
			by = "30 mins")
	
	# Perform linear interpolation
	interpolated_values <- approx(index(hourly_data), coredata(hourly_data),
			xout = time_points_30min)$y
	
	# Create a new xts object with the interpolated 30-minute data
	interpolated_data <- xts(interpolated_values, order.by = time_points_30min)
	
	return(interpolated_data)
}

# TODO account for foreign exchange rate USD / EUR

# Use the interpolation function to create 30-minute data
interpolated_data <- interpolate_30min(hourly_data = hourly_data)

indexWithoutCET <- gsub(pattern = " CET", replacement = "", x = index(interpolated_data))
idTmp <- interpolated_data[which(indexWithoutCET %in% rownames(iusa)), ]
indexWithoutCETtmp <- gsub(pattern = " CET", replacement = "", x = index(idTmp))
iusaTmp <- iusa[which(rownames(iusa) %in% indexWithoutCETtmp), ] 
data <- cbind.data.frame(iusaTmp, idTmp)


mean(data[, 6]) / mean(data[, 1])
plot(data[, 6] / data[, 1]); abline(h = mean(data[, 6]) / mean(data[, 1]))
abline(v = c(340, 600, 740, 990, 1380))

plot(data[, 6], pch = 19)
points(data[, 1] * (mean(data[, 6]) / mean(data[, 1])), pch = 19, col = oaColors("blue"))

idxOver <- which((data[, 6] / data[, 1]) > 110)
idxUnder <- which((data[, 6] / data[, 1]) < 106.5)
colVec <- rep(oaColors("grey"), nrow(data))
colVec[idxOver] <- oaColors("blue")
colVec[idxUnder] <- oaColors("orange")

# 
plot(data[, 6], pch = 19)
points(data[, 1] * (mean(data[, 6]) / mean(data[, 1])), pch = 19, col = colVec)
abline(v = c(340, 600, 740, 990, 1380))

# blue: iusa undervalued
# orange: iusa overvalued










