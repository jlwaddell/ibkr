

getSymbolsAV <- function(Symbols, env=parent.frame(), api.key,
		return.class="xts",
		periodicity="daily",
		adjusted=FALSE,
		month="2024-07",
		interval="1min",
		output.size="full",
		data.type="json",
		...)
{
	importDefaults("getSymbols.av")
	this.env <- environment()
	for (var in names(list(...))) {
		assign(var, list(...)[[var]], this.env)
	}
	
	if (!hasArg("api.key")) {
		stop("getSymbols.av: An API key is required (api.key). Free registration",
				" at https://www.alphavantage.co/.", call.=FALSE)
	}
	if (!hasArg("auto.assign")) auto.assign <- TRUE
	if (!hasArg("verbose")) verbose <- FALSE
	if (!hasArg("warnings")) warnings <- TRUE
	
	valid.periodicity <- c("daily", "weekly", "monthly", "intraday")
	periodicity <- match.arg(periodicity, valid.periodicity)
	interval <- match.arg(interval, c("1min", "5min", "15min", "30min", "60min"))
	output.size <- match.arg(output.size, c("compact", "full"))
	
	default.return.class <- return.class
	default.periodicity <- periodicity
	
	#
	# For daily, weekly, and monthly data, timestamps are "yyyy-mm-dd".
	# For intraday data, timestamps are "yyyy-mm-dd HH:MM:SS".
	#
	convertTimestamps <- function(ts, periodicity, tz) {
		if (periodicity == "intraday")
			as.POSIXct(ts, tz=tz)
		else
			as.Date(ts)
	}
	
	downloadOne <- function(sym, default.return.class, default.periodicity) {
		
		return.class <- getSymbolLookup()[[sym]]$return.class
		return.class <- if (is.null(return.class)) default.return.class else return.class
		
		periodicity <- getSymbolLookup()[[sym]]$periodicity
		periodicity <- if (is.null(periodicity)) default.periodicity else periodicity
		periodicity <- match.arg(periodicity, valid.periodicity)
		
		if (adjusted && periodicity == "intraday")
			stop("getSymbols.av: Intraday data cannot be adjusted.", call.=FALSE)
		
		sym.name <- getSymbolLookup()[[sym]]$name
		sym.name <- if (is.null(sym.name)) sym else sym.name
		
		FUNCTION <- paste0("TIME_SERIES_",
				switch(periodicity,
						daily = if (adjusted) "DAILY_ADJUSTED" else "DAILY",
						weekly = if (adjusted) "WEEKLY_ADJUSTED" else "WEEKLY",
						monthly = if (adjusted) "MONTHLY_ADJUSTED" else "MONTHLY",
						intraday = "INTRADAY" ))
		
		if (verbose) cat("loading", sym.name, ".....")
		
		URL <- paste0("https://www.alphavantage.co/query",
				"?function=", FUNCTION,
				"&symbol=", sym.name,
				"&interval=", interval,
				"&month=", month,
				"&outputsize=", output.size,
				"&datatype=", data.type,
				"&apikey=", api.key)
		
		if (data.type == "json") {
			lst <- jsonlite::fromJSON(URL)
			
			#
			# Errors return a list with one element: An error message
			#
			if (length(lst) == 1)
				stop("getSymbols.av: ", lst[[1]], call.=FALSE)
			
			if (verbose) cat("done.\n")
			
			#
			# The first element of 'lst' is the metadata.
			# Typical metadata (in JSON format):
			#
			#   "Meta Data": {
			#     "1. Information": "Intraday (1min) prices and volumes",
			#     "2. Symbol": "MSFT",
			#     "3. Last Refreshed": "2017-05-23 16:00:00",
			#     "4. Interval": "1min",
			#     "5. Output Size": "Compact",
			#     "6. Time Zone": "US/Eastern"
			#   }
			#
			meta <- lst[[1]]
			tz <- meta[["6. Time Zone"]]
			updated <- convertTimestamps(meta[["3. Last Refreshed"]], periodicity, tz=tz)
			
			#
			# The second element of 'lst' is the data: a list.
			# The names of the list elements are the timestamps.
			# Typical list element, non-adjusted data (in JSON format):
			#
			#   "2017-05-23": {
			#     "1. open": "68.6750",
			#     "2. high": "68.7100",
			#     "3. low": "68.6400",
			#     "4. close": "68.6800",
			#     "5. volume": "1591941"
			#   }
			#
			# Typical list element, adjusted data (again, JSON format):
			#
			#  "2017-06-30": {
			#    "1. open": "68.7800",
			#    "2. high": "69.3800",
			#    "3. low": "68.7400",
			#    "4. close": "68.9300",
			#    "5. adjusted close": "68.9300",
			#    "6. volume": "23039328",
			#    "7. dividend amount": "0.00",
			#    "8. split coefficient": "1.0000"
			#   },
			#
			elems <- lst[[2]]
			tm.stamps <- convertTimestamps(names(elems), periodicity, tz=tz)
			
			if (adjusted) {
				av_names <- c("1. open", "2. high", "3. low", "4. close", "6. volume", "5. adjusted close")
				qm_names <- paste(sym, c("Open", "High", "Low", "Close", "Volume", "Adjusted"), sep=".")
			} else {
				av_names <- c("1. open", "2. high", "3. low", "4. close", "5. volume")
				qm_names <- paste(sym, c("Open", "High", "Low", "Close", "Volume"), sep=".")
			}
			
			# extract columns from each element (row) and unlist to a vector
			rows <- lapply(elems, function(x) unlist(x[av_names], use.names=FALSE))
			rows <- do.call(rbind, rows)
			colnames(rows) <- qm_names
			storage.mode(rows) <- "numeric"
			# convert matrix to xts
			mat <- xts(rows, tm.stamps, src="alphavantage", updated=updated)
			mat <- quantmod:::convert.time.series(mat, return.class=return.class)
		} else {
			mat <- as.xts(read.zoo(curl::curl(URL), header=TRUE, sep=","),
					src="alphavantage", updated=Sys.time())
			# convert column names to symbol.series
			cn <- colnames(mat)
			cn <- paste0(toupper(substring(cn, 1, 1)), substring(cn, 2))
			colnames(mat) <- paste(sym, cn, sep=".")
			
			mat <- quantmod:::convert.time.series(mat, return.class=return.class)
		}
		if (auto.assign)
			assign(sym, mat, env)
		return(mat)
	}
	
	returnSym <- Symbols
	noDataSym <- NULL
	matrices <- list()
	
	for(i in seq_along(Symbols)) {
		test <- try({
					matrices[[i]] <- downloadOne(Symbols[[i]],
							default.return.class = default.return.class,
							default.periodicity = default.periodicity)
				}, silent = TRUE)
		if (inherits(test, "try-error")) {
			msg <- paste0("Unable to import ", dQuote(returnSym[[i]]),
					".\n", attr(test, "condition")$message)
			if (hasArg(".has1sym.") && match.call(expand.dots=TRUE)$.has1sym.) {
				stop(msg)
			}
			if (isTRUE(warnings)) {
				warning(msg, call. = FALSE, immediate. = TRUE)
			}
			noDataSym <- c(noDataSym, returnSym[[i]])
		}
	}
	
	if (auto.assign) {
		return(setdiff(returnSym, noDataSym))
	} else {
		return(matrices[[1]])
	}
}
