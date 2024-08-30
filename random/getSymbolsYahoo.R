# TODO: Add comment
# 
# Author: Jason
###############################################################################


function(Symbols,env,return.class='xts',index.class="Date",
		from='2007-01-01',
		to=Sys.Date(),
		...,
		periodicity="daily",
		curl.options=list())
{
	importDefaults("getSymbols.yahoo")
	this.env <- environment()
	for(var in names(list(...))) {
		# import all named elements that are NON formals
		assign(var, list(...)[[var]], this.env)
	}
	if(!hasArg("adjust"))
		adjust <- FALSE
	
	default.return.class <- return.class
	default.from <- from
	default.to <- to
	
	mins <- c(1, 2, 5, 15, 30, 60, 90)
	min_vals <- paste0(rep(mins, 2), "m")
	names(min_vals) <- c(paste0(mins, "minutes"), paste0(mins, " minutes"))
	
	intervals <- c(daily = "1d", weekly = "1wk", monthly = "1mo", hourly = "1h", min_vals)
	
	default.periodicity <- match.arg(periodicity, names(intervals))
	
	if(!hasArg("verbose")) verbose <- FALSE
	if(!hasArg("auto.assign")) auto.assign <- TRUE
	if(!hasArg("warnings")) warnings <- TRUE
	
	handle <- .getHandle(curl.options)
	
	returnSym <- Symbols
	noDataSym <- NULL
	for(i in seq_along(Symbols)) {
		test <- try({
					return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
					return.class <- ifelse(is.null(return.class),default.return.class,
							return.class)
					periodicity <- getSymbolLookup()[[Symbols[[i]]]]$periodicity
					periodicity <- if(is.null(periodicity)) default.periodicity else periodicity
					
					# ensure valid periodicity
					p <- pmatch(periodicity, names(intervals))
					if(is.na(p))
						stop("periodicity must be one of: ", paste(intervals, collapse=", "))
					interval <- intervals[p]
					
					is.intraday <- !(interval %in% c("1d", "1wk", "1mo"))
					
					from <- getSymbolLookup()[[Symbols[[i]]]]$from
					from <- if(is.null(from)) default.from else from
					to <- getSymbolLookup()[[Symbols[[i]]]]$to
					to <- if(is.null(to)) default.to else to
					
					if(is.intraday) {
						from.date <- as.Date(from)
						to.date <- as.Date(to)
						n.days <- difftime(time1 = to.date, time2 = from.date, units = "days")
						if(n.days > 7) {
							from <- to.date - 7
							if(warnings) {
								warning(paste0(
												"Only a maximum of 7 days is allowed for querying intraday data",
												"data from 'yahoo'. Setting `from` to '", from, "'."
										), call. = FALSE)
							}
						}
					}
					
					from.posix <- quantmod:::.dateToUNIX(from)
					to.posix <- quantmod:::.dateToUNIX(to)
					
					Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
					Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
					if(verbose) cat("downloading ",Symbols.name,".....\n\n")
					
					yahoo.URL <- quantmod:::.yahooJsonURL(Symbols.name, from.posix, to.posix, interval)
					conn <- curl::curl(yahoo.URL, handle = handle)
					y <- jsonlite::fromJSON(conn)
					if (is.null(y$chart) || is.null(y$chart$result)) {
						stop("no data for", Symbols.name)
					}
					y <- y$chart$result
					
					ohlcv <- unlist(y$indicators$quote[[1]], recursive = FALSE)
					
					tz <- y$meta$exchangeTimezoneName
					idx <- .POSIXct(y$timestamp[[1]], tz = tz)
					if (!is.intraday) {
						idx <- as.Date(idx)
					}
					
					x <- xts(do.call(cbind, ohlcv), idx,
							src='yahoo', updated=Sys.time())
					
					fr <- OHLCV(x)
					cnames <- c("Open", "High", "Low", "Close", "Volume")
					if (!is.intraday) {
						fr <- merge(fr, adjusted = unlist(y$indicators$adjclose))
						cnames <- c(cnames, "Adjusted")
					}
					
					# convert column names to Initial Capitalization
					cn <- colnames(fr)
					substring(cn, 1, 1) <- toupper(substring(cn, 1, 1))
					colnames(fr) <- cn
					
					# warn about missing values
					if (any(is.na(fr)) && isTRUE(warnings)) {
						warning(Symbols.name, " contains missing values. Some functions will",
								" not work if objects contain missing values in the middle",
								" of the series. Consider using na.omit(), na.approx(),",
								" na.fill(), etc to remove or replace them.", call. = FALSE)
					}
					
					# re-order column names and prefix with symbol
					corder <- pmatch(substr(cnames, 1, 3), colnames(fr))
					fr <- fr[,corder]
					colnames(fr) <- paste(toupper(gsub("\\^","",Symbols.name)), cnames, sep=".")
					
					if(adjust) {
						# Adjustment algorithm by Joshua Ulrich
						fr <- adjustOHLC(fr, symbol.name=Symbols.name)
					}
					
					fr <- convert.time.series(fr=fr,return.class=return.class)
					if(is.xts(fr)) {
						if(!is.intraday) {
							tclass(fr) <- index.class
						}
					}
					
					Symbols[[i]] <- toupper(gsub('\\^','',Symbols[[i]]))
					returnSym[[i]] <- gsub('\\^', '', returnSym[[i]])
					
					if(auto.assign)
						assign(Symbols[[i]],fr,env)
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
	if(auto.assign)
		return(setdiff(returnSym, noDataSym))
	return(fr)
}
