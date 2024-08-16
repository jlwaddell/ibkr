

# 01/16/2020 11:14 PM gets historical quotes for specified symbols at various frequencies
.proctime00 = proc.time()
library(IBrokers)
tws <- twsConnect()
print.contract.details = TRUE
get.mkt.data <- FALSE
get.hist.data <- FALSE
get.intraday.data <- TRUE
tws <- twsConnect()
symbols <- c("SPY","TLT")
nsym <- length(symbols)
# set the intervals for which data should be downloaded to TRUE
one.min <- FALSE
two.min <- FALSE
three.min <- FALSE
five.min <- FALSE
fifteen.min <- FALSE
thirty.min <- FALSE
hourly <- TRUE
if (get.mkt.data) print(reqMktData(tws, twsEquity(symbols[1])))
for (sym in symbols) {  # sym = "SPY"
	contract <- twsEquity(sym,'SMART','ISLAND')
	if (print.contract.details) print(reqContractDetails(tws, twsEquity(sym)))
	if (get.hist.data) {
		# by default retrieves 30 days of daily data
		xdaily <-  reqHistoricalData(tws, Contract=contract)
		print(xdaily)
	}
	if (get.intraday.data) {
		# by default retrieves a year of 1 minute bars
		if (one.min) {
			cat("\ngetting 1-minute data for ",sym,"\n")
			out.file <- paste("1_min_",sym,".csv",sep="")
			cat("\nwriting to",out.file,"\n")
			# was unable to download much more than 120 days of 1-minute bars, so duration set to 120 days
			xintra <- reqHistoricalData(tws, Contract=contract, barSize = "1 min",duration = "120 D", file=out.file)
			Sys.sleep(10) # mandatory 10s between request to avoid IB pacing violation
		}
		if (two.min) {
			cat("\ngetting 2-minute data for ",sym,"\n")
			out.file <- paste("2_min_",sym,".csv",sep="")
			cat("\nwriting to",out.file,"\n")
			xintra <- reqHistoricalData(tws, Contract=contract, barSize = "2 mins",duration = "1 Y", file=out.file)
			Sys.sleep(10) # mandatory 10s between request to avoid IB pacing violation
		}
		if (three.min) {
			cat("\ngetting 3-minute data for ",sym,"\n")
			out.file <- paste("3_min_",sym,".csv",sep="")
			cat("\nwriting to",out.file,"\n")
			xintra <- reqHistoricalData(tws, Contract=contract, barSize = "3 mins",duration = "1 Y", file=out.file)
			Sys.sleep(10) # mandatory 10s between request to avoid IB pacing violation
		}
		if (five.min) {
			cat("\ngetting 5-minute data for ",sym,"\n")
			out.file <- paste("5_min_",sym,".csv",sep="")
			cat("\nwriting to",out.file,"\n")
			xintra <- reqHistoricalData(tws, Contract=contract, barSize = "5 mins",duration = "1 Y", file=out.file)
			Sys.sleep(10) # mandatory 10s between request to avoid IB pacing violation
		}
		if (fifteen.min) {
			cat("\ngetting 15-minute data for ",sym,"\n")
			out.file <- paste("15_min_",sym,".csv",sep="")
			cat("\nwriting to",out.file,"\n")
			xintra <- reqHistoricalData(tws, Contract=contract, barSize = "15 mins",duration = "1 Y", file=out.file)
			Sys.sleep(10) # mandatory 10s between request to avoid IB pacing violation
		}
		if (thirty.min) {
			cat("\ngetting 30-minute data for ",sym,"\n")
			out.file <- paste("30_min_",sym,".csv",sep="")
			cat("\nwriting to",out.file,"\n")
			xintra <- reqHistoricalData(tws, Contract=contract, barSize = "30 mins",duration = "1 Y", file=out.file)
			Sys.sleep(10) # mandatory 10s between request to avoid IB pacing violation
		}
		if (hourly) {
			cat("\ngetting hourly data for ",sym,"\n")
			out.file <- paste("hourly_",sym,".csv",sep="")
			cat("\nwriting to",out.file,"\n")
			xintra <- reqHistoricalData(tws, Contract=contract, barSize = "1 hour", 
					duration = "1 Y", file=out.file)
			Sys.sleep(10) # mandatory 10s between request to avoid IB pacing violation
		}
	}
}
if (nsym > 0) {
	dtime <- (proc.time() - .proctime00)[3]
	cat("\n for #symbols =",nsym,"\n Time elapsed(s) (total,per_symbol): ",dtime,dtime/nsym,"\n")
}
