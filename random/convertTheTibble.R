



# filteredTimes
times2min <- filtered_times[which((1:length(filtered_times) %% 2) == 1)]
i <- 106


dataListNew <- dataList
for(i in 108:length(dataList)) {
	
	x <- dataList[[i]]
	
	date <- strsplit(names(dataList)[i], " ")[[1]][2]
	newRownames <- paste(date, times2min)[1:nrow(x)]
#	rownames(x) <- newRownames
	
	dataList[[i]] <- xts(x, order.by = as.POSIXct(newRownames, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
}























