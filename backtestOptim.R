
setwd("C:/Users/Jason/git/ibkr")
options(xts_check_TZ = FALSE)
library(quantmod)
library(calendar)
library(oaColors)
library(lubridate)
library(dplyr)
library(oaPlots)
library(tidyverse)
library(nloptr)
library(doParallel)
library(foreach)

load("./data/data2min.RData")
load("./data/fullDataList.RData")

functionFiles <- list.files("./functions")
for(iFile in 1:length(functionFiles)) {
	source(paste0("./functions/", functionFiles[iFile]))
}


backtestFun <- function(params, fullDataList, dataList) {
	
	# define variable parameters
	stopLossMult = params[1]
	profitTakeMult = params[2]
	
	# define fixed parameters
	r2Thresh <- params[3]   # .9
	chopThresh <- params[4]   # 0.382
	distanceFromTrendlineThreshold <- params[5] # 0.75
	prevDistFromTrendlineThresholdMult <- params[6] # 2
	
	prevDistFromTrendlineThreshold <- distanceFromTrendlineThreshold * 
			prevDistFromTrendlineThresholdMult # 2
	
	stopLossType = "fixed"
	breakevenTriggerMult = 1
	setBreakeven <- FALSE
	lookbackWindow <- 20
	minimumTimepoint <- 21
	
	plVec <- numeric()
	k <- 1
	
	for(i in 1:length(dataList)) {   # i <- 1
#	for(i in 1:5) {
		
#		cat(i)
		fullData <- fullDataList[[i]]
		
		# find the streaks and take the 2nd true
		tfVec <- (fullData$choppinessBest < chopThresh) & (fullData$rsquaredBest > r2Thresh)
		tfVec[which(is.na(tfVec))] <- FALSE  
		
		matchedIdxVec <- which(tfVec == TRUE)
		if(length(matchedIdxVec) > 0)
			matchedIdxVec <- matchedIdxVec[matchedIdxVec >= minimumTimepoint]
		
		while(jMatch <= length(matchedIdxVec)) {  # jMatch <- 1
			
			matchedIdx <- matchedIdxVec[jMatch]
			makeTrade <- TRUE
			
			# create fake data
			ticker <- strsplit(names(dataList)[i], " ")[[1]][1]
			date <- strsplit(names(dataList)[i], " ")[[1]][2]
			time <- fullData$time[matchedIdx]
			
			
			# BOT
			if(     
					fullData$stillMonotonic[matchedIdx] & 
					(fullData$Close[matchedIdx] > fullData$Close[matchedIdx - lookbackWindow]) & 
					!(is.na(fullData$slope[matchedIdx])) 
					) {
				
					# don't make the trade if the current Close is too far from the trendline (or under the trendline)
					if(fullData$distanceAboveTrendline[matchedIdx] > distanceFromTrendlineThreshold | 
							fullData$distanceAboveTrendline[matchedIdx] < 0)
						makeTrade <- FALSE
					
					# don't make the trade if the previous Close values were too far from the trendline
					if(fullData$precedingDistMax[matchedIdx] > prevDistFromTrendlineThreshold | 
							fullData$precedingDistMin[matchedIdx] < -0.5)
						makeTrade <- FALSE
			
				
				if(makeTrade) {
					
					if(stopLossType == "fixed") {
						pl <- compute_trade_result_fixed(fullData = fullData, matchedIdx = matchedIdx,
								stopLossMult = stopLossMult,
								profitTakeMult = profitTakeMult,
								action = "BOT")
						
						if(setBreakeven) {
							breakevenHappens <- checkBreakeven(fullData = fullData, matchedIdx = matchedIdx, 
									stopLossMult = stopLossMult,
									profitTakeMult = profitTakeMult,
									breakevenTriggerMult = breakevenTriggerMult,
									action = "BOT") 
							if(breakevenHappens)
								pl <- 0
						}
					} else if(stopLossType == "trailing") {
						pl <- compute_trade_result_trailing(fullData = fullData, matchedIdx = matchedIdx,
								stopLossMult = stopLossMult,
								profitTakeMult = profitTakeMult,
								action = "BOT")
					}
					
				}
				
				
			} else if(
					fullData$stillMonotonic[matchedIdx] & 
					(fullData$Close[matchedIdx] < fullData$Close[matchedIdx - lookbackWindow]) & 
					!(is.na(fullData$slope[matchedIdx]))   # trendline exists
					){  # SLD
				
				makeTrade <- TRUE
				quantity <- -1
				
				# don't make the trade if the current Close is too far from the trendline (or under the trendline)
				if(fullData$distanceAboveTrendline[matchedIdx] < (-1 * distanceFromTrendlineThreshold) |
						fullData$distanceAboveTrendline[matchedIdx] > 0)
					makeTrade <- FALSE
				
				# don't make the trade if the previous Close values were too far from the trendline
				if(fullData$precedingDistMin[matchedIdx] < (-1 * prevDistFromTrendlineThreshold) | 
						fullData$precedingDistMax[matchedIdx] > 0.5)
					makeTrade <- FALSE
				
				
				if(makeTrade) {
					
					if(stopLossType == "fixed") {
						pl <- compute_trade_result_fixed(fullData = fullData, 
								matchedIdx = matchedIdx,
								stopLossMult = stopLossMult,
								profitTakeMult = profitTakeMult,
								action = "SLD")
						
						if(setBreakeven) {
							breakevenHappens <- checkBreakeven(fullData = fullData,
									matchedIdx = matchedIdx, 
									stopLossMult = stopLossMult,
									profitTakeMult = profitTakeMult,
									breakevenTriggerMult = breakevenTriggerMult,
									action = "SLD") 
							if(breakevenHappens)
								pl <- 0
						}
					} else if(stopLossType == "trailing") {
						pl <- compute_trade_result_trailing(fullData = fullData, matchedIdx = matchedIdx,
								stopLossMult = stopLossMult,
								profitTakeMult = profitTakeMult,
								action = "SLD")
						
					}
					
				}			
			}  # close SLD bracket  
			
			if(makeTrade) {				
				plVec[k] <- pl
				k <- k + 1
				
				nextJMatch <- which(matchedIdxVec > matchedIdx + 20)
				if(length(nextJMatch) == 0) {
					jMatch <- length(matchedIdxVec) + 1
				} else 
					jMatch <- nextJMatch[1]
			} else 
				jMatch <- jMatch + 1
			
			
		}  # close while jMatch
		
		
	}  # close i loop
	
	output <- -1 * (mean(plVec) / (sd(plVec) / sqrt(length(plVec))))
	
	return(output)
}




#iteration: 24
#x = ( 5.500000, 5.500000, 0.491667, 0.350000, 1.000000, 1.333333 )
#f(x) = 2.113949

#iteration: 19
#x = ( 3.128661, 5.004120, 0.928397, 0.521018, 0.999210, 2.275515 )
#f(x) = -4.546813

#iteration: 26
#x = ( 1.944818, 6.700756, 0.805181, 0.399194, 1.331663, 2.691460 )
#f(x) = -2.125836

#iteration: 158
#x = ( 3.815768, 6.712687, 0.400000, 0.406327, 1.129130, 1.327209 )
#f(x) = -2.682443

#iteration: 199
#x = ( 2.004351, 5.737231, 0.411288, 0.390275, 1.858566, 2.761847 )
#f(x) = -2.065046

#iteration: 176
#x = ( 2.745138, 6.885039, 0.500701, 0.415268, 1.105526, 1.483471 )
#f(x) = -2.339834


# Define search ranges and starting values for parameters
param_ranges <- list(
		stopLossMult = c(1, 4),   
		profitTakeMult = c(3, 10),    
		r2Thresh = c(0.4, 0.95),
		chopThresh = c(0.3, 0.6), 
		distanceFromTrendlineThreshold = c(0.2, 2), 
		prevDistFromTrendlineThresholdMult = c(1.2, 3)
)
start_vals <- c(stopLossMult = 2.5,
		profitTakeMult = 7,    
		r2Thresh = 0.5,
		chopThresh = 0.4, 
		distanceFromTrendlineThreshold = 1.44, 
		prevDistFromTrendlineThresholdMult = 1.5)  # Starting values for parameters




# without parallel
lower_bounds <- sapply(param_ranges, `[`, 1)  # Lower bounds for each parameter
upper_bounds <- sapply(param_ranges, `[`, 2)

opt <- nloptr(
		x0 = start_vals,
		eval_f = function(params) backtestFun(params, 
					fullDataList = fullDataList, dataList = dataList),  # Pass additional parameters to the function
		lb = lower_bounds,
		ub = upper_bounds,
		opts = list(
#				algorithm = "NLOPT_LN_BOBYQA",  # Local derivative-free algorithm
#				algorithm = "NLOPT_GN_DIRECT",   # global rectangularization
				algorithm = "NLOPT_GN_CRS2_LM",
				maxeval = 200,  # Maximum number of function evaluations
				print_level = 3  # output during optimization
		)
)


result <- opt$solution # Return optimized solution


# Find the best solution from all parallel runs
best_solution <- result[which.max(sapply(1:nrow(result),
						function(i) fn(result[i, ], ...))), ]





















# Set up parallel backend
#num_cores <- detectCores() - 2  # Use all cores except one for safety
#cl <- makeCluster(num_cores)
#registerDoParallel(cl)
#clusterExport(cl, c("calcSupportLine", "proposeFallingTrendlines", "proposeRisingTrendlines", 
#		"find_best_support_line", "find_local_minmaxs", "bind_cols", "map_dfr", "map2", 
#		"compute_trade_result_fixed", "checkBreakeven", "compute_trade_result_trailing", 
#		"%>%"))

## Define the parallel optimization function
optimize_parallel <- function(fn, param_ranges, start_vals, ...) {
	# Create parameter bounds for optimization
	lower_bounds <- sapply(param_ranges, `[`, 1)  # Lower bounds for each parameter
	upper_bounds <- sapply(param_ranges, `[`, 2)  # Upper bounds for each parameter
	
	# Perform optimization in parallel
	result <- foreach(i = 1:num_cores, .combine = rbind, .packages = "nloptr") %dopar% {
		# Randomize the starting values for each parallel run
		init_vals <- sapply(seq_along(start_vals), function(j) runif(1, lower_bounds[j], upper_bounds[j]))
		
		# Run the optimization with additional parameters passed using '...'
		opt <- nloptr(
				x0 = init_vals,
				eval_f = function(params) fn(params, ...),  # Pass additional parameters to the function
				lb = lower_bounds,
				ub = upper_bounds,
				opts = list(
						algorithm = "NLOPT_LN_BOBYQA",  # Local derivative-free algorithm
						maxeval = 10,  # Maximum number of function evaluations
						print_level = 2  # No output during optimization
				)
		)
		
		return(opt$solution)  # Return optimized solution
	}
	
	# Find the best solution from all parallel runs
	best_solution <- result[which.max(sapply(1:nrow(result), function(i) fn(result[i, ], ...))), ]
	
	return(best_solution)
}
#
#
## Run the parallel optimization with additional parameters
#best_params <- optimize_parallel(backtestFun, param_ranges, start_vals, 
#		fullDataList = fullDataList, dataList = dataList)
#
## Stop the parallel backend
#stopCluster(cl)
#
#print(best_params)