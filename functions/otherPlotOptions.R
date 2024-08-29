# TODO: Add comment
# 
# Author: Jason
###############################################################################


# ADX
if(includeADX) {
	
		yRange <- par("usr")[3:4]
		adxRange <- c(0, 50)
		diRange <- range(c(fullData$DIn, fullData$DIp), na.rm = TRUE)
		
		segments(x0 = 0, x1 = nrow(fullData), y0 = yRange[1] - 0.4 * diff(yRange))
		segments(x0 = 0, x1 = nrow(fullData), y0 = yRange[1] - 0.3 * diff(yRange), 
				col = gray(0.7))
		segments(x0 = 0, x1 = nrow(fullData), y0 = yRange[1] - 0.32 * diff(yRange), 
				col = gray(0.7))
		text(x = 0, y = yRange[1] - 0.29 * diff(yRange), 
				label = "ADX = 25", adj = c(0, 0), col = gray(0.7))
		points(x = fullData$index, y = yRange[1] - 0.4 * diff(yRange)  + 
						(fullData$DX / max(adxRange)) * 0.2 * diff(yRange), pch = 19)
		lines(x = fullData$index, y = yRange[1] - 0.4 * diff(yRange)  + 
						(fullData$DIp / max(diRange)) * 0.2 * diff(yRange), lwd = 1, col = oaColors("green"))
		lines(x = fullData$index, y = yRange[1] - 0.4 * diff(yRange)  + 
						(fullData$DIn / max(diRange)) * 0.2 * diff(yRange), lwd = 1, col = oaColors("red"))
		
		blankPlot(xlim = c(0, 1), ylim = c(0, 1))
	
		layout.matrix <- matrix(c(1, 2), nrow = 2, ncol = 1)
		layout(mat = layout.matrix, 
				heights = c(2.2, 1))
		
		print(myChart)
		segments(x0 = 0.5, x1 = nrow(fullData)+0.5, lwd = 2, y0 = par("usr")[3] )
	

}






# weird ATR modification
if(includeADX) {
	
	op <- par(no.readonly = TRUE) # the whole list of settable par's.
	## do lots of plotting and par(.) calls, then reset:
	
	par(mai = c(0, 0.117, 0, 0.125))
	par(xaxs = "i")	
	
	yValues <- c(fullData$pvt - fullData$unifPvt, fullData$pvt, fullData$unifPvt)
	
	
	if(length(yValues) > 18) {
		
		ylim = range(yValues, na.rm = TRUE)
		xlim = range(fullData$index-0.5)
		
		if(ylim[1] == "Inf" | ylim[1] == ylim[2])
			ylim = c(0, 1)
		
		blankPlot(xlim = par("usr")[1:2], ylim = ylim)
		segments(x0 = 0.5, x1 = nrow(fullData)+0.5, lwd = 2, y0 = ylim[1] )
		
		rect(xleft = fullData$index - 0.5, xright = fullData$index + 0.5, 
				ybottom = 0, ytop = fullData$pvt - fullData$unifPvt)
		
		points(x = fullData$index, y = fullData$pvt)
		lines(x = fullData$index, y = fullData$unifPvt)
		
		
		addYlab(xlim = xlim, ylim = ylim)
	} else {
		blankPlot(xlim = c(0, 1), ylim = c(0, 1))
	}
	
	
	par(op)
}

}