createPlot2 <- function(){
	#Read Data
	data <- read.table('household_power_consumption.txt', header = TRUE, sep = ";")
	#Change Column Classes
	data[,1] <- as.Date(as.character(data[,1]), format = "%d/%m/%Y")
	data <- subset(data, Date == "2007-02-01" | Date == "2007-02-02")
	data[,2] <- as.POSIXct(strptime(paste(data[,1], data[,2]), "%Y-%m-%d %H:%M:%S"))
	data[,3:8] <- apply(data[,3:8],2,function(x) as.numeric(as.character(x)))
	#PLOT 2
	png(filename = 'plot2.png', width = 480, height = 480, units = 'px')
	with(data,plot(Time, Global_active_power, xlab = "", ylab = 'Global Active Power', type = 'l'))
	dev.off()
}