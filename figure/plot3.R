createPlot3 <- function(){
	#Read Data
	data <- read.table('household_power_consumption.txt', header = TRUE, sep = ";")
	#Change Column Classes
	data[,1] <- as.Date(as.character(data[,1]), format = "%d/%m/%Y")
	data <- subset(data, Date == "2007-02-01" | Date == "2007-02-02")
	data[,2] <- as.POSIXct(strptime(paste(data[,1], data[,2]), "%Y-%m-%d %H:%M:%S"))
	data[,3:8] <- apply(data[,3:8],2,function(x) as.numeric(as.character(x)))
	#Tidy Data
	data <- df
	data <- gather(data, key = "Type", value = "Sub_meter_value",Sub_metering_1:Sub_metering_3)
	#PLOT 3
	#Create and open png graphics device
	png(filename = 'plot3.png', width = 480, height = 480, units = 'px')
	with(data, plot(Time,Sub_meter_value,type = 'n',ylab = 'Energy sub metering'))
	#First plot is type line and uses Sub_metering_1 data only
	with(subset(data, Type == 'Sub_metering_1'),points(Time,Sub_meter_value,col = 'black', type = 'l'))
	#Second plot is type line and uses Sub_metering_2 data only
	with(subset(data, Type == 'Sub_metering_2'),points(Time,Sub_meter_value,col = 'red', type = 'l'))
	#Third plot is type line and uses Sub_metering_3 data only
	with(subset(data, Type == 'Sub_metering_3'),points(Time,Sub_meter_value,col = 'blue', type = 'l'))
	#Add lengend depicting color and data description
	legend('topright',col = c('black','red','blue'), legend = c('Sub_metering_1','Sub_metering_2','Sub_metering_3'), lty = 1)
	dev.off()
}