createPlot4 <- function(){
	#Read Data
	data <- read.table('household_power_consumption.txt', header = TRUE, sep = ";")
	#Change Column Classes
	data[,1] <- as.Date(as.character(data[,1]), format = "%d/%m/%Y")
	#Select records where date equals '2007-02-01' or '2007-02-02'
	data <- subset(data, Date == "2007-02-01" | Date == "2007-02-02")
	data[,2] <- as.POSIXct(strptime(paste(data[,1], data[,2]), "%Y-%m-%d %H:%M:%S"))
	#Convert factor to numeric
	data[,3:8] <- apply(data[,3:8],2,function(x) as.numeric(as.character(x)))
	#Tidy Data
	data <- df
	data <- gather(data, key = "Type", value = "Sub_meter_value",Sub_metering_1:Sub_metering_3)
	#Create and open png graphics device
	png(filename = 'plot4.png', width = 480, height = 480, units = 'px')
	#2 by 2 graph canvas
	par(mfrow = c(2,2))
	#PLOT 1
	with(subset(data, Type == "Sub_metering_1"), plot(Time, Global_active_power, xlab = "" ,ylab = 'Global Active Power', type = 'l'))
	#PLOT 2
	with(subset(data, Type == "Sub_metering_1"), plot(Time, Voltage, xlab = "datetime", type = 'l'))
	#PLOT 3
	with(data, plot(Time,Sub_meter_value,type = 'n',ylab = 'Energy sub metering'))
	with(subset(data, Type == 'Sub_metering_1'),points(Time,Sub_meter_value,col = 'black', type = 'l'))
	with(subset(data, Type == 'Sub_metering_2'),points(Time,Sub_meter_value,col = 'red', type = 'l'))
	with(subset(data, Type == 'Sub_metering_3'),points(Time,Sub_meter_value,col = 'blue', type = 'l'))
	#Add legend 
	legend('topright',col = c('black','red','blue'), legend = c('Sub_metering_1','Sub_metering_2','Sub_metering_3'), lty = 1)
	with(subset(data, Type == 'Sub_metering_2'),plot(Time, Global_reactive_power, xlab = "datetime", ylab = 'Global Reactive Power', type = 'l'))
	dev.off()
}