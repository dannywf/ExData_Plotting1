# Load energy consumption UC Irvine dataset into R

date_time <- function(date, time) {
  return (strptime(paste(date, time), "%d/%m/%Y %H:%M:%S"))
}

load_data <- function() {
  df <- read.table("household_power_consumption.txt",
                   header=TRUE,
                   sep=";",
                   colClasses=c("character", "character", rep("numeric",7)),
                   na="?")
  # convert date and time variables to Date/Time class
  df$Time <- strptime(paste(df$Date, df$Time), "%d/%m/%Y %H:%M:%S")
  df$Date <- as.Date(df$Date, "%d/%m/%Y")
  # only use data from the dates 2007-02-01 and 2007-02-02
  dates <- as.Date(c("2007-02-01", "2007-02-02"), "%Y-%m-%d")
  df <- subset(df, Date %in% dates)
  
  return(df)
  
}

# Plot Multiple Graphs

plot4 <- function(ds=NULL) {
  if(is.null(ds))
    ds <- load_ds()
  
  png("plot4.png", width=480, height=480)
  
  par(mfrow=c(2,2))
  # 1
  plot(ds$Time, ds$Global_active_power,
       type="l",
       xlab="",
       ylab="Global Active Power")
  # 2
  plot(ds$Time, ds$Voltage, type="l",
       xlab="datetime", ylab="Voltage")
  # 3
  plot(ds$Time, ds$Sub_metering_1, type="l", col="black",
       xlab="", ylab="Energy sub metering")
  lines(ds$Time, ds$Sub_metering_2, col="red")
  lines(ds$Time, ds$Sub_metering_3, col="blue")
  legend("topright",
         col=c("black", "red", "blue"),
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         lty=1,
         box.lwd=0)
  # 4
  plot(ds$Time, ds$Global_reactive_power, type="n",
       xlab="datetime", ylab="Global_reactive_power")
  lines(ds$Time, ds$Global_reactive_power)
  
  dev.off()
  
}