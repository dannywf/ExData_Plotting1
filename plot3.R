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

# Plot Energy SubMetering

plot3 <- function(ds=NULL) {
  if(is.null(ds))
    ds <- load_data()
  
  png("plot3.png", width=480, height=480)
  
  plot(ds$Time, ds$Sub_metering_1, type="l", col="black",
       xlab="", ylab="Energy sub metering")
  lines(ds$Time, ds$Sub_metering_2, col="red")
  lines(ds$Time, ds$Sub_metering_3, col="blue")
  legend("topright",
         col=c("black", "red", "blue"),
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         lty=1)
  
  dev.off()
}