## Script for creating and saving to a file in PNG format a plotting of
## time against "Energy sub metering 1, 2 and 3" information using
## data from the dates 2007-02-01 and 2007-02-02 of the “Individual
## household electric power consumption Data Set”, available from the
## UC Irvine Machine Learning Repository. This is part of Week 1
## project of the "Exploratory Data Analysis" course from Coursera's
## Data Science Specialization.
##
## Data set files: Electric power consumption
##     paste0("https://d396qusza40orc.cloudfront.net/",
##     "exdata%2Fdata%2Fhousehold_power_consumption.zip")
##
## Author: Sergio Rogelio Tinoco-Martínez
## Version: February 2016.
## Preconditions:
##     1) This script can be run as long as the “Individual
##     household electric power consumption Data Set” is in the working
##     directory.
##     2) PNG file will be generated as an image of 480x480 pixels.
## Main function: plot3().


## Reads the “Individual household electric power consumption Data
## Set”.
readData <- function() {
        read.table("./household_power_consumption.txt", header = TRUE,
                   sep = ";", stringsAsFactors = FALSE)
}


## Converts string variables 'Date' and 'Time' in a dataset to the
## corresponding R classes 'Date' and 'POSIXlt/POSIXt
strDateAndTimeToClasses <- function(dataset) {
        dataset$Time <- strptime(paste(dataset$Date, dataset$Time),
                                 "%d/%m/%Y %H:%M:%S")
        dataset$Date <- as.Date(dataset$Date, "%d/%m/%Y")
        dataset
}


## Filters a dataset by a time interval on the 'Date' variable
##     dataset: Data to be filtered
##     begin: Beginning date of the time interval to filter
##     end: End date of the time interval to filter
## Precondition: Format for both date parameters must be of the form
##     "%Y-%m-%d" (year in 4 digits + '-' + month in 2 digits + '-' +
##     day in 2 digits).
filterByDate <- function(dataset, begin, end) {
        beginDate <- as.Date(begin, "%Y-%m-%d")
        endDate <- as.Date(end, "%Y-%m-%d")
        
        subset(dataset, dataset$Date >= beginDate &
                       dataset$Date <= endDate)
}


createPNGplot <- function(dataset) {
        ## Set locale in order to get names of days in english
        Sys.setlocale("LC_TIME", "en_US.utf8")
        
        png(filename = "plot3.png", width = 480, height = 480,
            units = "px")
        par(bg = "transparent")
        
        ## Sub_metering_1 has the top value for 'y' axis, so,
        ## it sets the vertical scale
        plot(dataset$Time, dataset$Sub_metering_1, type = "n",
             xlab = NA, ylab = "Energy sub metering")
        
        with(dataset, lines(Time, Sub_metering_1, col = "black"))
        with(dataset, lines(Time, Sub_metering_2, col = "red"))
        with(dataset, lines(Time, Sub_metering_3, col = "blue"))
        
        legend("topright", lty = 1, col = c("black", "red", "blue"),
               legend = c("Sub_metering_1", "Sub_metering_2",
                          "Sub_metering_3"))
        dev.off()
}


## Creates and saves to a file in PNG format a plotting of time against
## "Energy sub metering 1, 2 and 3" information using data from the
## dates 2007-02-01 and 2007-02-02 of the “Individual household
## electric power consumption Data Set”.
plot3 <- function() {
        data <- readData()
        data <- strDateAndTimeToClasses(data)
        data <- filterByDate(data, "2007-02-01", "2007-02-02")
        
        data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
        data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
        data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)
        
        createPNGplot(data)
}