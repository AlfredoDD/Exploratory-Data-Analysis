#### This code is for project 1 of the Coursera course "Exploratory Data Analysis"
#### Date: 10 April 2015
#### Author: Alfredo Domínguez

############################## Environment Preparation ##############################
# Load necessary packages
library("data.table")
library("dplyr")
# First, create directory to contain data, and fix relative work environment
if(!file.exists("Project1")){
    dir.create("Project1")
}
setwd("Project1")

######### Read in zip file #########

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
file <- "household_power_consumption"
download.file(url, file, method = "curl")
datedownloaded <- date()
unzip(file, junkpaths = TRUE)
household <- data.frame(fread("household_power_consumption.txt", stringsAsFactors = FALSE))
 
######### Prepare data to work with #########

# Change the format of Date variable
household$Date <- as.Date(household$Date, format="%d/%m/%Y")

# Now, we select only data for analysis, and remove large data
householdFeb <- subset(household, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
rm(household)
household <- householdFeb
rm(householdFeb)


## Because list cannot be coerced to type 'double'
## we can not use household[,3:9] <- as.numeric(household[,3:9]
## in a single sentence, and we use a simple loop
## to have numbers and not characters in variables

for(i in c(3:9)) {household[,i] <- as.numeric(household[,i])}

# We use a variable with date an time
household$DateTime <- paste(household$Date, household$Time)
household$DateTime <- strptime(household$DateTime, format="%Y-%m-%d %H:%M:%S")

######### Save data file #########

write.csv(household, file = "Home Power Consumption Subset.csv")

######### Open Data File #########

household <- read.csv("Home Power Consumption Subset.csv")

######### Plot 1 #########

png(filename = "plot1.png", width = 480, height = 480, units = "px", bg = "white")

par(mar = c(6, 6, 5, 4))

hist(household$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power(kilowatts)")
dev.off()

######### Plot 2 #########

png(filename = "plot2.png", width = 480, height = 480, units = "px", bg = "white")

par(mar = c(6, 6, 5, 4))

plot(household$Date_Time, household$Global_active_power, xaxt=NULL, xlab = "", ylab = "Global Active Power (kilowatts)", type="n")
# type = "n" builds plots without points
# xaxt = NULL suppresses x axis
# xlab = "" removes the label from the x axis
# otherwise, the axis is the name of the x variable, which is date_time

lines(household$Date_Time, household$Global_active_power, type="S")

dev.off()

######### Plot 3 #########

png(filename = "plot3.png", width = 480, height = 480, units = "px", bg = "white")

par(mar = c(7, 6, 5, 4))

plot(household$Date_Time, household$Sub_metering_1, xaxt=NULL, xlab = "", ylab = "Energy sub metering", type="n")
## Sets up the plot, but does not populate with any data

lines(household$Date_Time, household$Sub_metering_1, col = "black", type = "S")
## Plots lines for sub_metering_1
lines(household$Date_Time, household$Sub_metering_2, col = "red", type = "S")
## Plots lines for sub_metering_2
lines(household$Date_Time, household$Sub_metering_3, col = "blue", type = "S")
## Plots lines for sub_metering_3

legend("topright", lty = c(1, 1), lwd = c(1, 1, 1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
# Adds a legend with lines
# lwd = c(1, 1, 1) assigns the lines widths of 1
# lty = c(1, 1) assigns the line type within the legend

dev.off()

######### Plot 4 #########

#### Turn on png device and set parameters
png(filename = "plot4.png", width = 480, height = 480, units = "px", bg = "white")
par(mfrow = c(2, 2), mar = c(14, 6, 2, 2), cex=.5)

#### Plot 2 in top left
plot(household$Date_Time, household$Global_active_power, xaxt=NULL, xlab = "", ylab = "Global Active Power", type="n")
# type = "n" builds plots without points
# xaxt = NULL suppresses x axis
# xlab = "" removes the label from the x axis
# otherwise, the axis is the name of the x variable, which is date_time

lines(household$Date_Time, household$Global_active_power, type="S")


#### Top right graph
plot(household$Date_Time, household$Voltage, xaxt=NULL, xlab = "datetime", ylab = "Voltage", type="n")
# type = "n" builds plots without points
# xaxt = NULL suppresses x axis

lines(household$Date_Time, household$Voltage, type="S")


#### Plot 3 in bottom left
plot(household$Date_Time, household$Sub_metering_1, xaxt=NULL, xlab = "", ylab = "Energy sub metering", type="n")
## Sets up the plot, but does not populate with any data

lines(household$Date_Time, household$Sub_metering_1, col = "black", type = "S")
## Plots lines for sub_metering_1
lines(household$Date_Time, household$Sub_metering_2, col = "red", type = "S")
## Plots lines for sub_metering_2
lines(household$Date_Time, household$Sub_metering_3, col = "blue", type = "S")
## Plots lines for sub_metering_3

legend("topright", bty = "n", lty = c(1, 1), lwd = c(1, 1, 1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
# Adds a legend with lines
# lwd = c(1, 1, 1) assigns the lines widths of 1
# lty = c(1, 1) assigns the line type within the legend
# bty = "n" sets the box type to none


#### Bottom right graph
plot(household$Date_Time, household$Global_reactive_power, xaxt=NULL, xlab = "datetime", ylab = "Global_reactive_power", type="n")
# type = "n" builds plots without points
# xaxt = NULL suppresses x axis

lines(household$Date_Time, household$Global_reactive_power, type="S")

#### Turn off device 
dev.off()


quartz()
# Open screen viewing device