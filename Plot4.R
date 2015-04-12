#### This code is for project 1 of the Coursera course "Exploratory Data Analysis"
#### Date: 10 April 2015
#### Author: Alfredo Domínguez

############################## Environment Preparation ##############################
# Load necessary packages
library("data.table")
# First, create directory to contain data, and fix relative work environment
if(!file.exists("Project1")){
    dir.create("Project1")
}
setwd("Project1")

######### Read in zip file #########
######### and extract data to household dataframe #########
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
file <- "household_power_consumption"
download.file(url, file, method = "curl")
datedownloaded <- date()
unzip(file, junkpaths = TRUE)
household <- data.frame(fread("household_power_consumption.txt", stringsAsFactors = FALSE))

######### Prepare data to work with #########

## Change the format of Date variable
household$Date <- as.Date(household$Date, format="%d/%m/%Y")

# Now, we select only data for analysis, and remove large data
householdFeb <- subset(household, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
rm(household)
household <- householdFeb
rm(householdFeb)


## Because list cannot be coerced to type 'double'
## we can not use household[,3:9] <- as.numeric(household[,3:9]
## in a single sentence, so we use a simple loop
## to have numbers and not characters in variables

for(i in c(3:9)) {household[,i] <- as.numeric(household[,i])}

# We use a variable with date an time
household$DataHora <- paste(household$Date, household$Time)
household$DataHora <- strptime(household$DataHora, format="%Y-%m-%d %H:%M:%S")

######### Plot 4 for the project ###########
######### 4 different plots in same PNG #########
######### 1st plot Global Active Power vs DateTime#######
######### 2nd plot Voltage vs DateTime#######
######### 3rd plot Submeterings vs DateTime#######
######### 4th plot Global Reactive Power vs DateTime#######
#### We set the figures with mfrow par ######
##### We use data from other plot exercises#####
##### but with X label in Plot 2 and 4 #######


png(filename = "plot4.png", width = 480, height = 480, units = "px", bg = "white")
par(mfrow = c(2, 2), mar = c(13, 6, 2, 2), cex=.6)

#### Plot 1 #####
plot(household$DataHora, household$Global_active_power, xaxt=NULL, xlab = "", ylab = "Global Active Power", type="n")
lines(household$DataHora, household$Global_active_power, type="S")


#### Plot 2 #####
plot(household$DataHora, household$Voltage, xaxt=NULL, xlab = "datetime", ylab = "Voltage", type="n")
lines(household$DataHora, household$Voltage, type="S")


#### Plot 3 #####


plot(household$DataHora, household$Sub_metering_1, xaxt=NULL, xlab = "", ylab = "Energy sub metering", type="n")

lines(household$DataHora, household$Sub_metering_1, col = "black", type = "S")

lines(household$DataHora, household$Sub_metering_2, col = "red", type = "S")

lines(household$DataHora, household$Sub_metering_3, col = "blue", type = "S")

legend("topright", lty = c(1, 1), lwd = c(1, 1, 1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#### Plot 4 #####


plot(household$DataHora, household$Global_reactive_power, xaxt=NULL, xlab = "datetime", ylab = "Global_reactive_power", type="n")
lines(household$DataHora, household$Global_reactive_power, type="S")

#### Turn off device 
dev.off()
