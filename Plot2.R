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
######### and extract information to household dataframe #########
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
## in a single sentence, so we use a simple loop
## to have numbers and not characters in variables

for(i in c(3:9)) {household[,i] <- as.numeric(household[,i])}

## We use a variable with date an time

household$DataHora <- paste(household$Date, household$Time)
household$DataHora <- strptime(household$DataHora, format="%Y-%m-%d %H:%M:%S")

######### Plot 2 for the Project1 #########
## This plot only have label and marks
## in Y dimension, so we have to take off marks and labels for
## the x axis.


png(filename = "plot2.png", width = 480, height = 480, units = "px", bg = "white")

par(mar = c(6, 6, 5, 4))

plot(household$DataHora, household$Global_active_power, xaxt=NULL, xlab = "", ylab = "Global Active Power (kilowatts)", type="n")
lines(household$DataHora, household$Global_active_power, type="S")

dev.off()
