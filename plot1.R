# plot1.R
# author: J Go
# purpose: generate plot1 for Exploratory Data Analysis Course project 1
# 12/13/15: last updated

## require packages:

require(sqldf)		# for read.csv.sql

## read input data into dat:

dat <- read.csv.sql("../r/household_power_consumption.txt",sep=";",header=TRUE,stringsAsFactors = FALSE, sql = "select * from file where Date = '1/2/2007' OR Date = '2/2/2007'")

closeAllConnections()	# doesn't always prevent warnings on unused connections
			# but seems to help

## check data.frame for "?", and replace with NA if found: 

chk <- function(x, pattern=NA) ifelse(x %in% pattern, TRUE, FALSE)
NA_found <- sum(unlist(lapply(lapply(dat, chk), sum))) 
if (NA_found > 0) { 
    replace <- function(x, string2replace="?") ifelse(x %in% string2replace, NA, x) 
    y <- as.data.frame(lapply(dat, replace), stringsAsFactors=FALSE)
    dat <- y
}

## create plot1:

hist(dat$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")

## copy plot1 to a PNG file: plot1.png, width and height of 480 pixels

dev.copy(png, units = "px", width = 480, height = 480, file = "plot1.png")  	
dev.off()  		## Don't forget to close the PNG device!

closeAllConnections()	# repeat close, not sure this helps
