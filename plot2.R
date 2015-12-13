# plot2.R
# author: J Go
# purpose: generate plot2 for Exploratory Data Analysis Course project 1
# date last updated: 12/13/15
# 

## require packages:

require(sqldf)		# for read.csv.sql
require(lubridate)      # for date functions ~ using instead of suggested functions

## read input with sql subsetting into dat:

dat <- read.csv.sql("../r/household_power_consumption.txt",sep=";",header=TRUE,stringsAsFactors = FALSE, sql = "select * from file where Date = '1/2/2007' OR Date = '2/2/2007'")

closeAllConnections()	# doesn't always prevent warnings on unused connections

## create date-time object dvzt:

dv <- dat[,1]						# dv = date vector
dvl <- dmy(dv)						# dvl = dv lubridate format
dvz <- with_tz(dvl, "America/New_York") + hours(5) 	# dvz = dvl EST tz
tv <- dat[,2]						# tv = time vector
tl <- hms(tv)						# tl = tv lubridate format

dvzt <- dvz + tl					# dvzt = dvz + tl

## check data.frame for "?", and replace with NA if found:

chk <- function(x, pattern=NA) ifelse(x %in% pattern, TRUE, FALSE)
NA_found <- sum(unlist(lapply(lapply(dat, chk), sum)))
if (NA_found > 0) {
    replace <- function(x, string2replace="?") ifelse(x %in% string2replace, NA, x)
    y <- as.data.frame(lapply(dat, replace), stringsAsFactors=FALSE)
    dat <- y
}

## create plot2:
plot(dvzt, dat[,3], type="s", xlab="", ylab="Global Active Power (kilowatts)")

## Copy my plot to a PNG file with width and height of 480 pixels:
dev.copy(png, units = "px", width = 480, height = 480, file = "plot2.png")  	
dev.off()  		## Don't forget to close the PNG device!
 
closeAllConnections()	# repeat close, not sure if it  helps
