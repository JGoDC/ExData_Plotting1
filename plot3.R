# plot3.R
# author: J Go
# purpose: generate plot3 for Exploratory Data Course project 1
# date last updated: 12/13/15
# 
require(sqldf)	# for read.csv.sql
require(lubridate)

dat <- read.csv.sql("../r/household_power_consumption.txt",sep=";",header=TRUE,stringsAsFactors = FALSE, sql = "select * from file where Date = '1/2/2007' OR Date = '2/2/2007'")


## check data.frame for "?", and replace with NA if found:

chk <- function(x, pattern=NA) ifelse(x %in% pattern, TRUE, FALSE)
NA_found <- sum(unlist(lapply(lapply(dat, chk), sum)))
if (NA_found > 0) {
    replace <- function(x, string2replace="?") ifelse(x %in% string2replace, NA, x)
    y <- as.data.frame(lapply(dat, replace), stringsAsFactors=FALSE)
    dat <- y
}

## create datetime object dvzt using lubridate rather than suggested functions

dv <- dat[,1]						# dv = date vector
dvl <- dmy(dv)						# dvl = dv lubridated
dvz <- with_tz(dvl, "America/New_York") + hours(5) 	# dvz = dvl EST tz
tv <- dat[,2]						# tv = time vector
tl <- hms(tv)						# tl = tv lubridated

dvzt <- dvz + tl					# dvzt = dvz + tl

## generate plot3 

plot(dvzt, dat[,7], type="s", xlab="", ylab="Energy sub metering", lwd=1)
lines(dvzt, dat[,8], type="s", col="red", lwd=1)
lines(dvzt, dat[,9], type="s", col="blue", lwd=1)
legend("topright", legend = c("Sub_metering_1  ", "Sub_metering_2  ", "Sub_metering_3  "), lwd=1, cex=1, col=c("black", "red", "blue"), lty=1:1)

closeAllConnections()	# doesn't always prevent warnings on unused connections
 
## Copy my plot to a PNG file
# save to a PNG file with a width of 480 pixels and a height of 480 pixels
dev.copy(png, units = "px", width = 480, height = 480, file = "plot3.png")  	
dev.off()  		## Don't forget to close the PNG device!
# 
closeAllConnections()	# always prevent warnings on unused connections?
