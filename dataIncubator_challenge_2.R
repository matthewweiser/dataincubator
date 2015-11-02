options(digits=10)

setwd("C:\\Users\\Matt\\Desktop\\dataIncubator")
setwd("/proj/fureylab/mweiser")
dat <- read.csv("nyc311calls.csv",stringsAsFactors=FALSE) #nrow=100000,
#some complaints duplicated; convert to upper:
dat$Complaint.Type <- toupper(dat$Complaint.Type)

# Manually investigate complaint types and pool the ones that are obviously identical.
# Do NOT pool types of noise complaints, for example, since we wish to retain as high a level of granularity as possible:
complaintTypes <- unique(dat$Complaint.Type)
complaintTypes[order(complaintTypes )]
filter <-  dat$Complaint.Type %in% c("DERELICT VEHICLES", "DERELICT VEHICLE")
dat$Complaint.Type[filter] <- "DERELICT VEHICLE"
filter <-  dat$Complaint.Type %in% c("ELECTRIC", "ELECTRICAL")
dat$Complaint.Type[filter] <- "ELECTRICAL"

#problem 1:
T <- table(dat$Agency)
T <- T[rev(order(T))]/sum(T)
T[2]

#problem 2:
table(dat$Borough)
allBor <- c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")
filter <- dat$Borough %in% allBor
dat_5Bor <- dat[filter,]
T_all <- table(dat_5Bor$Complaint.Type)/sum(table(dat_5Bor$Complaint.Type))
for(b in allBor){
  filter <- dat_5Bor$Borough == b
  T_raw <- table(dat_5Bor$Complaint.Type[filter])
  T <-  T_raw /  sum( T_raw)
  #remove low complaint numbers to avoid spuriosly high enrichment statistics:
  T <- T[ T_raw >= 10]
  filter <- names(T_all) %in% names(T)
  T_tmp <- T_all[filter]
  print(sum(names(T) != names(T_tmp)))
  enrichment <- T/(T_tmp)
  enrichment <- enrichment[rev(order(enrichment))]
  ind <- which( names(T_raw) == names(enrichment)[1])
  print(paste( b, T_raw[ind], names(enrichment)[1], enrichment[1]) )
}

#problem 3:
sum(is.na(dat$Latitude))
filter <- is.na(dat$Latitude) | is.na(dat$Longitude)
dat_latAndLong <- dat[!filter,]
#check for outliers and bad data points:
jpeg("lat.jpeg")
hist(dat_latAndLong$Latitude)
dev.off()
jpeg("long.jpeg")
hist(dat_latAndLong$Longitude)
dev.off()
#remove locations with bad coords:
filter <- dat_latAndLong$Latitude < 40.45  |  dat_latAndLong$Latitude < -74.5
dat_latAndLong  <-  dat_latAndLong[!filter,]
#get diff in 90/10% quantiles:
quantile(dat_latAndLong$Latitude, 0.9)
quantile(dat_latAndLong$Latitude, 0.1)
abs(quantile(dat_latAndLong$Latitude, 0.9)-quantile(dat_latAndLong$Latitude, 0.1))

#problem 4 (must use r 3.1.3 or later):
library(mclust)
library(MASS)
library(aspace)
coords <- data.frame( dat_latAndLong$Latitude, dat_latAndLong$Longitude )
m <- calc_sde(points=coords)
#can grab the area in degrees, but longitudnal degrees vary from N to S...
#did not have time to get conversion done, so only have a degree-based area from above model. rough estimate given in place
#of blank answer:
0.04172842*((111+85)/2)
#4.08938516


#problem 5:
date_full <- unlist(sapply(strsplit(dat$Created.Date, " "), "[[", 1))
time_full <- unlist(sapply(strsplit(dat$Created.Date, " "), "[[", 2))
hours <- as.numeric(unlist(sapply(strsplit(time_full,":"), "[[", 1)) )
minutes <- as.numeric(unlist(sapply(strsplit(time_full,":"), "[[", 2)) )
seconds <- as.numeric(unlist(sapply(strsplit(time_full,":"), "[[", 3)) )
amPM <- unlist(sapply(strsplit(dat$Created.Date, " "), "[[", 3))
allDayMinutes <- rep(0, nrow(dat))
filter <- amPM == "AM" & hours == 12
allDayMinutes[filter] <- minutes[filter]
filter <- amPM == "AM" & hours != 12
allDayMinutes[filter] <- 60*hours[filter] + minutes[filter]
filter <- amPM == "PM" & hours == 12
allDayMinutes[filter] <- 12*60 + minutes[filter]
filter <- amPM == "PM" & hours != 12
allDayMinutes[filter] <- 60*(12+hours[filter]) + minutes[filter]
allDayMinutes <- allDayMinutes+1
jpeg("minuteCounts_noFilter.jpeg")
hist(allDayMinutes, breaks=100)
dev.off()
filter <- allDayMinutes == 1 & seconds == 0
sum(filter)
#this looks more realistic!
jpeg("minuteCounts_withFilter.jpeg")
hist(allDayMinutes[!filter], breaks=100)
dev.off()
dat_goodMinutes <- dat[!filter,]
allDayMinutes <- allDayMinutes[!filter]
seconds <- seconds[!filter]
date_full <- date_full[!filter]

allVolByHour <- rep(0, 60*24)
for( i in 1:length(allVolByHour)){
  print(i)
  if( i > (60*24-59) ){
    rolloverInd <- i-(60*24-59)
    allVolByHour[i] <- sum(allDayMinutes >= i ) + sum(allDayMinutes <= rolloverInd )
  } else {
    allVolByHour[i] <- sum(allDayMinutes >= i & allDayMinutes < (i + 60))
  }
}
jpeg("volByHourHist.jpeg")
hist(allVolByHour, breaks=100)
dev.off()
jpeg("volByHourPlot.jpeg")
plot(allVolByHour)
dev.off()
#expected calls, at max/min hours (total call difference normalized by number of days in data set):
(max(allVolByHour) - min(allVolByHour))/length(unique(date_full))

#problem 6:
# order calls by date/time, then construct the time differential between each:
daySeconds <- 60*(allDayMinutes-1) + seconds
month <-  as.numeric(  unlist(sapply(strsplit(date_full, "/"), "[[", 1)))
day <-  as.numeric(  unlist(sapply(strsplit(date_full, "/"), "[[", 2)))
year <-  as.numeric(  unlist(sapply(strsplit(date_full, "/"), "[[", 3)))
ORD <- order(year, month, day, daySeconds)
#fix this!
daySeconds <- daySeconds[ORD]
daySeconds_shift <- c(daySeconds[2:length(daySeconds)] , daySeconds[1])
diffInSeconds <- abs(daySeconds_shift-daySeconds)
filter <- diffInSeconds > 80000
diffInSeconds[filter] <- abs((daySeconds_shift[filter]+(60*60*24))-(daySeconds[filter]))
jpeg("diffInSec_hist.jpeg")
hist(diffInSeconds)
dev.off()

#remove lapses of > 600 seconds (10 min) as this is likely either due to an aberration or a system shutdown:
filter <- diffInSeconds > 600
sum(filter)
sum(!filter)
#this is cleaner!
jpeg("diffInSec_filtered_hist.jpeg")
hist(diffInSeconds[!filter],breaks=100)
dev.off()
sd(diffInSeconds[!filter])



















