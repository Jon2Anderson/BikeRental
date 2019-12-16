#rm(list=ls(all=TRUE))
library(lubridate)
library(stringr)
library(data.table)

## Import Data ##
setwd("G:\\GradSchool\\DataScience\\Project\\data") ## Change this to proper dir
alldata <- read.csv("bikeData2018.csv") ## Change this to your data file name

### FUNCTIONS ###

## Function to find the last station that any
## given bike was returned to
getCurrentStation <- function(bike, df){
  df <- df[df$Bike.number == bike,]
  stationnums <- as.vector(df$End.station.number)
  stationnames <- as.vector(df$End.station)
  currentstationnum <- stationnums[length(stationnums)]
  currentstationname <- stationnames[length(stationnames)]
  currentstation <- paste(currentstationnum, currentstationname)
  return(currentstation)
}

getStationHistory <- function(bike, df) {
  df <- df[df$Bike.number == bike,]
  stations <- as.vector(df$End.station)
  stationnums <- as.vector(df$End.station.number)
  dates <- as.vector(as.character(df$EndDate))
  historydf <- data.frame(dates, stationnums, stations)
  colnames(historydf) <- c("Returned", "Station #", "Location")
  return(historydf)
}

stationDuration <- function(bike, df) {
  df <- df[df$Bike.number == bike,]
  startstations <- as.vector(df$Start.station.number)
  endstations <- as.vector(df$End.station.number)
  sdates <- as.vector(as.character(df$StartDate))
  edates <- as.vector(as.character(df$EndDate))
  x <- data.frame(sdates, edates, startstations, endstations)
  colnames(x)<-c("Start", "End", "Start St", "End St")
  
  for (i in 2:length(x$Start)-1) {
    timepassed <- x$End[i] %--% x$Start[i+1]
    x$Idle[i] <- as.duration(timepassed) / ddays(1)
  }
  x$Idle[length(x$Start)]<-0
  return(x)
}

## Separate day and time fields and delete original fields ##
startd<-str_split_fixed(alldata$Start.date, " ",2)
alldata$StartDate <- startd[,1]
alldata$StartTime <- startd[,2]
endd <- str_split_fixed(alldata$End.date, " ", 2)
alldata$EndDate <- endd[,1]
alldata$EndTime <- endd[,2]
alldata <- within(alldata,rm(Start.date, End.date))
alldata <- alldata[c(1,7,9,10,11,12,2,4,3,6,5,8)] #rearrange columns
alldata$StartDate <- ymd(alldata$StartDate)
alldata$EndDate <- ymd(alldata$EndDate)

## For this analysis I am going to imagine that we are somewhere
## in the middle of the dataset timeline. Let's imagine that it
## is September 1st, 2018 right now

### Delete all rows after 8/31/18 ###
# find index of first row with 9/1 as the end date
# and get rid of all rows after that 
x<-min(which(alldata$EndDate %like% "2018-09-01"))
alldata <- alldata[1:x-1,]

### Now our dataset is records from 1/1/2018 - 8/31/2018

# Find how many bikes have been rented this year #
bikecount <- length(unique(alldata$Bike.number)) #4,664 bikes
bikes <- unique(alldata$Bike.number)

# We want to find out how many times each bike has been rented
# this year, let's make the bike number column a vector so we can
# study the usage
bikelist <- alldata[,"Bike.number"]
xbike <- "W00143"

# Put the counts of each bike in a table
bikeusage <- table(bikelist)
dt <- as.data.table(bikeusage)
colnames(dt)<-c("Bike", "Rides")

# Get rid of rows that have 0 rides 
dt <- dt[Rides != 0]

# Ensure that our data is good, the total rides
# should be the same as the length of any column
# from our original data
totalrides<-dt[, sum(Rides)]
totalrides == length(alldata$X) #returns true


## From here we can go a number of different ways
## depending on what we're looking for. For this
## first example, let's find a bike that is not being
## used and see if we can find any suggestions that the
## bike is damaged or undesirable in some other way

# Order our data table by number of rides, ascending
dt <- dt[order(Rides)]
dt

tenrides <- dt[Rides==10]
threerides<-dt[Rides==3]

xbike <- "W23578"  #Let's use this bike for a demonstration

# Find this bike's station history for this year
stationhistory <-getStationHistory(xbike,alldata)

# Let's find where this bike is currently
currentstation <- getCurrentStation(xbike, alldata)

# Find out how long bike went unrented between each rental
idleTime <- stationDuration(xbike, alldata)
idleTime$Start <- ymd(idleTime$Start)
idleTime$End <- ymd(idleTime$End)
## In this example, we see that nobody rented this bike 
## between 8/18 and 8/25 - that is a possible red flag,
## let's see how many rentals this station had between
## those dates

# This nested loop checks for any idle times greater than 6
# and then finds how many rentals that station had on the 
# dates that that bike was sitting idle in that station

bikelist2=as.vector(bikelist[200:300])


bikesToCheck <- c()
for (i in 1:length(bikelist2)) {
  xbike=bikelist2[i]
  idleTime <- stationDuration(xbike, alldata)
  idleTime$Start <- ymd(idleTime$Start)
  idleTime$End <- ymd(idleTime$End)

  idleTime$TotRides<-"--"
  for (i in 1:length(idleTime$Start)) {
    if (idleTime$Idle[i] > 6) {
      station <- idleTime$`End St`[i]
      date1<-idleTime$End[i]
      date1=date1+days(1)
      date2<-idleTime$Start[i+1]
      date2=date2-days(1)
      elapsed <- date1 %--% date2
      ndays<-interval(date1,date2)/days(1)
      daysbetween<-date1 + days(0:ndays)
      totrides = 0
      for (k in 1:length(daysbetween)) {
        rides<-length(subset(alldata, Start.station.number==station & StartDate==daysbetween[k])$X)
        totrides = totrides+rides
      idleTime$TotRides[i]<-totrides
      if((totrides/ndays)>2) {
        append(bikesToCheck,xbike)
      }
      
      }
      
      
    }
  }
  print(xbike)
  #print(idleTime)
}
idleTime
## Viewing the data frame now shows that this station only
## had 5 rentals between 8/18 and 8/25 - so we don't suspect
## that there was something wrong with this bike in that time,
## it was just a low volume time period - possibly because of 
## weather



#######################
### DUPLICATE CODE ###
#######################

## Let's try a different bike

xbike <- "W00211"
stationhistory<-getStationHistory(xbike,alldata)
#This bike was used three times, once on 3/4, 
#and then not again until 7/3 and 7/5, and it
#hasn't been used since (note: today is 9/1
#in this simulation)

currentstation <- getCurrentStation(xbike, alldata)
#The bike should currently be sitting at station
#32052 - Key West Ave & Siesta Key Way

idleTime <- stationDuration(xbike, alldata)
idleTime$Start <- ymd(idleTime$Start)
idleTime$End <- ymd(idleTime$End)


idleTime$TotRides[i]<- totrides
for (i in 1:length(idleTime$Start)) {
  if (idleTime$Idle[i] > 6) {
    station <- idleTime$`End St`[i]
    date1<-idleTime$End[i]
    date1=date1+days(1)
    date2<-idleTime$Start[i+1]
    date2=date2-days(1)
    elapsed <- date1 %--% date2
    ndays<-interval(date1,date2)/days(1)
    daysbetween<-date1 + days(0:ndays)
    totrides = 0
    for (k in 1:length(daysbetween)) {
      rides<-length(subset(alldata, Start.station.number==station & StartDate==daysbetween[k])$X)
      totrides = totrides+rides
    }
    
    idleTime$TotRides<- "--"
  }
}
idleTime

## This bike was unrented for 121 days, however
## we only saw this station rent 29 bikes in that 
## time period - so this is clearly just a very
## rarely used station, so we shouldn't be surprised
## to see low usage of bikes


#######################
### DUPLICATE CODE ###
#######################

## Let's try a different bike

twentyrides <-dt[Rides==20]


xbike <- "W00857"
stationhistory<-getStationHistory(xbike,alldata)
#This bike was used three times, once on 3/4, 
#and then not again until 7/3 and 7/5, and it
#hasn't been used since (note: today is 9/1
#in this simulation)

currentstation <- getCurrentStation(xbike, alldata)
#The bike should currently be sitting at station
#32052 - Key West Ave & Siesta Key Way

idleTime <- stationDuration(xbike, alldata)
idleTime$Start <- ymd(idleTime$Start)
idleTime$End <- ymd(idleTime$End)

idleTime$TotRides<- "--"
for (i in 1:length(idleTime$Start)) {
  if (idleTime$Idle[i] > 6) {
    station <- idleTime$`End St`[i]
    date1<-idleTime$End[i]
    date1=date1+days(1)
    date2<-idleTime$Start[i+1]
    date2=date2-days(1)
    elapsed <- date1 %--% date2
    ndays<-interval(date1,date2)/days(1)
    daysbetween<-date1 + days(0:ndays)
    totrides = 0
    for (k in 1:length(daysbetween)) {
      rides<-length(subset(alldata, Start.station.number==station & StartDate==daysbetween[k])$X)
      totrides = totrides+rides
    idleTime$TotRides[i]<- totrides
    }
    
  }
}
idleTime



## Let's find a more frequently used bike to investigate

fiftyrides <-dt[Rides>50]


xbike <- "W01268"
stationhistory<-getStationHistory(xbike,alldata)
currentstation <- getCurrentStation(xbike, alldata)

idleTime <- stationDuration(xbike, alldata)
idleTime$Start <- ymd(idleTime$Start)
idleTime$End <- ymd(idleTime$End)

idleTime$TotRides<- "--"
for (i in 1:length(idleTime$Start)) {
  if (idleTime$Idle[i] > 6) {
    station <- idleTime$`End St`[i]
    date1<-idleTime$End[i]
    date1=date1+days(1)
    date2<-idleTime$Start[i+1]
    date2=date2-days(1)
    elapsed <- date1 %--% date2
    ndays<-interval(date1,date2)/days(1)
    daysbetween<-date1 + days(0:ndays)
    totrides = 0
    for (k in 1:length(daysbetween)) {
      rides<-length(subset(alldata, Start.station.number==station & StartDate==daysbetween[k])$X)
      totrides = totrides+rides
      idleTime$TotRides[i]<- totrides
    }
    
  }
}
idleTime
## This bike hasn't been used since 3/4, suggesting
## there might be something wrong with it - however
## it's also possible that this bike has been taken 
## out of circulation for other reasons