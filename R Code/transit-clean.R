# read station data
stations <- read.csv('/Users/vidit/Desktop/Masters/Sem_6/Advance Data Science/Project/R Code/Public_Transit_Stations.csv')
stations$X <- NULL
stations$Y <- NULL
stations$ID <- NULL
stations$OBJECTID <- NULL
stations$STATIONURL <- NULL
stations$SYS_AGENCY <- NULL
stations$NTDID = NULL
stations$UA <- NULL
stations$UACODE <- NULL
stations$SOURCE <- NULL
stations$SOURCEDATE <- NULL
stations$ADDRESS <- NULL

# no tax data for PR
stations <- stations[stations$STATE != "PR",]

# add leading zeros to zip codes
library(stringr)
stations$ZIP <- apply(stations, 1, function(row) str_replace_all(str_pad(toString(row['ZIP']), 5), " ", "0")) # for some reason setting pad to 0 doesn't work

# read route data
routes <- read.csv('/Users/vidit/Desktop/Masters/Sem_6/Advance Data Science/Project/R Code/Routes_and_Stations.csv')
routes$OBJECTID <- NULL

# read tax data
raw_taxes <- read.csv('/Users/vidit/Desktop/Masters/Sem_6/Advance Data Science/Project/R Code/16zpallagi.csv')
taxes <- data.frame('zip' = raw_taxes$zipcode, 'count' = raw_taxes$N02650, 'total' = raw_taxes$A02650)
taxes <- taxes[taxes$zip != 0,]

# sum up totals for each zip code
library(dplyr)
taxes <- taxes %>% 
  group_by(zip) %>% 
  summarise_all(sum)

# compute average income for the zip code
taxes$avg <- as.integer(taxes$total / taxes$count * 1000)

# put taxes into brackets
taxes$bracket <- apply(taxes, 1, function(row) round(row['avg'] / 10000))
taxes$bracket <- apply(taxes, 1, function(row) ifelse(row['bracket'] > 10, "> 10", toString(row['bracket'])))

# add leading zeros to zip codes
taxes$zip <- apply(taxes, 1, function(row) str_replace_all(str_pad(toString(row['zip']), 5), " ", "0")) # had to be below the above line for some reason

# append tax data to station data
stations$income <- apply(stations, 1, function(row) paste(taxes[taxes$zip == row['ZIP'], 'avg'], collapse=' '))
stations$bracket <- apply(stations, 1, function(row) paste(taxes[taxes$zip == row['ZIP'], 'bracket'], collapse=' '))

# find matched routes for the stations
stations$RTE_NAME <- apply(stations, 1, function(row) paste(routes[routes$STATIONSYS == row['STATIONSYS'], 'RTE_ID_SYS'], collapse='||'))

# # check if there are unique stations between the 2 sets
# tStations <- unique(stations$STA_ID)
# rStations <- unique(routes$STA_ID)
# aStations <- unique(c(tStations, rStations))
# length(tStations)
# length(rStations)
# length(aStations)

# save the data to transit.csv
write.csv(stations, file='/Users/vidit/Desktop/Masters/Sem_6/Advance Data Science/Project/R Code/transit.csv')


