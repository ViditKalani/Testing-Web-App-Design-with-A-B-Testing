# filter out bad data
raw <- read.csv('/Users/vidit/Desktop/Masters/Sem_6/Advance Data Science/Project/R Code/transit-data.csv', row.names = 'ResponseId')
filtered <- raw[raw$Status == 'IP Address',]
filtered <- filtered[filtered$Finished == 'True',]
filtered <- filtered[filtered$Progress == '100',]
filtered <- filtered[filtered$Q0 == 'I consent',]

# remove useless columns
filtered$Status <- NULL
filtered$IPAddress <- NULL
filtered$Progress <- NULL
filtered$Finished <- NULL
filtered$RecipientLastName <- NULL
filtered$RecipientFirstName <- NULL
filtered$RecipientEmail <- NULL
filtered$ExternalReference <- NULL
filtered$DistributionChannel <- NULL
filtered$UserLanguage <- NULL
filtered$Q0 <- NULL

# rename columns
colnames(filtered)[colnames(filtered) == 'Q14'] <- 'Poor'
colnames(filtered)[colnames(filtered) == 'Q12'] <- 'Usage'
colnames(filtered)[colnames(filtered) == 'Q15'] <- 'Texas'
colnames(filtered)[colnames(filtered) == 'Q16'] <- 'California'
colnames(filtered)[colnames(filtered) == 'Q17'] <- 'Alaska'
colnames(filtered)[colnames(filtered) == 'Q18'] <- 'Satisfaction'
colnames(filtered)[colnames(filtered) == 'Q19'] <- 'Intuitiveness'
colnames(filtered)[colnames(filtered) == 'Q1' ] <- 'Gender'
colnames(filtered)[colnames(filtered) == 'Q4' ] <- 'Age'
colnames(filtered)[colnames(filtered) == 'Q20'] <- 'Latino'
colnames(filtered)[colnames(filtered) == 'Q21'] <- 'Race'
colnames(filtered)[colnames(filtered) == 'Q22'] <- 'Education'
colnames(filtered)[colnames(filtered) == 'Q23'] <- 'Married'
colnames(filtered)[colnames(filtered) == 'Q24'] <- 'Employment'
colnames(filtered)[colnames(filtered) == 'Q25'] <- 'Income'
colnames(filtered)[colnames(filtered) == 'Q26'] <- 'Handedness'
colnames(filtered)[colnames(filtered) == 'Q10'] <- 'Zipcode'

# break up metadata column
library(stringr)
usage <- str_split_fixed(filtered$Usage, '-', 6)
filtered$MapClicks <- usage[,1]
filtered$DropClicks <- usage[,2]
filtered$Time1 <- usage[,3]
filtered$Time2 <- usage[,4]
filtered$Time3 <- usage[,5]
filtered$Sidebar <- usage[,6]

write.csv(filtered, file='/Users/vidit/Desktop/Masters/Sem_6/Advance Data Science/Project/R Code/clean-survey-data.csv')
