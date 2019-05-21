survey <- read.csv('/Users/vidit/Desktop/Masters/Sem_6/Advance Data Science/Project/R Code/clean-survey-data.csv')

# map on world
library(ggplot2)
usa <- map_data("world")

# map each survey
ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  coord_fixed(1.3) + geom_point(data = survey, aes(x = LocationLongitude, y = LocationLatitude), color = "yellow")

# distributions
hist(survey$Poor, main = 'Distribution of Number of Poor Stations', xlab = 'Number of Poor Stations', ylab = 'Number of Participants')
hist(survey$MapClicks, main = 'Distribution of Number of Map Clicks', xlab = 'Number of Map Clicks', ylab = 'Number of Participants')
hist(survey$DropClicks, main = 'Distribution of Number of Dropdown Clicks', xlab = 'Number of Drop Clicks', ylab = 'Number of Participants')

# states
barplot(table(survey$Texas), main = 'Number of Participants Who Knew Where Texas is.', xlab = 'Knew where Texas was', ylab = 'Number of Participants')
barplot(table(survey$California), main = 'Number of Participants Who Knew Where California is.', xlab = 'Knew where California was', ylab = 'Number of Participants')
barplot(table(survey$Alaska), main = 'Number of Participants Who Knew Where Alaska is.', xlab = 'Knew where Alaska was', ylab = 'Number of Participants')

# handedness
barplot(table(survey$Handedness), main = 'Handedness of Participants', xlab = 'Dominant Hand', ylab = 'Number of Participants')
barplot(table(survey$Sidebar), main = 'Distribution of Sidebar', xlab = 'Side', ylab = 'Number of Participants')

# demographics
barplot(table(survey$Age), main = 'Age of Participants', xlab = 'Age', ylab = 'Number of Participants')
barplot(table(survey$Latino), main = 'Latino Participants', xlab = 'Latino', ylab = 'Number of Participants')
barplot(table(survey$Race), main = 'Race of Participants', xlab = 'Race', ylab = 'Number of Participants')
barplot(table(survey$Education), main = 'Education of Participants', xlab = 'Education Level', ylab = 'Number of Participants')
barplot(table(survey$Married), main = 'Married Participants', xlab = 'Married', ylab = 'Number of Participants')
barplot(table(survey$Employment), main = 'Employment of Participants', xlab = 'Employment Status', ylab = 'Number of Participants')
barplot(table(survey$Income), main = 'Income of Participants', xlab = 'Income Level', ylab = 'Number of Participants')
