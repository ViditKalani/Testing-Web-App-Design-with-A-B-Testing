survey <- read.csv('/Users/vidit/Desktop/Masters/Sem_6/Advance Data Science/Project/R Code/clean-survey-data.csv', row.names = 'X')

library(dplyr)
library(ggpubr)

survey$StartDate <- as.Date(survey$StartDate)
survey$EndDate <- as.Date(survey$EndDate)

#
# Look at map clicks
#
group_by(survey, Age) %>%
  summarize(
    count = n(),
    mean = mean(MapClicks, na.rm = TRUE),
    sd = sd(MapClicks, na.rm = TRUE)
  )
clicks <- survey[survey$MapClicks <= 20,]
ggboxplot(clicks, x = 'Age', y = 'MapClicks', ylab = 'Clicks on Map', xlab = 'Age', color = 'Age')

#
# Look at drop clicks
#
group_by(survey, Age) %>%
  summarize(
    count = n(),
    mean = mean(DropClicks, na.rm = TRUE),
    sd = sd(DropClicks, na.rm = TRUE)
  )
ggboxplot(survey, x = 'Age', y = 'DropClicks', ylab = 'Clicks on Map', xlab = 'Age')

#
# Look at handedness and sidebar
#
times <- survey
times <- times[(times$Time1 <= 15000) & (times$Time2 <= 15000) & (times$Time3 <= 15000),]
times <- times[times$StartDate > as.Date('2019-04-14'),]
ggboxplot(times, x = 'Handedness', y = 'Time1', color = 'Handedness', ylab = 'Time to Click', xlab = 'Dominant Hand')
ggboxplot(times, x = 'Handedness', y = 'Time2', color = 'Handedness', ylab = 'Time to Click', xlab = 'Dominant Hand')
ggboxplot(times, x = 'Handedness', y = 'Time3', color = 'Handedness', ylab = 'Time to Click', xlab = 'Dominant Hand')

group_by(survey, Age) %>%
  summarize(
    count = n(),
    mean = mean(DropClicks, na.rm = TRUE),
    sd = sd(DropClicks, na.rm = TRUE)
  )

#
# Look at poor stations
#
ggboxplot(survey, x = 'Age', y = 'Poor', color = 'Gender', ylab = 'Poor Stations', xlab = 'Age')
          