library(tidyverse)
library(ggplot2)
library(car)

UTS <- read.csv("/Users/vidit/Desktop/Masters/Sem_6/Advance Data Science/Project/R Code/clean-survey-data.csv")

set.seed(100)

scatterplot(MapClicks ~ DropClicks | Age, data = UTS)
boxplot(MapClicks ~ DropClicks, UTS)

ggplot(UTS, aes(y = MapClicks, x = DropClicks, group = Age)) + geom_point() + geom_smooth(method = "lm")

ggplot(UTS, aes(y = MapClicks, x = Age)) + geom_boxplot()
ggplot(UTS, aes(y = DropClicks, x = Age)) + geom_boxplot()
ggplot(UTS, aes(y = DropClicks + MapClicks, x = Age)) + geom_boxplot()

#doing anova
anova(lm(MapClicks ~ DropClicks * Age, data = UTS))

#interactive plots
data.lm <- lm(MapClicks ~ DropClicks + Age, data = UTS)
plot(data.lm)



#summary of the model
summary(data.lm)

#confidence intervals
confint(data.lm)

#doing anova
anova(data.lm)

#predictions
predict(data.lm, newdata = data.frame(Age = levels(UTS$Age), DropClicks = mean(UTS$DropClicks, na.rm = TRUE)), 
interval = "prediction")


#generating effects plots
## generate a prediction data frame
newdata <- data.frame(Age = levels(UTS$Age), DropClicks = mean(UTS$DropClicks, na.rm = TRUE))
fit <- predict(data.lm, newdata = newdata, interval = "confidence")
fit <- data.frame(newdata, fit)

library(ggplot2)
ggplot(fit, aes(y = fit, x = Age)) + geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  scale_y_continuous("Y") + scale_x_discrete("X") + theme_classic() +
  theme(axis.title.y = element_text(vjust = 1, size = rel(1.25)), axis.title.x = element_text(vjust = -1,
                                                                                              size = rel(1.25)))

#adding observed values
ggplot(fit, aes(y = fit, x = Age)) + geom_point(data = UTS, aes(y = MapClicks),
                                              col = "grey") + geom_pointrange(aes(ymin = lwr, ymax = upr)) + 
                                              scale_y_continuous("Y") +
  scale_x_discrete("X") + theme_classic() + theme(axis.title.y = element_text(vjust = 1,
                                                                              size = rel(1.25)), axis.title.x = 
                                                                              element_text(vjust = -1, size = rel(1.25)))
