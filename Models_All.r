setwd("D:/2018 Spring/SDM/DataAnalyticsProject")
d <- read.csv("Transit_Data_Modified.csv")
# 


# Neal's Work
m1 <- lm(d$Intersection.Density..1.Mile.Diameter. ~ d$Modes.Served + as.factor(d$State.Designated.TOD)*d$Area.Population..2010. + d$Station.Type, data = d)
summary(m1)
hist(m1$res)
plot(m1$res ~ m1$fit)
plot(m1$res, m1$fit)
bartlett.test(list(m1$res, m1$fit))
qqnorm(m1$res)
qqline(m1$res, col = "red")
shapiro.test(m1$res)
plot(m1)
library(car)
vif(m1)
########
m2 = lm(log(d$Weekday_ridership) ~ d$Intersection.Density..1.Mile.Diameter. 
        + d$Area.Population..2010. + as.factor(d$State.Designated.TOD)
        + as.factor(d$Station.Type) 
        + d$Frequency_of_Service_Weekday_All
        + d$Frequency_of_Service_Weekday_All * d$Intersection.Density..1.Mile.Diameter.
        ,data =d)
summary(m2)
# Summary - R value us 0.597 which is higher.
# Frequency of Weekday is significant
# Higher Frequency of rail on weekday will have higher Ridership

# Data seems to be normally distributed
hist(m2$residuals)

# Plots 
plot(m2)

# Linear
qqnorm(m2$residuals)
qqline(m2$residuals, col="red")

#Data looks to be homoskedatic and not biased.
plot(m2$residuals ~ m2$fitted.values)

#Bartlett Test
bartlett.test(list(m2$res, m2$fit))

#Shapiro Test - Passed as the P > 0.5
shapiro.test(m2$residuals)

AIC(m2)
BIC(m2)
############
m3 = lm(formula= d$Area.Population..2010.~ 
         d$Rail.Lines.Served+
         d$Station.Type+
         d$TOD.Place.Type+
         d$Transit.Score+
         d$Transit.Connections,data=d)
summary(m3)


m_withLog = lm(formula= log(d$Area.Population..2010.)~ 
                 d$Rail.Lines.Served+
                 d$Station.Type+
                 d$TOD.Place.Type+
                 d$Transit.Score+
                 d$Transit.Connections,data=d)
summary(m_withLog)

install.packages("ggplot2")
library("ggplot2")

hist(m3$residuals)

hist(m_withLog$residuals)


plot(m3)
plot(m_withLog)


qqnorm(m3$residuals)
qqline(m3$residuals, col="red")

qqnorm(m_withLog$residuals)
qqline(m_withLog$residuals, col="red")
#######
m4 <- lm(d$Short.Trip.Opportunity.Analysis..1.Mile.Buffer. ~     d$Area.Population..2010. + d$Spaces.relative.to.other.stations.on.the.same.line + d$Spaces.relative.to.number.of.Average.Daily.Riders..Weekday..Total.lines + d$Modes.Served + d$Total.Rail.Lines, data = d)
summary(m4)
hist(m4$res)
plot(m4)
shapiro.test(m4$res)
bartlett.test(list(m4$res, m4$fit))
