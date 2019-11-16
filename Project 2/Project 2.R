### Question 1
roadrace <- read.csv("roadrace.csv")

## 1st Part
plotColumn <- table(roadrace$Maine)
barplot(plotColumn, ylab = "frequency", ylim = c(0,5000), col = c('white', 'green'), main = "Maine and Away", legend=plotColumn)

## 2nd Part
maineGroup = subset(roadrace$Time..minutes., roadrace$Maine=="Maine")
hist(maineGroup, xlab = "Runners time in minutes", ylab="frequency",ylim = c(0,2000), xlim = c(20, 140), main="Histogram of Maine Group")

summary(maineGroup)
IQR(maineGroup)
range(maineGroup)
sd(maineGroup)

awayGroup = subset(roadrace$Time..minutes., roadrace$Maine=="Away")
hist(awayGroup, xlab = "Runners time in minutes", ylab="frequency",ylim = c(0,2000), xlim = c(20, 140), main="Histogram of Away Group")

summary(awayGroup)
IQR(awayGroup)
range(awayGroup)
sd(awayGroup)

## 3rd Part
### With Outliers
boxplot(maineGroup, awayGroup, names = c("Maine", "Away"), outline = TRUE)
### Without Outliers
boxplot(maineGroup, awayGroup, names = c("Maine", "Away"), outline = FALSE)

## 4th Part
maleGroup = subset(roadrace$Age, roadrace$Sex=="M")
femaleGroup = subset(roadrace$Age, roadrace$Sex=="F")

##with Outliers
boxplot(maleGroup, femaleGroup, names = c("Males", "Females"), outline = TRUE)
## without Outliers
boxplot(maleGroup, femaleGroup, names = c("Males", "Females"), outline = FALSE)

summary(maleGroup)
IQR(maleGroup)
range(maleGroup)
sd(maleGroup)

summary(femaleGroup)
IQR(femaleGroup)
range(femaleGroup)
sd(femaleGroup)


### Question 2

motorcycles = read.csv("motorcycle.csv")

boxplot(motorcycles)

fatal <- motorcycles$Fatal.Motorcycle.Accidents

boxplot(fatal, xlab="Fatal Motorcycle Accidents")

summary(fatal)
IQR(fatal)
range(fatal)
sd(fatal)

Lowerboundary = max((quantile(fatal, prob=0.25) - 1.5*IQR(fatal)), min(fatal))

Upperboundary = min((quantile(fatal, prob=0.75) + 1.5*IQR(fatal)), max(fatal))

result = fatal[which(fatal < Lowerboundary | fatal > Upperboundary)]

subset(motorcycles$County,motorcycles$Fatal.Motorcycle.Accidents == result)
