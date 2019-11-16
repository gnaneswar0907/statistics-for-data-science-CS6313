#Box plot between heart_rate and gender
boxplot(heart_rate ~ gender, data = data, names= c("male", "female"), main = "Heart Rate")

#Storing heart_rates of males in heart_rate.m and 
#heart_rates of females in heart_rate.f 
heart_rate.m <- data.male$heart_rate
heart_rate.f <- data.female$heart_rate

#Drawing QQ plots of heart_rates of males and females 
qqnorm(heart_rate.m)
qqnorm(heart_rate.f)

#Performing Satterthwaite t-test of heart rate between males 
#and females for two sided #alternative
t.test(heart_rate.m, heart_rate.f, alternative = "two.sided", var.equal = F)