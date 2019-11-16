#Reading bodytemp-heartrate csv file
data = read.csv(file = "bodytemp-heartrate.csv", sep = ",", header = T)

# Subsetting the data based on the gender 
data.male = subset(data, gender == "1") 
data.female = subset(data, gender == "2")

# Boxplot between body_temperature and gender
boxplot(body_temperature ~ gender, data = data, names = c("male", "female"), main = "Body Temperature")

# Storing the body_temperatures of male in temp.male and female in temp.female 
temp.male = data.male$body_temperature
temp.female = data.female$body_temperature

#QQ plots of body_temperatures of male and female 
qqnorm(temp.male)
qqnorm(temp.female)

#Performing t-test for body_temperatures of male and female with two-sided alternative 
t.test(temp.male, temp.female, alternative = "two.sided", var.equal = F)