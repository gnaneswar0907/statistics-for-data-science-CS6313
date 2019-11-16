#Scatter plots between body temperature and heart rate for males and females 
plot(body_temperature ~ heart_rate, data = data.male, col = "blue")
points(body_temperature ~ heart_rate, data = data.female, col = "red")

#Correlation between body temperatures and heart rates for males and females 
cor(data.male$body_temperature, data.male$heart_rate) 
cor(data.female$body_temperature, data.female$heart_rate)

#Linear model for body temperature and heart rate for males and females 
model.m <- lm(body_temperature ~ heart_rate, data = data.male) 
model.f <- lm(body_temperature ~ heart_rate, data = data.female)

#Adding model to the same scatter plot 
abline(model.m, col = "blue") 
abline(model.f, col = "red")
text(locator(), labels = c("male", "female"))