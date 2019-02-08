#Data Cleaning and Transformation
# Import the "airquality" dataset 
data("airquality")
head(airquality)
mean(airquality$Ozone) # Calculate mean for ozone without removing the 'NA'
mean(airquality$Ozone,na.rm=TRUE) # Calculate mean for ozone after removing the 'NA'
#Descriptive Statistics
summary(airquality)
#Replace 'NA' by median
data = airquality
data$Ozone = ifelse(is.na(data$Ozone),median(data$Ozone,na.rm=TRUE),data$Ozone)
data$Solar.R = ifelse(is.na(data$Solar.R),median(data$Solar.R,na.rm=TRUE),data$Solar.R)
summary(data)
head(data)
#Adding a new categorical variable Solar.Danger
data$Solar.Danger = data$Solar.R>100
head(data)
#Data Transformation 
brks = c(0,50,100,150,200,250,300,350)
data$Solar.R = cut(data$Solar.R,breaks = brks,lowest = TRUE) # converts values into ranges
head(data)
data1 = data
data1$Month = gsub(5,"May",data1$Month) #substitutes numbers to text in months column
data1$Month = gsub(6,"June",data1$Month)
data1$Month = gsub(7,"July",data1$Month)
data1$Month = gsub(8,"Aug",data1$Month)
data1$Month = gsub(9,"Sep",data1$Month)
