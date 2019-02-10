library(ggplot2)
library(cowplot)
library(randomForest)
#Load the data from the .csv file
data <- read.csv("/Users/varun/Desktop/git/heart_disease.csv", header=FALSE)
head(data) #data without column names
colnames(data) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","hd")
head(data)  


  #"sex",# 0 = female, 1 = male
  #"cp", # chest pain
  # 1 = typical angina,
  # 2 = atypical angina,
  # 3 = non-anginal pain,
  # 4 = asymptomatic
  #"trestbps", # resting blood pressure (in mm Hg)
  #"chol", # serum cholestoral in mg/dl
  #"fbs",  # fasting blood sugar greater than 120 mg/dl, 1 = TRUE, 0 = FALSE
  #"restecg", # resting electrocardiographic results
  # 1 = normal
  # 2 = having ST-T wave abnormality
  # 3 = showing probable or definite left ventricular hypertrophy
  #"thalach", # maximum heart rate achieved
  #"exang",   # exercise induced angina, 1 = yes, 0 = no
  #"oldpeak", # ST depression induced by exercise relative to rest
  #"slope", # the slope of the peak exercise ST segment
  # 1 = upsloping
  # 2 = flat
  # 3 = downsloping
  #"ca", # number of major vessels (0-3) colored by fluoroscopy
  #"thal", # this is short of thalium heart scan
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  #"hd" # (the predicted attribute) - diagnosis of heart disease
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing

head(data)
str(data)
## First, replace "?"s with NAs.
data[data == "?"] <- NA
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.integer(data$ca)
data$thal <- as.factor(data$thal)
## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor
str(data)
set.seed(42)
## impute any missing values in the training set using proximities
data.imputed <- rfImpute(hd ~ ., data = data, iter=6)
#model building
model <- randomForest(hd ~ ., data=data.imputed, proximity=TRUE)
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), 
           each=nrow(model$err.rate),drop=FALSE),
#Calculating the error rate  
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"Healthy"],
          model$err.rate[,"Unhealthy"]))
#plotting the graph
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
