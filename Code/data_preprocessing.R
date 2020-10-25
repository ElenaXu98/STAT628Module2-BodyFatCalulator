### STAT628 module2 group 10 ###
### team members ###
# Sixu Li sli739@wisc.edu (SL)
# Zihang Wang zwang2547@wisc.edu (ZW)
# Yinqiu Xu yxu475@wisc.edu (YX)

##################### Introduction #####################
# This module we aim to create a calculator for bodyfat measurement. The task can be seperated into the following steps:
# 1. data preprocessing: We deleted or imputed problematic data points. Then we added 9 new ratios as predictors.
# 2. candidate models: We tried to list as many meaningful and effective models as possible.
# 3. cross validation: We performed cross validation and used average MAPE as a criterion. We evaluated the candidate models and found the best one.
# 4. model diagnostics: Using some common methods to check assumptions of the presented model.

##################### Contributions #####################
# 1. data preprocessing: ZW created and maintained, SL and YX revised
# 2. candidate models: YX created and maintained, SL and ZW revised
# 3. cross validation: ZW created and maintained, SL and YX revised
# 4. model diagnostics: SL created and maintained, YX and ZW revised

##################### PART 1: data cleaning #####################
library(ggplot2)

data = read.csv("data/BodyFat.csv")
dim(data)
colnames(data)
summary(data)

#1.delete variables: INFO and DENSITY.
data=data[,-c(1,3)]

#2.delete two people whose body fat percentage are lower than 2%.
data[which(data$BODYFAT<2),]
data = data[-which(data$BODYFAT<2),]

#3.scatter plots and check abnormal points

#deal with the individual 42, recalculate his height using weight and adiposity(BMI).
ggplot(data , mapping = aes(x = HEIGHT, y = BODYFAT)) + geom_point()
(id42=data[which(data$HEIGHT<30),])
height_correct=sqrt(id42$WEIGHT/id42$ADIPOSITY*703)
data$HEIGHT[which(data$HEIGHT<30)]=height_correct
ggplot(data , mapping = aes(x = HEIGHT, y = BODYFAT)) + geom_point()

#plot bodyfat vs adiposity and check individual 39
ggplot(data , mapping = aes(x = ADIPOSITY, y = BODYFAT)) + geom_point()
data[which(data$ADIPOSITY>40),]

#check adiposity(BMI) using its formula based on weight and height.
BMI = 703 * (data$WEIGHT) / (data$HEIGHT)^2
ggplot(data , mapping = aes(x = BMI, y = ADIPOSITY)) + geom_point()
bmi_diff=BMI-data$ADIPOSITY
data[which(bmi_diff==max(bmi_diff)),]
data[which(bmi_diff==min(bmi_diff)),]

# impute the original adiposity with newly calculated value
data$ADIPOSITY[which(bmi_diff==max(bmi_diff))]=BMI[which(bmi_diff==max(bmi_diff))]
data$ADIPOSITY[which(bmi_diff==min(bmi_diff))]=BMI[which(bmi_diff==min(bmi_diff))]
ggplot(data , mapping = aes(x = BMI, y = ADIPOSITY)) + geom_point()

#4.add 9 new ratios as predictors according to background research
data$ADOHIPr = data$ABDOMEN/data$HIP
data$THIHIPr = data$THIGH/data$HIP
data$WRIFORr = data$WRIST/data$FOREARM
data$KNETHIr = data$KNEE/data$THIGH
data$NECCHEr = data$NECK/data$CHEST
data$WRIBICr = data$WRIST/data$BICEPS
data$FORBICr = data$FOREARM/data$BICEPS
data$ANKTHIr = data$ANKLE/data$THIGH
data$CHEABDr = data$CHEST/data$ABDOMEN

# save the cleaned data
write.csv(data,file = "cleaned_data.csv",row.names = FALSE)

