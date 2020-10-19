### STAT628 module2 ###
### team members ###
# Sixu Li sli739@wisc.edu
# Zihang Wang zwang2547@wisc.edu
# Yinqiu Xu   yxu475@wisc.edu

##################### Introduction #####################
# This module we aim to create a calculator for bodyfat measurement. The task can be seperated into the following steps:
# 1. data preprocessing: We address some problematic data points by interpolation or deleting.
# 2. modelling: We try to list as many meaningful and effective models as possible.
# 3. cross validation: By cross validation and use MAPE as measure, we evaluate the models we list and present the best one.
# 4. model diagnostic: Using some common methods to check assumptions of the presented model.

##################### PART 1: data preprocessing #####################
#data = read.csv("BodyFat.csv")
data <- read.csv("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VispCourses/STAT628/module2/BodyFat.csv")
dim(data)
colnames(data)
summary(data)
# For point BODYFAT == 0, we simply exclude it.
data = data[-which(data$BODYFAT==0),]
# For point HEIGHT == 29.5, we use adiposity and weight to recalculate it.
# ADIPOSITY(BMI) = weight/(height^2)  unit: kg/(m^2)
# 1 lbs = 0.4535924 kg
# 1 inches = 0.0254 m
outlier <- data[which(data$HEIGHT==29.5),c("WEIGHT","HEIGHT","ADIPOSITY")]
data[which(data["HEIGHT"]==29.5),"HEIGHT"] <- sqrt((outlier[1]*0.4535924)/outlier[3])/0.0254  # 69.42894
summary(data)
data <- data[,c(2,4:17)] # get rid of INFO and DENSITY

# HEItest <- sqrt((data$WEIGHT*0.4535924)/data$ADIPOSITY)/0.0254
# summary(HEItest-data$HEIGHT)
# plot(x= 1: 251, y= HEItest-data$HEIGHT)
# diff <- HEItest-data$HEIGHT
# data[which(diff == max(diff)),]
# data[which(diff == min(diff)),]
# which(diff == max(diff))
# which(diff == min(diff))
# 
# ADItest <- data$WEIGHT*0.4535924/((data$HEIGHT*0.0254)^2)
# summary(ADItest-data$ADIPOSITY)
# plot(x= 1: 251, y= ADItest-data$ADIPOSITY)
# diff2 <- ADItest-data$ADIPOSITY
# which(diff2 == max(diff2))
# which(diff2 == min(diff2))

data["ADOHIPr"] <- data["ABDOMEN"]/data["HIP"]
data["THIHIPr"] <- data["THIGH"]/data["HIP"]
data["WRIFORr"] <- data["WRIST"]/data["FOREARM"]
data["KNETHIr"] <- data["KNEE"]/data["THIGH"]
data["NECCHEr"] <- data["NECK"]/data["CHEST"]
data["WRIBICr"] <- data["WRIST"]/data["BICEPS"]
data["FORBICr"] <- data["FOREARM"]/data["BICEPS"]
data["ANKTHIr"] <- data["ANKLE"]/data["THIGH"]
data["CHEABDr"] <- data["CHEST"]/data["ABDOMEN"]

data <- data[-c(163,220),]
n <- dim(data)[1]


# split testset
set.seed(2333)
a=sample(1:length(data$BODYFAT),50)
test = data[a,]
train = data[-a,]


##################### PART 2: modelling #####################
## linear regression
model <- lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data = train)
model1 <- step(model,direction = "both",k=log(n),trace = F)
summary(model1)

## quadratic regression
model <- lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST+I(AGE)^2+I(WEIGHT^2)+I(HEIGHT^2)+I(ADIPOSITY^2)+I(NECK^2)+I(CHEST^2)+I(ABDOMEN^2)+I(HIP^2)+I(THIGH^2)+I(KNEE^2)+I(ANKLE^2)+I(BICEPS^2)+I(FOREARM^2)+I(WRIST^2), data=train)
model2 <- step(model,direction = "both",k=log(n),trace = F)
summary(model2)

## Ratio  + age + weight + height + adiposity model

model <- lm(BODYFAT~ AGE+WEIGHT+HEIGHT+ADIPOSITY+ADOHIPr+THIHIPr+WRIFORr+KNETHIr+NECCHEr+WRIBICr+FORBICr+ANKTHIr+CHEABDr,data = train)
model3 <- step(model,direction = "both",k=log(n),trace = F)
summary(model3)

# summary(steppedmodel)
# model6 <- lm(formula = BODYFAT ~ WEIGHT + ABDOMEN + FOREARM + WRIST, data = train)
# model7 <- lm(formula = BODYFAT ~ AGE + ABDOMEN + HIP + WRIST + I(ADIPOSITY^2) + I(CHEST^2) + I(HIP^2), data = train)
# model8 <- lm(formula = BODYFAT ~ ADIPOSITY + ADOHIPr + NECCHEr + WRIBICr + CHEABDr, data = train)


## Abdomen + weight model
model4 <- lm(BODYFAT~AGE+ABDOMEN+WEIGHT,data = train)
summary(model4)

## Adiposity + abdomen/hip model
model5 <- lm(BODYFAT~ADIPOSITY+ADOHIPr,data=train)
summary(model5)

## age + age^2 + ad + ad^2 
model <- lm(BODYFAT ~ (AGE + I(AGE^2) + ADIPOSITY + I(ADIPOSITY^2))^2 - AGE:I(AGE^2) - ADIPOSITY:I(ADIPOSITY^2)  +  NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST, data = train)
model6 <- step(model, direction = "both", trace = F, k=log(n))
summary(model6)

##################### PART 3: cross validation #####################
## Intro goes here  ##


# cross validation function
cross_validation = function(model,data,fold=10){
  # generate 10 subsets
  cvlist = list()
  n = rep(1:fold,ceiling(length(data$BODYFAT)/fold))[1:length(data$BODYFAT)]
  #set.seed(seed)
  temp = sample(n,length(data$BODYFAT))  
  x = 1:fold
  dataseq = 1:length(data$BODYFAT)
  cvlist = lapply(x,function(x) dataseq[which(temp==x)])  
  
  # cross validation
  mape = c()
  for (i in 1:fold) {
    validation_set = data[cvlist[[i]],]
    training_set = data[-cvlist[[i]],]
    my_model = lm(model,data=training_set)
    validation_prediction = predict(my_model,validation_set)
    my_mape = sum(abs((validation_set$BODYFAT-validation_prediction)/validation_set$BODYFAT))/length(validation_prediction)
    mape = c(mape,my_mape)
  }
  final_mape = mean(mape)
  return(final_mape)
}

# compare candicate model
cross_validation(model1,train)
cross_validation(model2,train)
cross_validation(model3,train)
cross_validation(model4,train)
cross_validation(model5,train)
cross_validation(model6,train)

##################### PART 4: plots #####################
library(ggplot2)
ggplot(data = data, mapping = aes(x = ADIPOSITY, y = BODYFAT)) + geom_point()




final_model = lm(model2,data=train)
test_prediction = predict(final_model,test)
test_mape = sum(abs((test$BODYFAT-test_prediction)/test$BODYFAT))/length(test_prediction)
test_mape
summary(model2)

plot(final_model,1) # check homoscedasticity
plot(final_model,2) # check qqplot
plot(final_model,5) # check outliers

### model delete point 39
data2 = data[-39,]
test = data[a,]
train = data[-a,]
model <- lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST+I(AGE)^2+I(WEIGHT^2)+I(HEIGHT^2)+I(ADIPOSITY^2)+I(NECK^2)+I(CHEST^2)+I(ABDOMEN^2)+I(HIP^2)+I(THIGH^2)+I(KNEE^2)+I(ANKLE^2)+I(BICEPS^2)+I(FOREARM^2)+I(WRIST^2), data=train)
model22 <- step(model,direction = "both",k=log(n),trace = F)
summary(model22)
test_prediction = predict(model22,test)
test_mape = sum(abs((test$BODYFAT-test_prediction)/test$BODYFAT))/length(test_prediction)
test_mape




