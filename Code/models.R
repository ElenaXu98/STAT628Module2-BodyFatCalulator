### STAT628 module2 ###
### team members ###
# Sixu Li
# Zihang Wang
# Yinqiu Xu   yxu475@wisc.edu

##################### Introduction #####################
# This module we aim to create a calculator for bodyfat measurement. The task can be seperated into the following steps:
# 1. data preprocessing: We address some problematic data points by interpolation or deleting.
# 2. modelling: We try to list as many meaningful and effective models as possible.
# 3. cross validation: By cross validation and use MAPE as measure, we evaluate the models we list and present the best one.
# 4. model diagnostic: Using some common methods to check assumptions of the presented model.

##################### PART 1: data preprocessing #####################
data = read.csv("BodyFat.csv")
 data <- read.csv("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VispCourses/STAT628/module2/BodyFat.csv")
dim(data)
colnames(data)
summary(data)
# For point BODYFAT == 0, we simply exclude it.
data = data[-which(dat$BODYFAT==0),]
# For point HEIGHT == 29.5, we use adiposity and weight to recalculate it.
# ADIPOSITY(BMI) = weight/(height^2)  unit: kg/(m^2)
# 1 lbs = 0.4535924 kg
# 1 inches = 0.0254 m
outlier <- data[which(data$HEIGHT==29.5),c("WEIGHT","HEIGHT","ADIPOSITY")]
data[which(data["HEIGHT"]==29.5),"HEIGHT"] <- sqrt((outlier[1]*0.4535924)/outlier[3])/0.0254  # 69.42894
summary(data)
data <- data[,c(2,4:17)] # get rid of INFO and DENSITY

##################### PART 2: modelling #####################
## linear regression
model <- lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data = data)
steppedmodel <- step(model,direction = "both")
model1 <- lm(formula = BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST, data = data)
summary(model1)

## quadratic regression
model <- lm(BODYFAT~AGE++WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST+I(AGE)^2+I(WEIGHT^2)+I(HEIGHT^2)+I(ADIPOSITY^2)+I(NECK^2)+I(CHEST^2)+I(ABDOMEN^2)+I(HIP^2)+I(THIGH^2)+I(KNEE^2)+I(ANKLE^2)+I(BICEPS^2)+I(FOREARM^2)+I(WRIST^2), data=data)
steppedmodel <- step(model,direction = "both")
model2 <- lm(formula = BODYFAT ~ AGE + NECK + ABDOMEN + HIP + BICEPS + WRIST + I(ADIPOSITY^2) + I(CHEST^2) + I(HIP^2) + I(BICEPS^2) + I(WRIST^2), data = data)
summary(model2)


## Ratio  + age + weight + height + adiposity model
data["ADOHIPr"] <- data["ABDOMEN"]/data["HIP"]
data["THIHIPr"] <- data["THIGH"]/data["HIP"]
data["WRIFORr"] <- data["WRIST"]/data["FOREARM"]
data["KNETHIr"] <- data["KNEE"]/data["THIGH"]
data["NECCHEr"] <- data["NECK"]/data["CHEST"]
data["WRIBICr"] <- data["WRIST"]/data["BICEPS"]
data["FORBICr"] <- data["FOREARM"]/data["BICEPS"]
data["ANKTHIr"] <- data["ANKLE"]/data["THIGH"]
data["CHEABDr"] <- data["CHEST"]/data["ABDOMEN"]
model <- lm(BODYFAT~ AGE+WEIGHT+HEIGHT+ADIPOSITY+ADOHIPr+THIHIPr+WRIFORr+KNETHIr+NECCHEr+WRIBICr+FORBICr+ANKTHIr+CHEABDr,data = data)
steppedmodel <- step(model,direction = "both")
model3 <- lm(formula = BODYFAT ~ AGE + ADIPOSITY + ADOHIPr + KNETHIr + NECCHEr + WRIBICr + FORBICr + CHEABDr, data = data)
summary(model3)


## Abdomen + weight model
model4 <- lm(BODYFAT~ABDOMEN+WEIGHT,data = data)
summary(model4)

## Adiposity + abdomen/hip model
model5 <- lm(BODYFAT~ADIPOSITY+ADOHIPr,data=data)
summary(model5)


##################### PART 3: cross validation #####################
## Intro goes here  ##

# split testset
set.seed(123)
a=sample(1:length(data$BODYFAT),50)
test = data[a,]
train = data[-a,]

# cross validation function
cross_validation = function(model,data,fold=10,seed=2020){
  # generate 10 subsets
  cvlist = list()
  n = rep(1:fold,ceiling(length(data$BODYFAT)/fold))[1:length(data$BODYFAT)]
  set.seed(seed)
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








