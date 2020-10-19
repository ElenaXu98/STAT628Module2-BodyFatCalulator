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
library(ggplot2)
setwd("/Users/sixuli/Desktop/STAT 628/Module 2/Code")
data = read.csv("data/BodyFat.csv")
 #data <- read.csv("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VispCourses/STAT628/module2/BodyFat.csv")
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
BMI <- 703 * (data$WEIGHT) / (data$HEIGHT)^2
(data[which(abs(data$ADIPOSITY - BMI) > 1),])
(data[which(data$BODYFAT < 2), ])

# add ratios
data["ADOHIPr"] <- data["ABDOMEN"]/data["HIP"]
data["THIHIPr"] <- data["THIGH"]/data["HIP"]
data["WRIFORr"] <- data["WRIST"]/data["FOREARM"]
data["KNETHIr"] <- data["KNEE"]/data["THIGH"]
data["NECCHEr"] <- data["NECK"]/data["CHEST"]
data["WRIBICr"] <- data["WRIST"]/data["BICEPS"]
data["FORBICr"] <- data["FOREARM"]/data["BICEPS"]
data["ANKTHIr"] <- data["ANKLE"]/data["THIGH"]
data["CHEABDr"] <- data["CHEST"]/data["ABDOMEN"]

# split testset
set.seed(628)
a=sample(1:length(data$BODYFAT),50)
test = data[a,]
train = data[-a,]

# visualization

# ggplot(data = train, mapping = aes(x = AGE, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = ADIPOSITY, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = WEIGHT, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = HEIGHT, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = NECK, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = CHEST, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = ABDOMEN, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = HIP, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = THIGH, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = KNEE, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = ANKLE, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = BICEPS, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = FOREARM, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = WRIST, y = BODYFAT)) + geom_point()
# ggplot(data = train, mapping = aes(x = ADOHIPr, y = BODYFAT)) + geom_point()
#ggplot(data = train, mapping = aes(x = WBR, y = BODYFAT)) + geom_point()
#ggplot(data = train, mapping = aes(x = WFR, y = BODYFAT)) + geom_point()
#ggplot(data = train, mapping = aes(x = NCR, y = BODYFAT)) + geom_point()

ggplot(data = test, mapping = aes(x = AGE, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = ADIPOSITY, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = WEIGHT, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = HEIGHT, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = NECK, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = CHEST, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = ABDOMEN, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = HIP, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = THIGH, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = KNEE, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = ANKLE, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = BICEPS, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = FOREARM, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = WRIST, y = BODYFAT)) + geom_point()
ggplot(data = test, mapping = aes(x = ADOHIPr, y = BODYFAT)) + geom_point()


##################### PART 2: modelling #####################
## linear regression
model <- lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data = train)
model1 <- step(model,direction = "both", trace = F)
#model1 <- lm(formula = BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST, data = data)

summary(model1)

## quadratic regression
model <- lm(BODYFAT~AGE++WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST+I(AGE)^2+I(WEIGHT^2)+I(HEIGHT^2)+I(ADIPOSITY^2)+I(NECK^2)+I(CHEST^2)+I(ABDOMEN^2)+I(HIP^2)+I(THIGH^2)+I(KNEE^2)+I(ANKLE^2)+I(BICEPS^2)+I(FOREARM^2)+I(WRIST^2), data=train)
model2 <- step(model,direction = "both", trace = F)
#model2 <- lm(formula = BODYFAT ~ AGE + NECK + ABDOMEN + HIP + BICEPS + WRIST + I(ADIPOSITY^2) + I(CHEST^2) + I(HIP^2) + I(BICEPS^2) + I(WRIST^2), data = data)
summary(model2)


## Ratio  + age + weight + height + adiposity model
model <- lm(BODYFAT~ AGE+WEIGHT+HEIGHT+ADIPOSITY+ADOHIPr+THIHIPr+WRIFORr+KNETHIr+NECCHEr+WRIBICr+FORBICr+ANKTHIr+CHEABDr,data = train)
model3 <- step(model,direction = "both", trace = F)
#model3 <- lm(formula = BODYFAT ~ AGE + ADIPOSITY + ADOHIPr + KNETHIr + NECCHEr + WRIBICr + FORBICr + CHEABDr, data = data)
summary(model3)


## Abdomen + weight model
model4 <- lm(BODYFAT~ABDOMEN+WEIGHT,data = train)
summary(model4)

## Adiposity + abdomen/hip model
model5 <- lm(BODYFAT~ADIPOSITY+ADOHIPr,data=train)
summary(model5)


##################### PART 3: cross validation #####################
## Intro goes here  ##


# cross validation function
cross_validation = function(model,data,fold=10,seed=2020){
  # generate 10 subsets
  cvlist = list()
  n = rep(1:fold,ceiling(length(data$BODYFAT)/fold))[1:length(data$BODYFAT)]
  #print(n)
  set.seed(seed)
  temp = sample(n,length(data$BODYFAT)) 
  #print(temp)
  x = 1:fold
  dataseq = 1:length(data$BODYFAT)
  cvlist = lapply(x,function(x) dataseq[which(temp==x)])  
  #print(cvlist)
  
  # cross validation
  mape = c()
  for (i in 1:fold) {
    validation_set = data[cvlist[[i]],]
    training_set = data[-cvlist[[i]],]
    #print(summary(model))
    my_model = lm(model,data=training_set)
    #print(summary(my_model))
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

final_model1 <- lm(model1, data = train)
summary(final_model1)

pred_final_model1 <- predict(final_model1, test)
(final_model1_mape <- sum(abs(test$BODYFAT - pred_final_model1)/test$BODYFAT)/length(test))

final_model2 <- lm(model2, data = train)
summary(final_model2)

pred_final_model2 <- predict(final_model2, test)
(final_model2_mape <- sum(abs(test$BODYFAT - pred_final_model2)/test$BODYFAT) / length(test))

final_model3 <- lm(model3, data = train)
summary(final_model3)

pred_final_model3 <- predict(final_model3, test)
(final_model4_mape <- sum(abs(test$BODYFAT - pred_final_model3)/test$BODYFAT) / length(test))


final_model4 <- lm(model4, data = train)
summary(final_model4)

pred_final_model4 <- predict(final_model4, test)
(final_model4_mape <- sum(abs(test$BODYFAT - pred_final_model4)/test$BODYFAT) / length(test))


final_model5 <- lm(model5, data = train)
summary(final_model5)

pred_final_model5 <- predict(final_model5, test)
(final_model5_mape <- sum(abs(test$BODYFAT - pred_final_model5)/test$BODYFAT) / length(test))





