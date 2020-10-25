### STAT628 module2 ###
### team members ###
# Sixu Li sli739@wisc.edu
# Zihang Wang zwang2547@wisc.edu
# Yinqiu Xu   yxu475@wisc.edu

##################### Introduction #####################
# This module we aim to create a calculator for bodyfat measurement. The task can be seperated into the following steps:
# 1. data preprocessing: We deleted or imputed problematic data points. Then we added 9 new ratios as predictors.
# 2. candidate models: We tried to list as many meaningful and effective models as possible.
# 3. cross validation: We performed cross validation and used average MAPE as a criterion. We evaluated the candidate models and found the best one.
# 4. model diagnostics: Using some common methods to check assumptions of the presented model.

##################### Contributions #####################
# 1. data preprocessing: Zihang Wang created and maintained, Sixu Li and Yinqiu Xu revised.
# 2. candidate models: Yinqiu Xu created and maintained, Sixu Li and Zihang Wang revised.
# 3. cross validation: Zihang Wang created and maintained, Sixu Li and Yinqiu Xu revised.
# 4. model diagnostics: Sxiu Li created and maintained, Yinqiu Xu and Zihang Wang revised.
# 5. Shiny app: Yinqiu Xu created and maintained, Sixu Li and Zihang Wang revised.

##################### PART 2: candidate models #####################
library(ggplot2)
library(gmodels)

data=read.csv("cleaned_data.csv")
## linear regression
n = dim(data)[1]

## full model with original variables step
model = lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data = data)
model1 = step(model,direction = "both",k=log(n),trace = F)
summary(model1)

## quadratic regression
model = lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+
             ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+
             WRIST+I(AGE)^2+I(WEIGHT^2)+I(HEIGHT^2)+
             I(ADIPOSITY^2)+I(NECK^2)+I(CHEST^2)+I(ABDOMEN^2)+
             I(HIP^2)+I(THIGH^2)+I(KNEE^2)+I(ANKLE^2)+I(BICEPS^2)+
             I(FOREARM^2)+I(WRIST^2), data=data)
model2 = step(model,direction = "both",k=log(n),trace = F)
summary(model2)

## Ratio  + age + weight + height + adiposity model
model = lm(BODYFAT~ AGE+WEIGHT+HEIGHT+ADIPOSITY+ADOHIPr+
             THIHIPr+WRIFORr+KNETHIr+NECCHEr+WRIBICr+FORBICr+
             ANKTHIr+CHEABDr,data = data)
model3 = step(model,direction = "both",k=log(n),trace = F)
summary(model3)

## Abdomen + weight model
model4 = lm(BODYFAT~ABDOMEN+WEIGHT,data = data)
summary(model4)

## Adiposity + abdomen/hip model
model5 = lm(BODYFAT~ADIPOSITY+ADOHIPr,data=data)
summary(model5)

## age + age^2 + ad + ad^2 
model = lm(BODYFAT ~ (AGE + I(AGE^2) + ADIPOSITY + I(ADIPOSITY^2))^2 - 
             AGE:I(AGE^2) - ADIPOSITY:I(ADIPOSITY^2) +NECK+CHEST+
             ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST, data = data)
model6 = step(model, direction = "both", trace = F, k=log(n))
summary(model6)

## put full model into step function
model = lm(BODYFAT ~ ., data = data)
model7 = step(model, direction = "both", trace = F, k=log(n))
summary(model7)

##################### PART 3: cross validation#####################
# cross validation function
cross_validation = function(model,data,fold=10){
  # generate 10 subsets
  cvlist = list()
  n = rep(1:fold,ceiling(length(data$BODYFAT)/fold))[1:length(data$BODYFAT)]
  set.seed(628)
  temp = sample(n,length(data$BODYFAT))  
  x = 1:fold
  dataseq = 1:length(data$BODYFAT)
  cvlist = lapply(x,function(x) dataseq[which(temp==x)])  
  #print(cvlist)
  
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

# compare candidate model
cross_validation(model1,data)
cross_validation(model2,data)
cross_validation(model3,data)
cross_validation(model4,data)
cross_validation(model5,data)
cross_validation(model6,data)
cross_validation(model7,data)

# final model: model 6
final_model = lm(model6,data=data)
summary(final_model)
ci(final_model)

# prediction on test sample
test = data.frame(AGE=44.9,ADIPOSITY=25.5,NECK=38,ABDOMEN=92.7,WRIST=18.2)
(pre_test = predict(final_model,test,interval = "confidence"))

##################### PART 4: model diagostics #####################

plot(final_model,1,sub="") # check homoscedasticity
plot(final_model,2,sub="") # check qqplot
plot(final_model,4,sub = "") #cook distance
plot(final_model,5,sub = "") # check outliers

# Remove data point 39 and fit final model again
data_new = data[-39, ]
final_model_new = lm(model6,data=data_new)
summary(final_model)
cross_validation(final_model_new,data_new)
cross_validation(final_model,data)
