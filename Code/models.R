#### Introduction ####
### For this 

## Totally we would consider 5 kinds of models. Some of them are selected by stepwise selection and some are constructed based on experience and refered papers.

# data loading and preprocessing
data <- read.csv("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VispCourses/STAT628/module2/BodyFat.csv")
dim(data)
colnames(data)
summary(data)


## linear regression
model <- lm(BODYFAT~AGE+WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,data = data)
steppedmodel <- step(model,direction = "both")
model1 <- lm(formula = BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST, data = data)
summary(model1)

## quadratic regression
model <- lm(BODYFAT~AGE++WEIGHT+HEIGHT+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST+I(AGE)^2+I(WEIGHT^2)+I(HEIGHT^2)+I(ADIPOSITY^2)+I(NECK^2)+I(CHEST^2)+I(ABDOMEN^2)+I(HIP^2)+I(THIGH^2)+I(KNEE^2)+I(ANKLE^2)+I(BICEPS^2)+I(FOREARM^2)+I(WRIST^2), data=data[,-c(1,3)])
steppedmodel <- step(model,direction = "both")
model2 <- lm(formula = BODYFAT ~ AGE + NECK + ABDOMEN + HIP + BICEPS + WRIST + I(ADIPOSITY^2) + I(CHEST^2) + I(HIP^2) + I(BICEPS^2) + I(WRIST^2), data = data[, -c(1, 3)])
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

