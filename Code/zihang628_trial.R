library(ggplot2)
library(gridExtra)
library(corrplot)
library(factoextra)
library(psych)
library(car)

dat = read.csv("BodyFat.csv")
summary(dat)

#PCA
my_dat = dat[,3:17]
my_dat$DENSITY=scale(my_dat$DENSITY)
my_dat$AGE=scale(my_dat$AGE)
my_dat$WEIGHT=scale(my_dat$WEIGHT)
my_dat$HEIGHT=scale(my_dat$HEIGHT)
my_dat$ADIPOSITY=scale(my_dat$ADIPOSITY)
my_dat$NECK=scale(my_dat$NECK)
my_dat$CHEST=scale(my_dat$CHEST)
my_dat$ABDOMEN=scale(my_dat$ABDOMEN)
my_dat$HIP=scale(my_dat$HIP)
my_dat$THIGH=scale(my_dat$THIGH)
my_dat$KNEE=scale(my_dat$KNEE)
my_dat$ANKLE=scale(my_dat$ANKLE)
my_dat$BICEPS=scale(my_dat$BICEPS)
my_dat$FOREARM=scale(my_dat$FOREARM)
my_dat$WRIST=scale(my_dat$WRIST)

comp = princomp(my_dat,cor = TRUE)
summary(comp)
screeplot(comp)
a=comp$loadings
a=a[,1:2]
plot(a[,1],a[,2])

#plot of bodyfat
ggplot(dat, aes(x = BODYFAT)) + 
  geom_histogram()+         
  xlab("bodyfat") + ylab("frequency") +   
  theme_classic() 
hist(dat$BODYFAT)

#rule out outlier
summary(dat$BODYFAT)
dat[which(dat$BODYFAT==min(dat$BODYFAT)),]
dat[which(dat$BODYFAT==max(dat$BODYFAT)),]

mydat = dat[-which(dat$BODYFAT==0),]
mydat = mydat[-which(mydat$BODYFAT==max(mydat$BODYFAT)),]
mydat = mydat[,-1]
str(mydat)

#correlation plot
corr <- cor(mydat[,c(2:17)])  
corrplot(corr,tl.col="black",order = "hclust")  

#saperate scatter plot
plot(dat$DENSITY,dat$BODYFAT)
plot(dat$AGE,dat$BODYFAT)
plot(dat$WEIGHT,dat$BODYFAT)
plot(dat$HEIGHT,dat$BODYFAT)
plot(dat$ADIPOSITY,dat$BODYFAT)
plot(dat$NECK,dat$BODYFAT)
plot(dat$CHEST,dat$BODYFAT)
plot(dat$ABDOMEN,dat$BODYFAT)
plot(dat$HIP,dat$BODYFAT)
plot(dat$THIGH,dat$BODYFAT)
plot(dat$KNEE,dat$BODYFAT)
plot(dat$ANKLE,dat$BODYFAT)
plot(dat$BICEPS,dat$BODYFAT)
plot(dat$FOREARM,dat$BODYFAT)
plot(dat$WRIST,dat$BODYFAT)

pairs(dat[,2:17])
pairs.panels(dat[,2:17])
kappa(dat[,2:17])

#linear regression model
full_model = lm(BODYFAT~.,data = mydat)
summary(full_model)

par(mfrow = c(2, 2))
plot(full_model)
par(mfrow = c(1,1))

plot(residuals(full_model))
vif(full_model)

#step model
step_model = step(full_model)
summary(step_model)

#remove density?
mydat2 = mydat[,-2]
full_model2 = lm(BODYFAT~.,data = mydat2)
summary(full_model2)

par(mfrow = c(2, 2))
plot(full_model2)
par(mfrow = c(1,1))

plot(residuals(full_model2))
vif(full_model2)

#step model
step_model2 = step(full_model2)
summary(step_model2)
















