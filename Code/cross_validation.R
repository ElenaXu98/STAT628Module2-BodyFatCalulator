#stat 628 cross validation zihang wang 2020.10.9

setwd("/Users/sixuli/Desktop/STAT 628/Module 2/Code/")
# data cleaning
dat = read.csv("data/BodyFat.csv")
dat = dat[-which(dat$BODYFAT==0),]
dat = dat[-which(dat$HEIGHT==29.5),]
dat = dat[,c(2,4:17)]

# split testset
set.seed(123)
a=sample(1:length(dat$BODYFAT),50)
test = dat[a,]
train = dat[-a,]

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
    validation_set = dat[cvlist[[i]],]
    training_set = dat[-cvlist[[i]],]
    my_model = lm(model,data=training_set)
    validation_prediction = predict(my_model,validation_set)
    my_mape = sum(abs((validation_set$BODYFAT-validation_prediction)/validation_set$BODYFAT))/length(validation_prediction)
    mape = c(mape,my_mape)
  }
  final_mape = mean(mape)
  return(final_mape)
}

# compare candicate model
model1 = "BODYFAT~."
cross_validation(model1,train)





