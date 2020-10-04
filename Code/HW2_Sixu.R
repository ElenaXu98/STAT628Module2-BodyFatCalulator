library(dplyr)
library(MASS)
library(ggplot2)
library(glmnet)
library(cluster)
library(factoextra)
setwd("/Users/sixuli/Desktop/STAT 628/Module 2/Code/")

# Import Data
body_data <- read.csv("data/BodyFat.csv")
summary(body_data)

# Generate Some Ratios
body_data <- mutate(body_data, AHR = ABDOMEN / HIP)
body_data <- mutate(body_data, WBR = WRIST / BICEPS)
body_data <- mutate(body_data, WFR = WRIST / FOREARM)
body_data <- mutate(body_data, NCR = NECK / CHEST)

# Visualization
attach(body_data)
ggplot(data = body_data, mapping = aes(x = AGE, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = ADIPOSITY, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = WEIGHT, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = HEIGHT, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = NECK, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = CHEST, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = ABDOMEN, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = HIP, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = THIGH, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = KNEE, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = ANKLE, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = BICEPS, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = FOREARM, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = WRIST, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = AHR, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = WBR, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = WFR, y = BODYFAT)) + geom_point()
ggplot(data = body_data, mapping = aes(x = NCR, y = BODYFAT)) + geom_point()


# K-means CLuster and Visualization
k2 <- kmeans(body_data, centers = 2, nstart = 25)
fviz_cluster(k2, data = body_data)


#(body_data[which(ADIPOSITY >= 35),  ])


# Remove Wrong Points
body_data <- body_data[-which(body_data$BODYFAT == 0), ]

# Split Training and Test Data
smp_size <- floor(0.8 * nrow(body_data))
set.seed(628)
train_ind <- sample(seq_len(nrow(body_data)), size = smp_size)

train <- body_data[train_ind, ]
test <- body_data[-train_ind, ]
attach(train)

# Backward Selection (Linear)
full_model_linear <- lm(BODYFAT ~ . - IDNO - DENSITY, data = train)
summary(full_model_linear)
pred <- predict(full_model_linear, newdata = test)
(MAP_Error <- sum(abs(test$BODYFAT - pred) / test$BODYFAT)) / length(test$BODYFAT)

step_model_linear <- stepAIC(full_model_linear, direction = "backward", trace = F)
summary(step_model_linear)
pred <- predict(step_model_linear, newdata = test)
(MAP_Error <- sum(abs(test$BODYFAT - pred) / test$BODYFAT)) / length(test$BODYFAT)


# Backward Selection (Polynomial)
full_model_poly <- lm(BODYFAT ~ (AGE + I(AGE^2) + ADIPOSITY + I(ADIPOSITY^2))^2 
                     - AGE:I(AGE^2) - ADIPOSITY:I(ADIPOSITY^2) 
                     + . - DENSITY -IDNO, data = train)
summary(full_model_poly)

step_model_poly <- stepAIC(full_model_poly, direction = "backward", trace = F)
summary(step_model_poly)
pred <- predict(step_model_poly, newdata = test)
(MAP_Error <- sum(abs(test$BODYFAT - pred) / test$BODYFAT)) / length(test$BODYFAT)


# Linquan Ma their Model (ABDOMEN and WEIGHT)
lm1 <- lm(BODYFAT ~ ABDOMEN + WEIGHT, data = train)
summary(lm1)

pred <- predict(lm1, newdata = test)
(MAP_Error <- sum(abs(test$BODYFAT - pred) / test$BODYFAT)) / length(test$BODYFAT)

# ABDOMEN and HEIGHT
lm2 <- lm(BODYFAT ~ ABDOMEN + HEIGHT, data = train)
summary(lm2)

pred <- predict(lm2, newdata = test)
(MAP_Error <- sum(abs(test$BODYFAT - pred) / test$BODYFAT)) / length(test$BODYFAT)

# AHR and ADIPOSITY
lm3 <- lm(BODYFAT ~ AHR + ADIPOSITY, data = train)
summary(lm3)

pred <- predict(lm3, newdata = test)
(MAP_Error <- sum(abs(test$BODYFAT - pred) / test$BODYFAT)) / length(test$BODYFAT)


# Ridge Regression
x <- model.matrix(BODYFAT~. - IDNO - DENSITY, train)[,-1]
x_test <- model.matrix(BODYFAT~. - IDNO - DENSITY, test)[,-1]
head(x)
y <- train$BODYFAT

grid <- 10^seq(10, -2, length = 100)
ridge_mod <- glmnet(x, y, alpha = 0, lambda = grid)

cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = grid)
plot(cv_fit)

(opt_lambda <- cv_fit$lambda.min)

ridge_pred <- predict(ridge_mod, s=0.017, newx = x_test)
(MAP_Error <- sum(abs(test$BODYFAT - ridge_pred) / test$BODYFAT)) / length(test$BODYFAT)











