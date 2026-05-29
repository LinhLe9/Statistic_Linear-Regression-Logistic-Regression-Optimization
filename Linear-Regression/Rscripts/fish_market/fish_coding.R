install.packages("corrplot")
install.packages("moments")

library(dplyr)
library(car) #VIF
library(corrplot)
library("moments") #kutoris


#load data file
print("Read and input dataset")
fish_data <- read.csv("Fishmarket.csv")

# Pre-processing data set
# check missing value
print ("Check missing value in dataset")
sum(is.na(fish_data))
#0

# check duplicate value 
print("Check duplicated data in dataset")
sum(duplicated(fish_data))
#0

# one hot encoding for species variables
dummies <- model.matrix(~ Species - 1, data = fish_data)
fish_onehot <- cbind(fish_data[, -which(names(fish_data) == "Species")], dummies)
M <- cor(fish_onehot )

# heat map cor matrix to identify multilinearity
print("Show correlation matrix plot")
par(mfrow=c(1,1))
corrplot(M, method = "color", type = "upper", 
         addCoef.col = "black", 
         number.cex = 0.7, 
         tl.cex = 0.5, tl.col = "black")

# histogram applied only on numeric variables
print ("Show histogram plot of each variables in dataset")
numeric_cols <- fish_data[sapply(fish_data, is.numeric)]
par(mfrow = c(2, 3)) 
for(i in 1:ncol(numeric_cols)) {
  hist(numeric_cols[[i]], 
       main = names(numeric_cols)[i], 
       xlab = "Value", col = "skyblue", border = "white")
}
# kutoris 
print("Show kurtosis metric of dataset")
kurt_values <- sapply(numeric_cols, kurtosis)
print(kurt_values)

# skewness
print("Show skewness metric of dataset")
skew_values <- sapply(numeric_cols, skewness)
print(skew_values)

# identify outlier of weight in general
print("Show boxplot of whole dataset")
par(mfrow=c(1,1))
weight_BP <-Boxplot(fish_data$Weight, 
                    main = "Boxplot chart of Weight", 
                    id = list(n = Inf, col = "red", horizontal = TRUE))
outlier_data <- fish_data[weight_BP , ]

# identify outlier of weight by species
print("Show boxplot of whole dataset by species")
weight_outlier_s <-Boxplot(Weight ~ Species, data = fish_data,
                           col = terrain.colors(7),
                           main = "Boxplot chart of Weight by Species",
                           id = list(n = Inf)) 
outlier_data_s <- fish_data[weight_outlier_s , ]

# scatter plot to see relationship
print("Show scatter plot of weight variable vs other variables")
par(mfrow=c(2,3))
plot(fish_data$Length1, fish_data$Weight, 
    main="Length1 vs Weight", 
    xlab="Length1", 
    ylab="Weight")
plot(fish_data$Length2, fish_data$Weight, 
     main="Length2 vs Weight", 
     xlab="Length2", 
     ylab="Weight")
plot(fish_data$Length3, fish_data$Weight, 
     main="Length3 vs Weight", 
     xlab="Length3", 
     ylab="Weight")
plot(fish_data$Height, fish_data$Weight, 
     main="Height vs Weight", 
     xlab="Height", 
     ylab="Weight")
plot(fish_data$Width, fish_data$Weight, 
     main="Width vs Weight", 
     xlab="Width", 
     ylab="Weight")

# transformation --> check if any data cell = 0
print("Total data point equal to 0 in dataset")
fish_data[rowSums(fish_data == 0, na.rm = TRUE) > 0, ]
# 41 : Weight = 0 --> delete

# delete 0 line
print("Delete data line weight equal to 0 in dataset")
fish_cleaned <- fish_data[fish_data$Weight > 0, ]

# histogram of weight after transformation
print("Show histogram of each transformation of weight dimension")
par(mfrow=c(1,3))
hist(sqrt(fish_cleaned$Weight),main = "Square root-transformed Weight", 
     col = "skyblue", 
     xlab = "square(Weight)")

hist(log(fish_cleaned$Weight),
     main = "Log-transformed Weight", 
     col = "skyblue", 
     xlab = "log(Weight)")

hist((fish_cleaned$Weight)^(1/3),
     main = "Cube root-transformed Weight",
     col = "skyblue", 
     xlab = "Weight^(1/3)")

# check skewwness and kurtosis after transforming
trans_list <- list(
  Sqrt = sqrt(fish_cleaned$Weight),
  Cube_Root = (fish_cleaned$Weight)^(1/3),
  Log = log(fish_cleaned$Weight)
)
skew_trans <- sapply(trans_list, skewness)
print("Show skewness metric of each weight transformation")
print(skew_trans)
kurs_trans <- sapply(trans_list, kurtosis)
print("Show kurtosis metric of each weight transformation")
print(kurs_trans)

# plot with other variables after transforming
# square root
print("Show scatter plot of square root of weight interacting with other variables")
plot(fish_cleaned$Length1, sqrt(fish_cleaned$Weight), main="Length1 vs sqrt(Weight)", xlab="Length1", ylab="sqrt(Weight)")
plot(fish_cleaned$Length2, sqrt(fish_cleaned$Weight), main="Length2 vs sqrt(Weight)", xlab="Length2", ylab="sqrt(Weight)")
plot(fish_cleaned$Length3, sqrt(fish_cleaned$Weight), main="Length3 vs sqrt(Weight)", xlab="Length3", ylab="sqrt(Weight)")
plot(fish_cleaned$Height, sqrt(fish_cleaned$Weight), main="Height vs sqrt(Weight)", xlab="Height", ylab="sqrt(Weight)")
plot(fish_cleaned$Width, sqrt(fish_cleaned$Weight), main="Width vs sqrt(Weight)", xlab="Width", ylab="sqrt(Weight)")
pairs(~ I(Weight^(1/2)) + Length1 + Height + Width, data = fish_cleaned,main = "Square Root Weight vs Dimensions")

# cube root
print("Show scatter plot of cube root of weight interacting with other variables")
plot(fish_cleaned$Length1, (fish_cleaned$Weight)^(1/3), main="Length1 vs Weight^(1/3)", xlab="Length1", ylab="Weight^(1/3)")
plot(fish_cleaned$Length2, (fish_cleaned$Weight)^(1/3), main="Length2 vs Weight^(1/3)", xlab="Length2", ylab="Weight^(1/3)")
plot(fish_cleaned$Length3, (fish_cleaned$Weight)^(1/3), main="Length3 vs Weight^(1/3)", xlab="Length3", ylab="Weight^(1/3)")
plot(fish_cleaned$Height, (fish_cleaned$Weight)^(1/3), main="Height vs Weight^(1/3)", xlab="Height", ylab="Weight^(1/3)")
plot(fish_cleaned$Width, (fish_cleaned$Weight)^(1/3), main="Width vs Weight^(1/3)", xlab="Width", ylab="Weight^(1/3)")
pairs(~ I(Weight^(1/3)) + Length1 + Height + Width, data = fish_cleaned,main = "Cube Root Weight vs Dimensions")

# log - linear
print("Show scatter plot of logarithm of weight interacting with other variables")
plot(fish_cleaned$Length1, log(fish_cleaned$Weight), main="Length1 vs log(Weight)", xlab="Length1", ylab="ln(Weight)")
plot(fish_cleaned$Length2, log(fish_cleaned$Weight), main="Length2 vs log(Weight)", xlab="Length2", ylab="ln(Weight)")
plot(fish_cleaned$Length3, log(fish_cleaned$Weight), main="Length3 vs log(Weight)", xlab="Length3", ylab="ln(Weight)")
plot(fish_cleaned$Height, log(fish_cleaned$Weight), main="Height vs log(Weight)", xlab="Height", ylab="ln(Weight)")
plot(fish_cleaned$Width, log(fish_cleaned$Weight), main="Width vs log(Weight)", xlab="Width", ylab="ln(Weight)")
pairs(~ I(log(Weight)) + Length1 + Height + Width, data = fish_cleaned,main = "Log Weight vs Dimensions")

# log - log
print("Show scatter plot of logarithm of weight interacting with logarithm of other variables")
plot(log(fish_cleaned$Length1), log(fish_cleaned$Weight), main="log(Length1) vs log(Weight)", xlab="ln(Length1)", ylab="ln(Weight)")
plot(log(fish_cleaned$Length2), log(fish_cleaned$Weight), main="log(Length2) vs log(Weight)", xlab="ln(Length2)", ylab="ln(Weight)")
plot(log(fish_cleaned$Length3), log(fish_cleaned$Weight), main="log(Length3) vs log(Weight)", xlab="ln(Length3)", ylab="ln(Weight)")
plot(log(fish_cleaned$Height), log(fish_cleaned$Weight), main="log(Height) vs log(Weight)", xlab="ln(Height)", ylab="ln(Weight)")
plot(log(fish_cleaned$Width), log(fish_cleaned$Weight), main="log(Width) vs log(Weight)", xlab="ln(Width)", ylab="ln(Weight)")
pairs(~ I(log(Weight)) + log(Length1) + log(Height) + log(Width), data = fish_cleaned,main = "Log Weight vs Log Dimensions")

# model test transformation
model_t1 <- lm(sqrt(Weight)~Species+Length1+Length2+Length3+Height+Width, data = fish_cleaned)
model_t2 <- lm((Weight)^(1/3)~Species+Length1+Length2+Length3+Height+Width, data = fish_cleaned)
model_t3 <- lm(log(Weight)~Species+Length1+Length2+Length3+Height+Width, data = fish_cleaned)
model_t4 <- lm(log(Weight)~Species+log(Length1)+log(Length2)+log(Length3)+log(Height)+log(Width), data = fish_cleaned)
# residual - fitted plot for each transformation
print("Residual - fitted plot for each transformation of weight")
par(mfrow=c(2,2))
plot(model_t1, which=1, main = "Square root of Weight")
plot(model_t2, which=1, main = "Cube root of Weight")
plot(model_t3, which=1, main = "Log of Weight")
plot(model_t4, which=1, main = "Log_log of Weight")

# model test length
model_t_full<-lm(Weight~Species+Length1+Length2+Length3+Height+Width, data = fish_cleaned)
model_t_l1<-lm(Weight~Species+Length1+Height+Width, data = fish_cleaned)
model_t_l2<-lm(Weight~Species+Length2+Height+Width, data = fish_cleaned)
model_t_l3<-lm(Weight~Species+Length3+Height+Width, data = fish_cleaned)
# vif
print("Show VIF test results on each length model")
model_list <- list(Full = model_t_full, L1 = model_t_l1, L2 = model_t_l2, L3 = model_t_l3)
lapply(model_list, vif)

# anova
print("Show anova test results between each individual length variable and full model")
anova(model_t_l1,model_t_full)
anova(model_t_l2,model_t_full)
anova(model_t_l3,model_t_full)
# AIC
print("Show AIC test results between 4 models")
AIC(model_t_full, model_t_l1, model_t_l2, model_t_l3)

# split dataset into train set and test set
# set seed to ensure it wont change each time
set.seed(456) 

# Create index
n <- nrow(fish_cleaned)
train_indices <- sample(1:n, size = 0.8 * n)

# Train và Test
train_data <- fish_cleaned[train_indices, ]
test_data  <- fish_cleaned[-train_indices, ]

# check the species group distribution of each set 
# to see if train and test set have full species and enough quantity for each type
print("Show train data set group by species")
table(train_data$Species)
#Bream     Perch      Pike     Roach     Ruffe     Smelt Whitefish 
#29        43        15        14         8        11         5 

print("Show test data set group by species")
table(test_data$Species)
#Bream     Perch      Pike     Roach     Ruffe     Smelt Whitefish 
#6        12         2         5         3         3         1 

# model full
model_full<-lm(Weight~Species+Length1+Length2+Length3+Height+Width, data = train_data)
print("Summary of full model")
summary(model_full)
# check VIF + D-W
print("VIF test results of full model")
vif(model_full)
print("Durbin-Watson test results of full model")
durbinWatsonTest(model_full)

par(mfrow = c(2, 2))
print("Show plots of full model")
plot(model_full)
# rmse
print("RMSE metric on test data of full model")
pred_full <-predict(model_full, newdata = test_data)
actual_weight <- test_data$Weight
rmse_val_full <- sqrt(mean((actual_weight - pred_full)^2))

# model 2 elimination
model_length3 <- lm(Weight~Species+Length3+Height+Width, data = train_data)
summary(model_length3)
# check VIF + D-W
vif(model_length3)
durbinWatsonTest(model_length3)
#plot
par(mfrow = c(2, 2)) 
plot(model_length3)
#rmse
pred_l3 <-predict(model_length3, newdata = test_data)
rmse_val_l3 <- sqrt(mean((actual_weight - pred_l3)^2))
print(rmse_val_l3)

# model reduce Height, Weight
model_length <- lm(Weight~Species+Length3, data = train_data)
anova(model_length, model_full)

# cube root model 
model_cuberoot <- lm((Weight)^(1/3)~Species+Length3+Height+Width, data = train_data)
summary(model_cuberoot)
# check VIF + D-W
vif(model_cuberoot)
durbinWatsonTest(model_cuberoot)
#plot
par(mfrow = c(2, 2)) 
plot(model_cuberoot)
# rmse
#test set
pred_cube <-predict(model_cuberoot, newdata = test_data)
pred_weight <- pred_cube^3
actual_weight <- test_data$Weight
rmse_val <- sqrt(mean((actual_weight - pred_weight)^2))
print(rmse_val)

# plot
plot(actual_weight, pred_weight, 
main = "Actual vs. Predicted Fish Weight (Test Set) - Cube Root Model",
xlab = "Actual Weight (g)", 
ylab = "Predicted Weight (g)", 
pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)

# model 4 full log
model_fullLog <- lm(log(Weight)~Species+log(Length3) + log(Width) + log(Height), data = train_data)
summary(model_fullLog)
plot(model_fullLog)
# check VIF + D-W
vif(model_fullLog)
durbinWatsonTest(model_fullLog)
# test set 
# RMSE
pred_log <- predict(model_fullLog, newdata  = test_data)
pred_weight_2 <- exp(pred_log)
rmse_val_2 <- sqrt(mean((actual_weight - pred_weight_2 )^2))
print(rmse_val_2)

# plot
plot(actual_weight, pred_weight_2, 
main = "Actual vs. Predicted Fish Weight (Test Set) - Full Log Model",
xlab = "Actual Weight (g)", 
ylab = "Predicted Weight (g)", 
pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)

# model Log only Length3
model_fullLog3 <- lm(log(Weight)~Species+log(Length3), data = train_data)
anova(model_fullLog3,model_fullLog)

# model log with interaction
# model 1
model_interaction1 <- lm(log(Weight)~Species+log(Length3)+log(Width/Length3)+log(Height/Length3), data = train_data)
summary(model_interaction1)
par(mfrow=c(2,2))
plot(model_interaction1)
# check VIF + D-W
vif(model_interaction1)
durbinWatsonTest(model_interaction1)
# rmse
pred_i1 <- predict(model_interaction1, newdata  = test_data)
pred_weight_i1 <- exp(pred_i1)
rmse_i1 <- sqrt(mean((actual_weight - pred_weight_i1 )^2))
print(rmse_i1)

#model 2
model_interaction2 <- lm(log(Weight)~Species+log(Length3)+log(Width*Height), data = train_data)
summary(model_interaction2)
# check VIF + D-W
vif(model_interaction2)
durbinWatsonTest(model_interaction2)
# rmse
pred_i2 <- predict(model_interaction2, newdata  = test_data)
pred_weight_i2 <- exp(pred_i2)
rmse_i2 <- sqrt(mean((actual_weight - pred_weight_i2 )^2))
print(rmse_i2)

#model 3
model_interaction3 <- lm(log(Weight)~Species+log(Length3)+log(Height)+log(Width/Length3), data = train_data)
summary(model_interaction3)
# check VIF + D-W
vif(model_interaction3)
durbinWatsonTest(model_interaction3)
# rmse
pred_i3 <- predict(model_interaction3, newdata  = test_data)
pred_weight_i3 <- exp(pred_i3)
rmse_i3 <- sqrt(mean((actual_weight - pred_weight_i3 )^2))
print(rmse_i3)

# model 4
model_interaction4 <- lm((Weight)^(1/3)~Species+Length3 * Width * Height, data = train_data)
summary(model_interaction4)
# check VIF + D-W
vif(model_interaction4)
durbinWatsonTest(model_interaction4)
# rmse
pred_i4 <-predict(model_interaction4, newdata = test_data)
pred_weight_i4 <- pred_i4^3
rmse_i4 <- sqrt(mean((actual_weight - pred_weight_i4)^2))
print(rmse_i4)

# simple model with log(length3)
model_log_length3 <- lm(log(Weight)~Species+log(Length3), data = train_data)
summary(model_log_length3)
vif(model_log_length3)
durbinWatsonTest(model_log_length3)
pred_ll3 <-predict(model_log_length3, newdata = test_data)
pred_weight_ll3 <- exp(pred_ll3)
rmse_ll3 <- sqrt(mean((actual_weight - pred_weight_ll3)^2))
print(rmse_ll3)

# rmse for the full data set
# full log with interaction term model
pred_f1 <- predict(model_interaction1, newdata  = fish_cleaned)
pred_weight_f1 <- exp(pred_f1)
actual_weight_f = fish_cleaned$Weight
rmse_f1 <- sqrt(mean((actual_weight_f - pred_weight_f1 )^2))
print(rmse_f1)

# cube root model
pred_f2 <-predict(model_cuberoot, newdata = fish_cleaned)
pred_weight_f2 <- pred_f2^3
rmse_f2 <- sqrt(mean((actual_weight_f - pred_weight_f2)^2))
print(rmse_f2)

# input data set of new fish 
new_fish <- data.frame(
  Species = "Perch",
  Length3 = 41.9,
  Height = 12.8,
  Width = 6.9
)
predicted_perch <- predict(model_cuberoot, newdata = new_fish)
estimated_weight <- (predicted_perch)^3
print(paste("Estimated Weight:", round(estimated_weight, 2), "grams"))