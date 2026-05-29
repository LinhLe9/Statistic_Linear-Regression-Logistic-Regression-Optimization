install.packages("PRROC")
install.packages("DescTools")
if (!require("glmnet")) install.packages("glmnet")
install.packages("caret", dependencies = TRUE)
install.packages("prodlim")
library(glmnet)
library("moments") #kutoris
library(corrplot)
library(ggplot2)
library(dplyr)
library(car)
library(DescTools)
library(PRROC)
library(caret)

#load data file
print("Read and input dataset")
credit_data <- read.csv("creditcard.csv")
# 284807 obs of 31 variables

# begin EDA
sum(is.na(credit_data))
# [1] 0

sum(duplicated(credit_data))
# [1] 1081
credit_cleaned <- unique(credit_data)
# 283726 obs of 31 variables
sum(duplicated(credit_cleaned))

# heat map cor matrix to identify multilinearity
M <- cor(credit_cleaned)
par(mfrow=c(1,1))
corrplot(M, method = "color", type = "upper", 
         addCoef.col = "black", 
         number.cex = 0.4,    
         tl.cex = 0.6,
         tl.col = "black",
         cl.pos = "r",        
         diag = FALSE)

# histogram applied only on numeric variables
print ("Show histogram plot of each variables in dataset")
numeric_cols <- credit_cleaned[sapply(credit_cleaned, is.numeric)]
# exlude the target variable - 'class' column
numeric_cols$Class <- NULL
n_total <- ncol(numeric_cols)
cols <- 3
rows <- ceiling(n_total / cols) 

# save pict to the local folder instead of show it into R display because it's too big
png("EDA_Histograms_Full.png", width = 1600, height = 2400, res = 150)
par(mfrow = c(rows, cols), 
    mar = c(3, 3, 2, 1), 
    oma = c(1, 1, 1, 1))
for(i in 1:n_total) {
  column_data <- numeric_cols[[i]]
  column_name <- names(numeric_cols)[i]
  
  hist(column_data, 
       main = column_name, 
       xlab = "", 
       col = "skyblue", 
       border = "white",
       cex.main = 0.9, 
       cex.axis = 0.8) 
}

# save and close file 
dev.off()

# pie chart to see the distribution of the class feature
class_counts <- table(credit_cleaned$Class)
labels <- c("Legit (0)", "Fraud (1)")
pct <- round(100 * class_counts / sum(class_counts), 2)
labels <- paste(labels, ": ", pct, "%", sep = "")
par(mfrow=c(1,1))
pie(class_counts, 
    labels = labels, 
    col = c("skyblue", "red"), 
    main = "Distribution of Credit Card Transactions")
    
legend("topright", legend = labels, fill = c("skyblue", "red"), cex = 0.8)

# kutoris 
print("Show kurtosis metric of dataset")
kurt_values <- sapply(numeric_cols, kurtosis)
print(kurt_values)

# skewness
print("Show skewness metric of dataset")
skew_values <- sapply(numeric_cols, skewness)
print(skew_values)

selected_vars <- c("V10", "V12", "V14", "V17", "V28", "Amount")

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

for (var in selected_vars) {
  boxplot(as.formula(paste(var, "~ Class")), 
          data = credit_cleaned,
          main = paste("Boxplot of", var),
          xlab = "Class", 
          ylab = "Value",
          col = c("lightblue", "orange"), 
          border = "black",
          outcol = "red",         
          outcex = 0.5)           
}
par(mfrow = c(1, 1))

boxplot(V28 ~ Class, data = credit_cleaned,
        main = "Boxplot of V28 (Zoomed)",
        col = c("skyblue", "tomato"),
        ylim = c(-2, 2), # limit view point
        outline = FALSE)


boxplot(Amount ~ Class, data = credit_cleaned,
        main = "Boxplot of Amount (Zoomed)",
        col = c("skyblue", "tomato"),
        ylim = c(0, 300), # limit view point
        outline = FALSE)

# draw boxplott for all feature but save it to local file 
png("All_Variables_Boxplots.png", width = 2500, height = 3500, res = 200)
all_vars <- names(credit_cleaned)[sapply(credit_cleaned, is.numeric)]
all_vars <- all_vars[all_vars != "Class"]
par(mfrow = c(rows, cols), 
    mar = c(4, 4, 3, 1),         
    oma = c(2, 2, 2, 2))          

for (var in all_vars) {
  # 1. Calculate quantitile of Class 0 (Legit)
  q_0 <- quantile(credit_cleaned[credit_cleaned$Class == 0, var], probs = c(0.05, 0.95), na.rm = TRUE)
  
  # 2. Calculate quantitile of Class 1 (Fraud)
  q_1 <- quantile(credit_cleaned[credit_cleaned$Class == 1, var], probs = c(0.05, 0.95), na.rm = TRUE)
  
  # 3.ylim will cover Min and Max of two above range)
  y_limits <- c(min(q_0[1], q_1[1]), max(q_0[2], q_1[2]))
  boxplot(as.formula(paste(var, "~ Class")), 
          data = credit_cleaned,
          main = paste("Var:", var),
          col = c("skyblue", "tomato"),
          outline = TRUE,         
          ylim = y_limits,        
          cex.main = 1.2, 
          xlab = "Class", 
          ylab = "Value")
}

dev.off()

summary(credit_cleaned)

# extract hour feature
credit_cleaned$hour <- (credit_cleaned$Time %% 86400) %/% 3600
summary(credit_cleaned$hour)

ggplot(credit_cleaned, aes(x = hour, fill = as.factor(Class))) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = c("skyblue", "red"), 
                    labels = c("Legit", "Fraud")) +
  labs(title = "Transaction Distribution by Hour",
       x = "Hour (0-23h)",
       y = "Density",
       fill = "Class") +
  theme_minimal()

train_df <- credit_cleaned %>% filter(Time <= 86400)
test_df <- credit_cleaned %>% filter(Time > 86400)
summary(train_df$Time)
summary(test_df$Time)

# calculate percentage of outlier
outlier_ratio <- sapply(numeric_cols, function(x) {
  outliers <- boxplot.stats(x)$out
  return(length(outliers) / length(x) * 100)
})

diagnostic_table <- data.frame(
  Variable = names(skew_values),
  Skewness = round(skew_values, 2),
  Outlier_Perc = round(outlier_ratio, 2)
)

print(diagnostic_table)
diagnostic_table$Skew_Status <- ifelse(abs(diagnostic_table$Skewness) > 1, "High", "Normal")
diagnostic_table$Outlier_Status <- ifelse(diagnostic_table$Outlier_Perc > 5, "High", "Normal")
diagnostic_table$Recommendation <- ifelse(
  diagnostic_table$Skew_Status == "High" | diagnostic_table$Outlier_Status == "High",
  "W",""
)
print(diagnostic_table)

diagnostic_table$Capping <- ifelse(
  diagnostic_table$Skew_Status == "High" & diagnostic_table$Outlier_Status == "High", "Cap","")

print(diagnostic_table)

skew_log_amount <- skewness(log1p(train_df$Amount))
log_amount <- log1p(train_df$Amount)
outliers_log <- boxplot.stats(log_amount)$out
outlier_log_ratio <- (length(outliers_log) / length(log_amount)) * 100
print(skew_log_amount)
print(outlier_log_ratio)

vars_to_robust <- diagnostic_table$Variable[diagnostic_table$Recommendation == "W"]
# exclude Time and Amount columns 
vars_to_robust <- setdiff(vars_to_robust, c("Amount", "Time"))
print(vars_to_robust)

# process Robust Scaling 
robust_scale <- function(x) {
  (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
}

train_df_processed <- train_df
train_df_processed[vars_to_robust] <- lapply(train_df[vars_to_robust], robust_scale)
train_df_processed$Amount <- log1p(train_df_processed$Amount)

model_1 <- glm(Class ~ . - Time, data = train_df, family = "binomial")
summary(model_1)
vif_values <- vif(model_1)
print(vif_values)

ODD_table <- exp(cbind(OR = coef(model_1), confint(model_1)))
print(ODD_table)

model_no_hour <- glm(Class ~ . - Time - hour, data = train_df, family = "binomial")
vif_no_hour <- vif(model_no_hour)
print(vif_no_hour)

model_V <- glm(Class ~ . - Time - hour - Amount, data = train_df, family = "binomial")
vif_V <- vif(model_V)
print(vif_V)

model_credit <- glm(Class ~ . - hour, data = credit_cleaned, family = "binomial")
vif_credit <- vif(model_credit)
print(vif_credit)

model_credit_V <- glm(Class ~ . - Time - hour - Amount, data = credit_cleaned, family = "binomial")
vif_credit_V <- vif(model_credit_V)
print(vif_credit_V)

sum(train_df$Amount <= 0)
table(train_df$Amount == 0, train_df$Class)

model_2 <- glm(Class ~ . - Time - Amount + log1p(Amount), 
               data = train_df, family = "binomial")
summary(model_2)

model_3 <- glm(Class ~ . - Time, 
                data = train_df_processed, 
                family = "binomial")
summary(model_3)
vif_values_3 <- vif(model_3)
print(vif_values_3)

# Ví dụ xóa V5 và V3 trước vì VIF > 125
# model_4 <- glm(Class ~ . - Time - V3 - V5, 
#                data = train_df_processed, 
#                family = "binomial")
# vif(model_4)
# summary(model_4)

# anova(model_4, model_3, test="Chisq")
# r2_full <- PseudoR2(model_3, which = "Nagelkerke")
# r2_reduced <- PseudoR2(model_4, which = "Nagelkerke")

# delta_r2 <- r2_full - r2_reduced
# print(paste("Model including V3, V5:", round(r2_full, 4)))
# print(paste("Model excluding V3, V5:", round(r2_reduced, 4)))
# print(paste("Difference:", round(delta_r2, 4)))


# Waste time, can reduce the VIF 
# X_train <- as.matrix(train_df_processed %>% select(-Class, -Time))
# y_train <- train_df_processed$Class

# cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, family = "binomial", type.measure = "auc")

# plot(cv_ridge)
# best_lambda <- cv_ridge$lambda.min
# model_ridge <- glmnet(X_train, y_train, alpha = 0, family = "binomial", lambda = best_lambda)
# summary (model_ridge)

# working on imbalance problem 
# set.seed(42) 
# train_fraud <- train_df_processed %>% filter(Class == 1)
# train_legit <- train_df_processed %>% filter(Class == 0)
# train_legit_sample <- train_legit %>% sample_n(nrow(train_fraud) * 10)
# train_balanced <- rbind(train_fraud, train_legit_sample)
# table(train_balanced$Class)

# process the eliminate feature procedure
# create a table to track this process
get_model_metrics <- function(model, model_name, pseudo) {
  aic_val <- AIC(model)
  deviance_val <- deviance(model)
  fisher_iter <- model$iter 
  
  data.frame(
    Model = model_name,
    AIC = round(aic_val, 2),
    Deviance = round(deviance_val, 2),
    Fisher_Iterations = fisher_iter,
    Pseudo_R2 = round(pseudo, 4), # Thêm round ở đây cho đẹp
    stringsAsFactors = FALSE
  )
}
metrics_table <- get_model_metrics(model_3, "Full Model (V1-V28)",0)
print(metrics_table)

model_5 <- glm(Class ~ . - Time - V5, data = train_df_processed, family = "binomial")
r2_full <- PseudoR2(model_3, which = "Nagelkerke")
r2_reduced <- PseudoR2(model_5, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including V5:", round(r2_full, 4)))
print(paste("Model excluding V5:", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_5, "Minus V5",delta_r2))
print(metrics_table)
vif(model_5)

# exclude V3
model_6 <- glm(Class ~ . - Time - V5 - V3, data = train_df_processed, family = "binomial")
r2_full <- PseudoR2(model_3, which = "Nagelkerke")
r2_reduced <- PseudoR2(model_6, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including V5, V3:", round(r2_full, 4)))
print(paste("Model excluding V5, V3:", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_6, "Minus V5, V3",delta_r2))
print(metrics_table)
vif(model_6)

# exclude V16
model_7 <- glm(Class ~ . - Time - V5 - V3 - V16, data = train_df_processed, family = "binomial")
r2_full <- PseudoR2(model_3, which = "Nagelkerke")
r2_reduced <- PseudoR2(model_7, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including V5, V3, V16:", round(r2_full, 4)))
print(paste("Model excluding V5, V3, V16:", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_7, "Minus V5, V3, V16",delta_r2))
print(metrics_table)
vif(model_7)

# exclude V12
model_8 <- glm(Class ~ . - Time - V5 - V3 - V16 -V12, data = train_df_processed, family = "binomial")
summary(model_8)
r2_full <- PseudoR2(model_3, which = "Nagelkerke")
r2_reduced <- PseudoR2(model_8, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including V5, V3, V16, V12:", round(r2_full, 4)))
print(paste("Model excluding V5, V3, V16, V12:", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_8, "Minus V5, V3, V16, V12",delta_r2))
print(metrics_table)
vif(model_8)

# add V3 again
model_9 <- glm(Class ~ . - Time - V5 - V16 -V12, data = train_df_processed, family = "binomial")
summary(model_9)
vif(model_9)

# add V16 again
model_10 <- glm(Class ~ . - Time - V5 - V3 -V12, data = train_df_processed, family = "binomial")
summary(model_10)
vif(model_10)

# add V5 again
model_11 <- glm(Class ~ . - Time - V16 - V3 -V12, data = train_df_processed, family = "binomial")
summary(model_11)
vif(model_11)

model_final <- step(model_8, direction = "backward")

r2_full <- PseudoR2(model_3, which = "Nagelkerke")
r2_reduced <- PseudoR2(model_final, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including :", round(r2_full, 4)))
print(paste("Model excluding :", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_final, "15 features",delta_r2))
print(metrics_table)
print(r2_full)
summary(model_final)
vif(model_final)

# try to add 12 back
current_formula <- formula(model_final)
# update to auto add V12 as a dependent variable
new_formula <- update(current_formula, . ~ . + V12)
# Run model
model_with_V12 <- glm(new_formula, data = train_df_processed, family = binomial(link = "logit"))
# run VIF
summary(model_with_V12)

r2_full <- PseudoR2(model_3, which = "Nagelkerke")
r2_reduced <- PseudoR2(model_with_V12, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including :", round(r2_full, 4)))
print(paste("Model final + V12 :", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_with_V12, "15 features+12",delta_r2))
print(metrics_table)
vif(model_with_V12)

# remove V17
current_formula <- formula(model_with_V12)
# update to auto add V12 as a dependent variable
new_formula <- update(current_formula, . ~ . - V17 - hour)
# Run model
model_V12_17 <- glm(new_formula, data = train_df_processed, family = binomial(link = "logit"))
# run VIF
summary(model_V12_17)

r2_full <- PseudoR2(model_3, which = "Nagelkerke")
r2_reduced <- PseudoR2(model_V12_17, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including :", round(r2_full, 4)))
print(paste("Model final + V12 -V17, hour :", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_V12_17, "15 features+12-17-hour",delta_r2))
print(metrics_table)
vif(model_V12_17)

anova(model_V12_17, model_3, test = "Chisq")

# 1. Inverse Probability Weighting)
n_total <- nrow(train_df_processed)
weights <- ifelse(train_df_processed$Class == 1, 
                  n_total / (2 * sum(train_df_processed$Class == 1)), 
                  n_total / (2 * sum(train_df_processed$Class == 0)))

# 2. run final model + add weights
# model_weighted_final <- glm(model_final$formula, 
#                            data = train_df_processed, 
#                            family = "binomial", 
#                            weights = weights)

# summary(model_weighted_final)

# applied weight on model 
model_weighted_V12_17 <- glm(model_V12_17$formula, 
                             data = train_df_processed, 
                             family = "binomial", 
                             weights = weights)

summary(model_weighted_V12_17)
vif(model_weighted_V12_17)
# _________________________________________________________________________
# transform and scaling test set
vars_to_scale <- c("V1", "V2", "V3", "V5", "V6", "V7", "V8", "V10", 
                   "V12", "V14", "V16", "V17", "V20", "V21", "V23", "V27", "V28")
# Get median of each feature from train set
train_medians <- sapply(train_df[vars_to_scale], median, na.rm = TRUE)

# Get IQR of each feature from train set
train_iqrs <- sapply(train_df[vars_to_scale], IQR, na.rm = TRUE)

# robust scaling for test set using train set list of median + IQR
# copy test set
test_df_processed <- test_df

# for loop
for (var in vars_to_scale) {
  test_df_processed[[var]] <- (test_df[[var]] - train_medians[var]) / train_iqrs[var]
}

test_df_processed$Amount <- log1p(test_df_processed$Amount)

#__________________________________________________________________________
# Validate assumption 
plot(model_weighted_V12_17, which = 4) # Cook's Distance
train_df_processed[c(20130, 72476), ]
res <- residuals(model_weighted_V12_17, type = "pearson")

# plot residence <> time
plot(res, 
     main = "Residuals vs Observation Order", 
     ylab = "Pearson Residuals", 
     pch = 20, 
     col = rgb(0,0,0,0.2)) 
abline(h = 0, col = "red", lwd = 2)
#_______________________________________________________CAPPING
# Apply capping
# 1. Copy
train_capped <- train_df_processed 
test_capped <- test_df_processed

# 2.cols to apply
vars_to_cap <- diagnostic_table$Variable[diagnostic_table$Capping == "Cap"]

# 3. Apply capping
for(col in vars_to_cap) {
  # cal parameter on train
  lower_val <- quantile(train_capped[[col]], 0.01, na.rm = TRUE)
  upper_val <- quantile(train_capped[[col]], 0.99, na.rm = TRUE)
  
  # apply on TRAIN
  train_capped[[col]] <- pmax(lower_val, pmin(train_capped[[col]], upper_val))
  
  # Appy on TEST (lower_val & upper_val of TRAIN)
  test_capped[[col]] <- pmax(lower_val, pmin(test_capped[[col]], upper_val))
}

current_formula <- formula(model_with_V12)
# update to auto add V12 as a dependent variable
new_formula <- update(current_formula, . ~ . - V17 - hour)
# Run model
model_V12_17_capped <- glm(new_formula, 
                           data = train_capped, 
                           family = binomial(link = "logit"))
summary(model_V12_17_capped)
plot(model_V12_17_capped, which = 4) # Cook's Distance
print(metrics_table)


cooks_d <- cooks.distance(model_V12_17_capped)

threshold <- 4 / nrow(train_capped)
adj_factor <- ifelse(cooks_d > threshold, threshold / cooks_d, 1)
final_weights <- weights * adj_factor

model_weighted_capped <- glm(model_V12_17_capped$formula, 
                             data = train_capped, 
                             family = "binomial", 
                             weights = final_weights)
summary(model_weighted_capped)
vif(model_weighted_capped)

plot(model_weighted_capped, which = 4) # Cook's Distance
# plot residence <> time
res <- residuals(model_weighted_capped, type = "pearson")
plot(res, 
     main = "Residuals vs Observation Order", 
     ylab = "Pearson Residuals", 
     pch = 20, 
     col = rgb(0,0,0,0.2)) 
abline(h = 0, col = "red", lwd = 2)

probs_weighted_cap <- predict(model_weighted_capped, newdata = test_capped, type = "response")

# create a threshold sequence
f1_results_cap <- sapply(thresholds, function(t) {
  y_pred_cap <- ifelse(probs_weighted_cap > t, 1, 0)
  y_pred_cap <- factor(y_pred_cap, levels = c(0, 1))
  y_true <- factor(test_capped$Class, levels = c(0, 1))
  
  # Calculate F1 score from confustion matrix
  cm <- confusionMatrix(y_pred_cap, y_true, positive = "1")
  return(cm$byClass["F1"])
})

# Fill Na by 0
f1_results_cap[is.na(f1_results_cap)] <- 0

# Find the best threshold
best_t_cap <- thresholds[which.max(f1_results_cap)]
cat("Best threshold for F1 score on no weighted model:", best_t_cap, "\n")
cat("max F1 Score on no weighted model:", max(f1_results_cap), "\n")

# Confusion matrix
final_labels_cap <- ifelse(probs_weighted_cap > 0.84, 1, 0)
final_cm_cap <- confusionMatrix(factor(final_labels_cap, levels=c(0,1)), 
                                  factor(test_capped$Class, levels=c(0,1)), 
                                  positive = "1")
final_cm_cap


# _________________________________________________
# NO EXCESSS____________________________________




























# predict using final model 
probs_no_weight_12 <- predict(model_V12_17, newdata = test_df_processed, type = "response")

# predict using latest model
probs_weighted_12 <- predict(model_weighted_V12_17, newdata = test_df_processed, type = "response")

pr_weighted_12 <- pr.curve(scores.class0 = probs_weighted_12[test_df_processed$Class == 1],
                           scores.class1 = probs_weighted_12[test_df_processed$Class == 0], curve = TRUE)

pr_no_weight_12 <- pr.curve(scores.class0 = probs_no_weight_12[test_df_processed$Class == 1],
                            scores.class1 = probs_no_weight_12[test_df_processed$Class == 0], curve = TRUE)


par(mfrow = c(1, 1))
# PR no weighted
plot(pr_no_weight_12, col = "red", main = "Precision-Recall Curve Comparison")
# PR weighted
plot(pr_weighted_12, col = "blue", add = TRUE)
# add legend
legend("bottomleft", 
       legend = c("Non-Weighted Model", "Weighted Model"), 
       col = c("red", "blue"), 
       lty = 1,     
       lwd = 2,      
       bty = "n")   


# MODEL 12 17 weighted 
# find the best threshold
# create a threshold sequence
thresholds <- seq(0.01, 0.99, by = 0.01)
f1_results_12 <- sapply(thresholds, function(t) {
  y_pred_12 <- ifelse(probs_weighted_12 > t, 1, 0)
  y_pred_12 <- factor(y_pred_12, levels = c(0, 1))
  y_true <- factor(test_df_processed$Class, levels = c(0, 1))
  
  # Calculate F1 score from confustion matrix
  cm <- confusionMatrix(y_pred_12, y_true, positive = "1")
  return(cm$byClass["F1"])
})

# Fill Na by 0
f1_results_12[is.na(f1_results_12)] <- 0

# Find the best threshold
best_t_12 <- thresholds[which.max(f1_results_12)]
cat("Best threshold for F1 score:", best_t_12, "\n")
cat("max F1 Score:", max(f1_results_12), "\n")


# Confusion matrix
final_labels_12 <- ifelse(probs_weighted_12 > 0.99, 1, 0)
final_cm_12 <- confusionMatrix(factor(final_labels_12, levels=c(0,1)), 
                               factor(test_df_processed$Class, levels=c(0,1)), 
                               positive = "1")

final_cm_12$byClass
cm_table_df <- as.data.frame(final_cm_12$table)
ggplot(data = cm_table_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Predicted Class", y = "Actual Class", title = "Confusion Matrix of Weighted Model at threshold = 0.99") +
  theme_minimal()


# MODEL 12 17 no weighted 
# find the best threshold no weighted model
# create a threshold sequence
f1_results_no <- sapply(thresholds, function(t) {
  y_pred_no <- ifelse(probs_no_weight_12 > t, 1, 0)
  y_pred_no <- factor(y_pred_no, levels = c(0, 1))
  y_true <- factor(test_df_processed$Class, levels = c(0, 1))
  
  # Calculate F1 score from confustion matrix
  cm <- confusionMatrix(y_pred_no, y_true, positive = "1")
  return(cm$byClass["F1"])
})

# Fill Na by 0
f1_results_no[is.na(f1_results_no)] <- 0

# Find the best threshold
best_t_no <- thresholds[which.max(f1_results_no)]
cat("Best threshold for F1 score on no weighted model:", best_t_no, "\n")
cat("max F1 Score on no weighted model:", max(f1_results_no), "\n")

# Confusion matrix
final_labels_no <- ifelse(probs_no_weight_12 > 0.24, 1, 0)
final_cm_12_no <- confusionMatrix(factor(final_labels_no, levels=c(0,1)), 
                                  factor(test_df_processed$Class, levels=c(0,1)), 
                                  positive = "1")
cm_table_df_no <- as.data.frame(final_cm_12_no$table)
ggplot(data = cm_table_df_no, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Predicted Class", y = "Actual Class", title = "Confusion Matrix of Non-weighted Model at threshold = 0.24") +
  theme_minimal()

final_cm_12_no$byClass


# find the best threshold for F2-score
thresholds <- seq(0.01, 0.99, by = 0.01)
f2_scores_12 <- sapply(thresholds, function(t) {
  preds_12 <- ifelse(probs_weighted_12 > t, 1, 0)
  precision <- sum(preds_12 == 1 & test_df_processed$Class == 1) / sum(preds_12 == 1)
  recall <- sum(preds_12 == 1 & test_df_processed$Class == 1) / sum(test_df_processed$Class == 1)
  
  f2 <- (5 * precision * recall) / (4 * precision + recall)
  return(f2)
})
# Fill Na by 0
f2_scores_12[is.na(f2_scores_12)] <- 0
best_t_f2_12 <- thresholds[which.max(f2_scores_12)]
cat("Optimal Threshold for F2-Score:", best_t_f2_12, "\n")
cat("max F2 Score:", max(f2_scores_12), "\n")

# at default threshold
pred_labels_12 <- ifelse(probs_weighted_12 > 0.5, 1, 0)
pred_labels_12 <- factor(pred_labels_12, levels = c(0, 1))
true_labels <- factor(test_df_processed$Class, levels = c(0, 1))

# Confusion Matrix
conf_matrix <- confusionMatrix(pred_labels_12, true_labels, positive = "1")
print(conf_matrix)


# calculate cost for each model 
# No Weighted
total_fp_cost_no <- 66 * 16.34
fn_cases_no <- test_df_processed[test_df_processed$Class == 1 & final_labels_no == 0, ]
total_fn_cost_no <- sum(expm1(fn_cases_no$Amount)) # Hoặc Amount gốc tùy dữ liệu

cat("--- Cost of NO WEIGHTED Model---\n")
cat("FP Cost:", total_fp_cost_no, "€\n")
cat("FN Cost:", total_fn_cost_no, "€\n")
cat("Total:", total_fp_cost_no + total_fn_cost_no, "€\n")

# weighted
total_fp_cost_12 <- 191 * 16.34
fn_cases_12 <- test_df_processed[test_df_processed$Class == 1 & final_labels_12 == 0, ]
total_fn_cost_12 <- sum(expm1(fn_cases_12$Amount)) # Hoặc Amount gốc tùy dữ liệu

cat("--- Cost of WEIGHTED 12 Model---\n")
cat("FP Cost:", total_fp_cost_12, "€\n")
cat("FN Cost:", total_fn_cost_12, "€\n")
cat("Total:", total_fp_cost_12 + total_fn_cost_12, "€\n")


# compare train and test 
# 1. Predict on train set
probs_train <- predict(model_weighted_final, newdata = train_df, type = "response")
train_preds <- ifelse(probs_train > best_t, 1, 0)
cm_train <- confusionMatrix(factor(train_preds, levels=c(0,1)), 
                            factor(train_df$Class, levels=c(0,1)), positive = "1")

# 2. get metrix 
compare_df <- data.frame(
  Metric = c("Accuracy", "Sensitivity (Recall)", "Precision", "F1-Score", "Balanced Accuracy"),
  Train = c(cm_train$overall["Accuracy"], 
            cm_train$byClass["Sensitivity"], 
            cm_train$byClass["Pos Pred Value"], 
            cm_train$byClass["F1"],
            cm_train$byClass["Balanced Accuracy"]),
  Test = c(final_cm$overall["Accuracy"], 
           final_cm$byClass["Sensitivity"], 
           final_cm$byClass["Pos Pred Value"], 
           final_cm$byClass["F1"],
           final_cm$byClass["Balanced Accuracy"])
)

print(compare_df)

# find the best threshold for F2-score
thresholds <- seq(0.01, 0.99, by = 0.01)
f2_scores <- sapply(thresholds, function(t) {
  preds <- ifelse(probs_weighted > t, 1, 0)
  precision <- sum(preds == 1 & test_df_processed$Class == 1) / sum(preds == 1)
  recall <- sum(preds == 1 & test_df_processed$Class == 1) / sum(test_df_processed$Class == 1)
  
  f2 <- (5 * precision * recall) / (4 * precision + recall)
  return(f2)
})
# Fill Na by 0
f2_scores[is.na(f2_scores)] <- 0
best_t_f2 <- thresholds[which.max(f2_scores)]
cat("Optimal Threshold for F2-Score:", best_t_f2, "\n")
cat("max F2 Score:", max(f2_scores), "\n")


# Weighted
total_fp_cost <- 176 * 16.34
fn_cases <- test_df_processed[test_df_processed$Class == 1 & final_labels == 0, ]
total_fn_cost <- sum(expm1(fn_cases$Amount)) # Hoặc Amount gốc tùy dữ liệu

cat("--- Cost of WEIGHTED Model---\n")
cat("FP Cost:", total_fp_cost, "€\n")
cat("FN Cost:", total_fn_cost, "€\n")
cat("Total:", total_fp_cost + total_fn_cost, "€\n")
