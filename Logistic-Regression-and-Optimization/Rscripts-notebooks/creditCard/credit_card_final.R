library(glmnet)
library("moments") #kutoris
library(corrplot)
library(ggplot2)
library(dplyr)
library(car)
library(DescTools)
library(PRROC)
library(caret)

#______________________________________________ LOADING + CLEANING
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

#________________________________________________ EXPLORE
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

#___________________________________________________EXTRACT + TRANSFORM
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

# Split set
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

# Add  recommendation column
diagnostic_table$Skew_Status <- ifelse(abs(diagnostic_table$Skewness) > 1, "High", "Normal")
diagnostic_table$Outlier_Status <- ifelse(diagnostic_table$Outlier_Perc > 5, "High", "Normal")
diagnostic_table$Recommendation <- ifelse(
  diagnostic_table$Skew_Status == "High" | diagnostic_table$Outlier_Status == "High",
  "W",""
)
print(diagnostic_table)

# Add capping columns
diagnostic_table$Capping <- ifelse(
  diagnostic_table$Skew_Status == "High" & diagnostic_table$Outlier_Status == "High", "Cap","")

print(diagnostic_table)

# check skewness and outlier of log(Amount)
skew_log_amount <- skewness(log1p(train_df$Amount))
log_amount <- log1p(train_df$Amount)
outliers_log <- boxplot.stats(log_amount)$out
outlier_log_ratio <- (length(outliers_log) / length(log_amount)) * 100
print(skew_log_amount)
print(outlier_log_ratio)

# robust scaling 
# exclude Time and Amount columns
vars_to_robust <- diagnostic_table$Variable[diagnostic_table$Recommendation == "W"]
vars_to_robust <- setdiff(vars_to_robust, c("Amount", "Time"))
print(vars_to_robust)

# process Robust Scaling 
medians_train <- sapply(train_df[vars_to_robust], median, na.rm = TRUE)
iqrs_train <- sapply(train_df[vars_to_robust], IQR, na.rm = TRUE)

apply_robust_scale <- function(data, vars, medians, iqrs) {
  for (var in vars) {
    data[[var]] <- (data[[var]] - medians[var]) / iqrs[var]
  }
  return(data)
}

# Apply for train set
train_df_processed <- apply_robust_scale(train_df, vars_to_robust, medians_train, iqrs_train)
train_df_processed$Amount <- log1p(train_df_processed$Amount)

# Apply for test set
test_df_processed <- apply_robust_scale(test_df, vars_to_robust, medians_train, iqrs_train)
test_df_processed$Amount <- log1p(test_df_processed$Amount)

# ___________________________________________ TRAIN MODEL FULL
model_1 <- glm(Class ~ . - Time, data = train_df, family = "binomial")
summary(model_1)
vif_values <- vif(model_1)
print(vif_values)

# VIF too high, check other models
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

# _____________________________________________ TRAIN MODEL ON PROCESSED DATA
sum(train_df$Amount <= 0)
table(train_df$Amount == 0, train_df$Class)

# log
model_2 <- glm(Class ~ . - Time - Amount + log1p(Amount),data = train_df, family = "binomial")
summary(model_2)

# log + scaling
model_3 <- glm(Class ~ . - Time, data = train_df_processed,family = "binomial")
summary(model_3)
vif_values_3 <- vif(model_3)
plot(model_3, which = 4) # Cook's Distance

# ______________________________________________ CAPPING DATASET
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

#_____________________________________________TRAIN MODEL ON CAPPED DATASET
model_3.2 <- glm(Class ~ . - Time, data = train_capped,family = "binomial")
summary(model_3.2)
vif_values_3_2 <- vif(model_3.2)
vif_values_3_2
plot(model_3.2, which = 4)

# VIF too high
# ____________________________________________ ELIMINATE FEATURES
get_model_metrics <- function(model, model_name, pseudo) {
  aic_val <- AIC(model)
  deviance_val <- deviance(model)
  fisher_iter <- model$iter 
  
  data.frame(
    Model = model_name,
    AIC = round(aic_val, 2),
    Deviance = round(deviance_val, 2),
    Fisher_Iterations = fisher_iter,
    Pseudo_R2 = round(pseudo, 4), 
    stringsAsFactors = FALSE
  )
}
metrics_table <- get_model_metrics(model_3.2, "Full Model (V1-V28)",0)
print(metrics_table)

# Exclude V5 
model_5 <- glm(Class ~ . - Time - V5, data = train_capped, family = "binomial")
r2_full <- PseudoR2(model_3.2, which = "Nagelkerke")
r2_reduced <- PseudoR2(model_5, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including V5:", round(r2_full, 4)))
print(paste("Model excluding V5:", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_5, "Minus V5",delta_r2))
print(metrics_table)
vif(model_5)

# exclude V3
model_6 <- glm(Class ~ . - Time - V5 - V3, data = train_capped, family = "binomial")
r2_reduced <- PseudoR2(model_6, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including V5, V3:", round(r2_full, 4)))
print(paste("Model excluding V5, V3:", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_6, "Minus V5, V3",delta_r2))
print(metrics_table)
vif(model_6)

# exclude V17 
model_7 <- glm(Class ~ . - Time - V5 - V3 - V17, data = train_capped, family = "binomial")
r2_reduced <- PseudoR2(model_7, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including V5, V3, V17:", round(r2_full, 4)))
print(paste("Model excluding V5, V3, V17:", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_7, "Minus V5, V3, V17",delta_r2))
print(metrics_table)
vif(model_7)

# exclude V16
model_8 <- glm(Class ~ . - Time - V5 - V3 - V16 -V17, data = train_capped, family = "binomial")
summary(model_8)
r2_reduced <- PseudoR2(model_8, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including V5, V3, V16, V17:", round(r2_full, 4)))
print(paste("Model excluding V5, V3, V16, V17:", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_8, "Minus V5, V3, V16, V17",delta_r2))
print(metrics_table)
vif(model_8)

# **************** VIF < 10 ******************
# add back 
# add V3 again
model_9 <- glm(Class ~ . - Time - V5 - V16 -V17, data = train_capped, family = "binomial")
vif(model_9)

# add V17 again
model_10 <- glm(Class ~ . - Time - V5 - V3 -V16, data = train_capped, family = "binomial")
vif(model_10)

# add V5 again
model_11 <- glm(Class ~ . - Time - V16 - V3 -V17, data = train_capped, family = "binomial")
vif(model_11)

# ***************** run step() ***************
model_reduce <- step(model_8, direction = "backward")
r2_reduced <- PseudoR2(model_reduce, which = "Nagelkerke")
delta_r2 <- r2_full - r2_reduced
print(paste("Model full :", round(r2_full, 4)))
print(paste("Model reduce :", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_reduce, "reduced features",delta_r2))
print(metrics_table)
summary(model_reduce)
vif(model_reduce)

# ***************** manual reduce after step *****************
current_formula <- formula(model_reduce)
new_formula <- update(current_formula, . ~ . - V11)
model_try <- glm(new_formula, data = train_capped, family = binomial(link = "logit"))
summary(model_try)
r2_reduced <- PseudoR2(model_try, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
print(paste("Model including :", round(r2_full, 4)))
print(paste("Model reduce -V11 :", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_try, "Reduce -11",delta_r2))
print(metrics_table)

new_formula_2 <- update(current_formula, . ~ . - V11 -V15)
model_try_2 <- glm(new_formula_2, data = train_capped, family = binomial(link = "logit"))
summary(model_try_2)
r2_reduced <- PseudoR2(model_try_2, which = "Nagelkerke")
r2_full
delta_r2 <- r2_full - r2_reduced
print(paste("Model including :", round(r2_full, 4)))
print(paste("Model reduce -V11 -V15:", round(r2_reduced, 4)))
print(paste("Difference:", round(delta_r2, 4)))
metrics_table <- rbind(metrics_table, get_model_metrics(model_try_2, "Reduce -11 -V15",delta_r2))
print(metrics_table)

# __________________manual add back___________________
# try to add 17 back
new_formula_17 <- update(current_formula, . ~ . + V17)
model_V17 <- glm(new_formula_17, data = train_capped, family = binomial(link = "logit"))
r2_reduced <- PseudoR2(model_V17, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
metrics_table <- rbind(metrics_table, get_model_metrics(model_V17, "step + 17",delta_r2))
print(metrics_table)
summary(model_V17)
vif(model_V17)
# VIF high

# try to add 3 back 
new_formula_3 <- update(current_formula, . ~ . + V3)
model_V3 <- glm(new_formula_3, data = train_capped, family = binomial(link = "logit"))
r2_reduced <- PseudoR2(model_V3, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
metrics_table <- rbind(metrics_table, get_model_metrics(model_V3, "step + 3",delta_r2))
print(metrics_table)
vif(model_V3)

# try to add 5 back 
new_formula_5 <- update(current_formula, . ~ . + V3)
model_V5 <- glm(new_formula_5, data = train_capped, family = binomial(link = "logit"))
r2_reduced <- PseudoR2(model_V5, which = "Nagelkerke")

delta_r2 <- r2_full - r2_reduced
metrics_table <- rbind(metrics_table, get_model_metrics(model_V5, "step + 5",delta_r2))
print(metrics_table)
vif(model_V5)

anova(model_try_2, model_reduce, test = "Chisq")
# ------> model reduce not better than model exclude 11, 15 stastical significance


# ___________________CHOSE MODEL: model_try_2 - check assumptions____________
vif(model_try_2)
plot(model_try_2, which = 4)
res <- residuals(model_try_2, type = "pearson")
# plot residence <> time
plot(res, 
     main = "Residuals vs Observation Order", 
     ylab = "Pearson Residuals", 
     pch = 20, 
     col = rgb(0,0,0,0.2)) 
abline(h = 0, col = "red", lwd = 2)

#__________________ APPLY WEIGHT______________________________________________
n_total <- nrow(train_capped)
weights <- ifelse(train_capped$Class == 1, 
                  n_total / (2 * sum(train_capped$Class == 1)), 
                  n_total / (2 * sum(train_capped$Class == 0)))
cooks_d <- cooks.distance(model_try_2)
threshold <- 4 / nrow(train_capped)
adj_factor <- ifelse(cooks_d > threshold, threshold / cooks_d, 1)
final_weights <- weights * adj_factor

model_try_2_weighted <- glm(model_try_2$formula, 
                             data = train_capped, 
                             family = "binomial", 
                             weights = final_weights)
summary(model_try_2_weighted)
vif(model_try_2_weighted)
#_______________ FIND THRESHOLD - WEIGHT____________
# 80% train 20% validation
total_n <- nrow(train_df_processed)
split_point <- floor(total_n * 0.8)

# split
train_80 <- train_df_processed[1:split_point, ]
val_20 <- train_df_processed[(split_point + 1):total_n, ]

train_80_capped <- train_80
val_20_capped <- val_20 

# capping
for(col in vars_to_cap) {
  lower_val <- quantile(train_80[[col]], 0.01, na.rm = TRUE)
  upper_val <- quantile(train_80[[col]], 0.99, na.rm = TRUE)
  
  train_80_capped[[col]] <- pmax(lower_val, pmin(train_80_capped[[col]], upper_val))
  
  val_20_capped[[col]] <- pmax(lower_val, pmin(val_20_capped[[col]], upper_val))
}

# Calculate weight
n_obs <- nrow(train_80_capped)
n_fraud <- sum(train_80_capped$Class == 1)
n_normal <- n_obs - n_fraud
weights_base <- ifelse(train_80_capped$Class == 1, n_obs/(2*n_fraud), n_obs/(2*n_normal))

model_prelim <- glm(Class ~ V4 + V6 + V8 + V9 + V10 + V12 + V13 + V14 + V18 + V20 + 
                      V21 + V22 + V26 + V27 + hour, 
                    data = train_80_capped, family = "binomial", weights = weights_base)

cooks_d <- cooks.distance(model_prelim)
threshold_cook <- 4 / n_obs
adj_factor <- ifelse(cooks_d > threshold_cook, threshold_cook / cooks_d, 1)
final_weights_80 <- weights_base * adj_factor

# train model
model_final_80 <- glm(Class ~ V4 + V6 + V8 + V9 + V10 + V12 + V13 + V14 + V18 + V20 + 
                        V21 + V22 + V26 + V27 + hour, 
                      data = train_80_capped, family = "binomial", weights = final_weights_80)
model_no_weight <- glm(Class ~ V4 + V6 + V8 + V9 + V10 + V12 + V13 + V14 + V18 + V20 + 
                         V21 + V22 + V26 + V27 + hour, 
                       data = train_80_capped, 
                       family = "binomial")

#**************** weighted ********************
# predict on 20% remaining
val_probs <- predict(model_final_80, newdata = val_20, type = "response")

# Find the best threshold on the 20%
thresholds <- seq(0.05, 0.5, by = 0.01)
val_results <- data.frame()

for(t in thresholds) {
  preds <- ifelse(val_probs > t, 1, 0)
  
  tp <- sum(preds == 1 & val_20_capped$Class == 1)
  fp <- sum(preds == 1 & val_20_capped$Class == 0)
  fn <- sum(preds == 0 & val_20_capped$Class == 1)
  
  recall <- tp / (tp + fn)
  precision <- tp / (tp + fp)
  f1 <- ifelse((precision + recall) == 0, 0, 2 * (precision * recall) / (precision + recall))
  
  val_results <- rbind(val_results, data.frame(threshold = t, f1 = f1, recall = recall, precision = precision))
}

best_t <- val_results[which.max(val_results$f1), "threshold"]
print(paste("Best Threshold:", best_t))

#***************** no weighted *****************
# predict on 20% remaining
val_probs_no_weight <- predict(model_no_weight, newdata = val_20, type = "response")
thresholds <- seq(0.001, 0.5, by = 0.005) 
val_results_no_weight <- data.frame()

for(t in thresholds) {
  preds <- ifelse(val_probs_no_weight > t, 1, 0)
  
  tp <- sum(preds == 1 & val_20_capped$Class == 1)
  fp <- sum(preds == 1 & val_20_capped$Class == 0)
  fn <- sum(preds == 0 & val_20_capped$Class == 1)
  
  recall <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
  precision <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
  f1 <- ifelse((precision + recall) == 0, 0, 2 * (precision * recall) / (precision + recall))
  
  val_results_no_weight <- rbind(val_results_no_weight, 
                                 data.frame(threshold = t, f1 = f1, recall = recall, precision = precision))
}

best_t_no_weight <- val_results_no_weight[which.max(val_results_no_weight$f1), "threshold"]
print(paste("Best Threshold (No Weight):", best_t_no_weight))
print(val_results_no_weight[which.max(val_results_no_weight$f1), ])

#_________________TEST SET WEIGHTED___________________________________
probs_weighted_cap <- predict(model_try_2_weighted, newdata = test_df_processed, type = "response")
final_labels_cap <- ifelse(probs_weighted_cap > best_t, 1, 0)
final_cm_cap <- confusionMatrix(factor(final_labels_cap, levels=c(0,1)), 
                                factor(test_df_processed$Class, levels=c(0,1)), 
                                positive = "1")
final_cm_cap
pr_weighted_cap <- pr.curve(scores.class0 = probs_weighted_cap[test_df_processed$Class == 1],
                           scores.class1 = probs_weighted_cap[test_df_processed$Class == 0], curve = TRUE)
plot(pr_weighted_cap, col = "red", main = "Precision-Recall Curve Comparison")


cm_table_df <- as.data.frame(final_cm_cap$table)
ggplot(data = cm_table_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Predicted Class", y = "Actual Class", title = "Confusion Matrix of Weighted Model at threshold = 0.49") +
  theme_minimal()
final_cm_cap$byClass

# calculate cost for each model 
# Weighted
total_fp_cost <- 238 * 16.34
fn_cases <- test_df[test_df$Class == 1 & final_labels_cap == 0, ]
total_fn_cost <- sum(fn_cases$Amount, na.rm = TRUE) 

cat("--- Cost of NO WEIGHTED Model---\n")
cat("FP Cost:", total_fp_cost, "€\n")
cat("FN Cost:", total_fn_cost, "€\n")
cat("Total:", total_fp_cost + total_fn_cost, "€\n")

# compare train and test 
# 1. Predict on train set
probs_train <- predict(model_try_2_weighted, newdata = train_df_processed, type = "response")
train_preds <- ifelse(probs_train > best_t, 1, 0)
cm_train <- confusionMatrix(factor(train_preds, levels=c(0,1)), 
                            factor(train_df_processed$Class, levels=c(0,1)), positive = "1")

# 2. get metrix 
compare_df <- data.frame(
  Metric = c("Accuracy", "Sensitivity (Recall)", "Precision", "F1-Score", "Balanced Accuracy"),
  Train = c(cm_train$overall["Accuracy"], 
            cm_train$byClass["Sensitivity"], 
            cm_train$byClass["Pos Pred Value"], 
            cm_train$byClass["F1"],
            cm_train$byClass["Balanced Accuracy"]),
  Test = c(final_cm_cap$overall["Accuracy"], 
           final_cm_cap$byClass["Sensitivity"], 
           final_cm_cap$byClass["Pos Pred Value"], 
           final_cm_cap$byClass["F1"],
           final_cm_cap$byClass["Balanced Accuracy"])
)

print(compare_df)

#___________________________ TEST SET NO WEIGHT ____________________
probs_cap <- predict(model_try_2, newdata = test_df_processed, type = "response")
final_labels <- ifelse(probs_cap > best_t_no_weight, 1, 0)
final_cm <- confusionMatrix(factor(final_labels, levels=c(0,1)), 
                                factor(test_df_processed$Class, levels=c(0,1)), 
                                positive = "1")
final_cm
pr_cap <- pr.curve(scores.class0 = probs_cap[test_df_processed$Class == 1],
                            scores.class1 = probs_cap[test_df_processed$Class == 0], curve = TRUE)
par(mfrow = c(1, 1))
plot(pr_weighted_cap, col = "red", main = "Precision-Recall Curve Comparison")
plot(pr_cap, col = "blue", add = TRUE)
legend("bottomleft", 
       legend = c("Weighted Model", "Non-Weighted Model"), 
       col = c("red", "blue"), 
       lty = 1,     
       lwd = 2,      
       bty = "n")  

cm_table_df_no <- as.data.frame(final_cm$table)
ggplot(data = cm_table_df_no, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Predicted Class", y = "Actual Class", title = "Confusion Matrix of Non-weighted Model at threshold = 0.49") +
  theme_minimal()
final_cm$byClass

# calculate cost for each model 
# Non-Weighted
total_fp_cost_no <- 233 * 16.34
fn_cases_no <- test_df[test_df$Class == 1 & final_labels == 0, ]
total_fn_cost_no <- sum(fn_cases_no$Amount, na.rm = TRUE)

cat("--- Cost of NO WEIGHTED Model---\n")
cat("FP Cost:", total_fp_cost_no, "€\n")
cat("FN Cost:", total_fn_cost_no, "€\n")
cat("Total:", total_fp_cost_no + total_fn_cost_no, "€\n")

#________________________MAXIMIZE BY COST FUNCTION__________________
cost_FP <- 16.34

thresholds <- seq(0.001, 0.9, by = 0.001)
total_costs <- numeric(length(thresholds))

for(i in seq_along(thresholds)) {
  t <- thresholds[i]
  preds <- ifelse(probs_weighted_cap > t, 1, 0)
  
  # False Positive
  n_fp <- sum(preds == 1 & test_df$Class == 0)
  cost_from_fp <- n_fp * cost_FP
  
  # False Negative
  fn_indices <- which(preds == 0 & test_df$Class == 1)
  cost_from_fn <- sum(test_df$Amount[fn_indices])
  
  total_costs[i] <- cost_from_fp + cost_from_fn
}

best_t_cost <- thresholds[which.min(total_costs)]
min_cost <- min(total_costs)

print(paste("Best Threshold for Minimal Cost:", best_t_cost))
print(paste("Minimum Total Cost on Validation Set:", min_cost))
labels_cap <- ifelse(probs_weighted_cap > best_t_cost, 1, 0)
cm_cap <- confusionMatrix(factor(labels_cap, levels=c(0,1)), 
                                factor(test_df_processed$Class, levels=c(0,1)), 
                                positive = "1")
cm_cap$byClass