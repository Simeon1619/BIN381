library(caret)        
library(randomForest) 
library(dplyr)        
library(ggplot2)      
library(car)
data <- read.csv("health_indicators.csv")
data_processed <- prepare_features(data)
data_clean <- na.omit(data_processed)
set.seed(123)  
train_index <- createDataPartition(data_clean$Value, 
                                   p = 0.70, 
                                   list = FALSE)
train_data <- data_clean[train_index, ]
temp_data <- data_clean[-train_index, ]
validation_index <- createDataPartition(temp_data$Value, 
                                        p = 0.50, 
                                        list = FALSE)

validation_data <- temp_data[validation_index, ]
test_data <- temp_data[-validation_index, ]
cat("Training set size:", nrow(train_data), "\n")
cat("Validation set size:", nrow(validation_data), "\n")
cat("Test set size:", nrow(test_data), "\n")
mlr_formula <- Value ~ value_z + value_lag1 + 
  facility_public + facility_private +
  provider_nurse + provider_midwife +
  antenatal_visits + survey_year
mlr_model <- lm(mlr_formula, data = train_data)
summary(mlr_model)
par(mfrow = c(2, 2))
plot(mlr_model)
vif_values <- vif(mlr_model)
print(vif_values)
library(lmtest)
bp_test <- bptest(mlr_model)
print(bp_test)
dw_test <- dwtest(mlr_model)
print(dw_test)
mlr_train_pred <- predict(mlr_model, train_data)
mlr_val_pred <- predict(mlr_model, validation_data)
mlr_test_pred <- predict(mlr_model, test_data)
mlr_train_rmse <- sqrt(mean((train_data$Value - mlr_train_pred)^2))
mlr_val_rmse <- sqrt(mean((validation_data$Value - mlr_val_pred)^2))
mlr_test_rmse <- sqrt(mean((test_data$Value - mlr_test_pred)^2))

mlr_train_mae <- mean(abs(train_data$Value - mlr_train_pred))
mlr_val_mae <- mean(abs(validation_data$Value - mlr_val_pred))
mlr_test_mae <- mean(abs(test_data$Value - mlr_test_pred))

mlr_train_r2 <- summary(mlr_model)$r.squared
mlr_train_adj_r2 <- summary(mlr_model)$adj.r.squared
train_control <- trainControl(method = "cv", 
                              number = 10,
                              savePredictions = TRUE)
mlr_cv <- train(mlr_formula, 
                data = train_data,
                method = "lm",
                trControl = train_control)
print(mlr_cv)
mlr_cv_rmse <- mlr_cv$results$RMSE
mlr_cv_r2 <- mlr_cv$results$Rsquared
predictors <- c("value_z", "value_lag1", 
                "facility_public", "facility_private",
                "provider_nurse", "provider_midwife",
                "antenatal_visits", "survey_year")
rf_initial <- randomForest(
  x = train_data[, predictors],
  y = train_data$Value,
  ntree = 1000,
  importance = TRUE
)
print(rf_initial)
mtry_values <- c(2, 3, 4, 5, 6, 8)
results <- data.frame(mtry = integer(), 
                      rmse = numeric(), 
                      nodesize = integer())
nodesize_values <- c(1, 5, 10, 20)

for (ns in nodesize_values) {
  for (m in mtry_values) {
    rf_temp <- randomForest(
      x = train_data[, predictors],
      y = train_data$Value,
      ntree = 1000,
      mtry = m,
      nodesize = ns,
      importance = TRUE
    )
    
    val_pred <- predict(rf_temp, validation_data[, predictors])
    val_rmse <- sqrt(mean((validation_data$Value - val_pred)^2))
    
    results <- rbind(results, 
                     data.frame(mtry = m, 
                                rmse = val_rmse, 
                                nodesize = ns))
  }
}
optimal_params <- results[which.min(results$rmse), ]
print(optimal_params)
rf_final <- randomForest(
  x = train_data[, predictors],
  y = train_data$Value,
  ntree = 1000,
  mtry = optimal_params$mtry,
  nodesize = optimal_params$nodesize,
  importance = TRUE
)
importance_df <- as.data.frame(importance(rf_final))
importance_df$variable <- rownames(importance_df)
importance_df <- importance_df[order(-importance_df$`%IncMSE`), ]
print(importance_df)
varImpPlot(rf_final, main = "Random Forest Variable Importance")
rf_train_pred <- predict(rf_final, train_data[, predictors])
rf_val_pred <- predict(rf_final, validation_data[, predictors])
rf_test_pred <- predict(rf_final, test_data[, predictors])
rf_train_rmse <- sqrt(mean((train_data$Value - rf_train_pred)^2))
rf_val_rmse <- sqrt(mean((validation_data$Value - rf_val_pred)^2))
rf_test_rmse <- sqrt(mean((test_data$Value - rf_test_pred)^2))
rf_train_mae <- mean(abs(train_data$Value - rf_train_pred))
rf_val_mae <- mean(abs(validation_data$Value - rf_val_pred))
rf_test_mae <- mean(abs(test_data$Value - rf_test_pred))
rf_test_ss_res <- sum((test_data$Value - rf_test_pred)^2)
rf_test_ss_tot <- sum((test_data$Value - mean(test_data$Value))^2)
rf_test_r2 <- 1 - (rf_test_ss_res / rf_test_ss_tot)
rf_grid <- expand.grid(mtry = optimal_params$mtry)
rf_cv <- train(
  x = train_data[, predictors],
  y = train_data$Value,
  method = "rf",
  trControl = train_control,
  tuneGrid = rf_grid,
  ntree = 1000,
  nodesize = optimal_params$nodesize
)
print(rf_cv)
rf_cv_rmse <- rf_cv$results$RMSE
rf_cv_r2 <- rf_cv$results$Rsquared
comparison <- data.frame(
  Model = c("MLR", "RF"),
  Train_RMSE = c(mlr_train_rmse, rf_train_rmse),
  Val_RMSE = c(mlr_val_rmse, rf_val_rmse),
  Test_RMSE = c(mlr_test_rmse, rf_test_rmse),
  CV_RMSE = c(mlr_cv_rmse, rf_cv_rmse),
  Test_MAE = c(mlr_test_mae, rf_test_mae),
  Test_R2 = c(mlr_train_r2, rf_test_r2)
)
print(comparison)
library(reshape2)
comparison_long <- melt(comparison, id.vars = "Model")
ggplot(comparison_long %>% filter(grepl("RMSE", variable)), 
       aes(x = variable, y = value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "RMSE Comparison Across Datasets",
       x = "Dataset", y = "RMSE") +
  theme_minimal()
mlr_residuals <- test_data$Value - mlr_test_pred
rf_residuals <- test_data$Value - rf_test_pred
par(mfrow = c(2, 2))
plot(mlr_test_pred, mlr_residuals,
     main = "MLR: Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)
plot(rf_test_pred, rf_residuals,
     main = "RF: Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)
qqnorm(mlr_residuals, main = "MLR: Q-Q Plot")
qqline(mlr_residuals, col = "red")
qqnorm(rf_residuals, main = "RF: Q-Q Plot")
qqline(rf_residuals, col = "red")
saveRDS(mlr_model, "mlr_final_model.rds")
saveRDS(rf_final, "rf_final_model.rds")
write.csv(comparison, "model_comparison_results.csv", row.names = FALSE)
write.csv(importance_df, "rf_variable_importance.csv", row.names = FALSE)

print("Pipeline complete!")