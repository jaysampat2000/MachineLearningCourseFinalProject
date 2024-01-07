# Load in necessary libraries
library(ggplot2)
library(caret)
library(Metrics)
library(randomForest)
library(xgboost)
library(glmnet)
library(e1071)

load("stocks_data_obj.RData")

msft_clean <- msft_data[,-c(1, 18, 25)]
msft_clean <- na.omit(msft_clean)

# Split data into training and test sets
set.seed(111111)  # For reproducibility
total_obs <- dim(msft_clean)[1]

# Calculate the index where the dataset will be split (e.g., 80% for training)
split_index <- floor(0.8 * total_obs)
# Split the data while preserving the time order
train_data <- msft_clean[1:split_index,]
test_data <- msft_clean[(split_index + 1):total_obs,]
# Record the size of training data and test data
train_obs <- nrow(train_data)
test_obs <- nrow(test_data)

# Prepare data for XGBoost
train_data_matrix <-
  as.matrix(train_data[,-2])  # Exclude Future Return column
train_labels <- train_data$future_return
test_data_matrix <-
  as.matrix(test_data[,-2])  # Exclude Future Return column
test_labels <- test_data$future_return

# Convert data to xgb.DMatrix format
dtrain <-
  xgb.DMatrix(data = train_data_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_data_matrix, label = test_labels)

######################################################################

# Be Careful - This can take a very long time to run
max_depth_vals <-
  c(3, 5, 7, 10, 15) # Create vector of max depth values
min_child_weight <-
  c(1, 3, 5, 7, 10, 15) # Create vector of min child values

# Expand grid of parameter values
cv_params <- expand.grid(max_depth_vals, min_child_weight)
names(cv_params) <- c("max_depth", "min_child_weight")
# Create results vector
rmse_vec  <- rep(NA, nrow(cv_params))
# Loop through results
for (i in 1:nrow(cv_params)) {
  set.seed(111111)
  bst_tune <- xgb.cv(
    data = dtrain,
    # Set training data
    
    nfold = 5,
    # Use 5 fold cross-validation
    
    eta = 0.1,
    # Set learning rate
    max.depth = cv_params$max_depth[i],
    # Set max depth
    min_child_weight = cv_params$min_child_weight[i],
    # Set minimum number of samples in node to split
    
    
    nrounds = 100,
    # Set number of rounds
    early_stopping_rounds = 20,
    # Set number of rounds to stop at if there is no improvement
    
    verbose = 1,
    # 1 - Prints out fit
    nthread = 1,
    # Set number of parallel threads
    print_every_n = 20 # Prints out result every 20th iteration
    
  ) # Set evaluation metric to use
  
  rmse_vec[i] <-
    bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit]
}


# Join results in dataset
res_db <- cbind.data.frame(cv_params, rmse_vec)
names(res_db)[3] <- c("rmse")
res_db$max_depth <-
  as.factor(res_db$max_depth) # Convert tree number to factor for plotting
res_db$min_child_weight <-
  as.factor(res_db$min_child_weight) # Convert node size to factor for plotting
# Print AUC heatmap
g_2 <-
  ggplot(res_db, aes(y = max_depth, x = min_child_weight, fill = rmse)) + # set aesthetics
  geom_tile() + # Use geom_tile for heatmap
  theme_bw() + # Set theme
  scale_fill_gradient2(
    low = "blue",
    # Choose low color
    mid = "white",
    # Choose mid color
    high = "red",
    # Choose high color
    midpoint = mean(res_db$rmse),
    # Choose mid point
    space = "Lab",
    na.value = "grey",
    # Choose NA value
    guide = "colourbar",
    # Set color bar
    aesthetics = "fill"
  ) + # Select aesthetics to apply
  labs(x = "Minimum Child Weight", y = "Max Depth", fill = "RMSE") # Set labels
g_2 # Generate plot




###### 2 - Gamma Tuning ######


gamma_vals <-
  c(0, 0.05, 0.1, 0.15, 0.2) # Create vector of gamma values

# Be Careful - This can take a very long time to run
set.seed(111111)
rmse_vec  <- rep(NA, length(gamma_vals))
for (i in 1:length(gamma_vals)) {
  bst_tune <- xgb.cv(
    data = dtrain,
    # Set training data
    
    nfold = 5,
    # Use 5 fold cross-validation
    
    eta = 0.1,
    # Set learning rate
    max.depth = 7,
    # Set max depth
    min_child_weight = 10,
    # Set minimum number of samples in node to split
    gamma = gamma_vals[i],
    # Set minimum loss reduction for split
    
    
    
    nrounds = 100,
    # Set number of rounds
    early_stopping_rounds = 20,
    # Set number of rounds to stop at if there is no improvement
    
    verbose = 1,
    # 1 - Prints out fit
    nthread = 1,
    # Set number of parallel threads
    print_every_n = 20 # Prints out result every 20th iteration
  ) # Set evaluation metric to use
  
  
  rmse_vec[i] <-
    bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit]
  
  
}

# Lets view our results to identify the value of gamma to use:

# Gamma results
# Join gamma to values
gamma_vec <- cbind.data.frame(gamma_vals, rmse_vec)
min_rmse_index <- which.min(gamma_vec$rmse_vec)
gamma_vec$gamma_vals[min_rmse_index]


###### 3 - Subsample and Column sample Tuning ######

# Be Careful - This can take a very long time to run
subsample <-
  c(0.6, 0.7, 0.8, 0.9, 1) # Create vector of subsample values
colsample_by_tree <-
  c(0.6, 0.7, 0.8, 0.9, 1) # Create vector of col sample values

# Expand grid of tuning parameters
cv_params <- expand.grid(subsample, colsample_by_tree)
names(cv_params) <- c("subsample", "colsample_by_tree")
# Create vectors to store results
rmse_vec <- rep(NA, nrow(cv_params))
# Loop through parameter values
for (i in 1:nrow(cv_params)) {
  set.seed(111111)
  bst_tune <- xgb.cv(
    data = dtrain,
    # Set training data
    
    nfold = 5,
    # Use 5 fold cross-validation
    
    eta = 0.1,
    # Set learning rate
    max.depth = 7,
    # Set max depth
    min_child_weight = 10,
    # Set minimum number of samples in node to split
    gamma = 0,
    # Set minimum loss reduction for split
    subsample = cv_params$subsample[i],
    # Set proportion of training data to use in tree
    colsample_bytree = cv_params$colsample_by_tree[i],
    # Set number of variables to use in each tree
    
    nrounds = 150,
    # Set number of rounds
    early_stopping_rounds = 20,
    # Set number of rounds to stop at if there is no improvement
    
    verbose = 1,
    # 1 - Prints out fit
    nthread = 1,
    # Set number of parallel threads
    print_every_n = 20 # Prints out result every 20th iteration
  ) # Set evaluation metric to use
  
  
  rmse_vec[i] <-
    bst_tune$evaluation_log$test_rmse_mean[bst_tune$best_ntreelimit]
  
  
}







# visualise tuning sample params

res_db <- cbind.data.frame(cv_params, rmse_vec)
names(res_db)[3] <- c("rmse")
res_db$subsample <-
  as.factor(res_db$subsample) # Convert tree number to factor for plotting
res_db$colsample_by_tree <-
  as.factor(res_db$colsample_by_tree) # Convert node size to factor for plotting
g_4 <-
  ggplot(res_db, aes(y = colsample_by_tree, x = subsample, fill = rmse)) + # set aesthetics
  geom_tile() + # Use geom_tile for heatmap
  theme_bw() + # Set theme
  scale_fill_gradient2(
    low = "blue",
    # Choose low color
    mid = "white",
    # Choose mid color
    high = "red",
    # Choose high color
    midpoint = mean(res_db$rmse),
    # Choose mid point
    space = "Lab",
    na.value = "grey",
    # Choose NA value
    guide = "colourbar",
    # Set color bar
    aesthetics = "fill"
  ) + # Select aesthetics to apply
  labs(x = "Subsample", y = "Column Sample by Tree", fill = "RMSE") # Set labels
g_4 # Generate plot




###### 4 - eta tuning ######

# Use xgb.cv to run cross-validation inside xgboost
set.seed(111111)
bst_mod_1 <- xgb.cv(
  data = dtrain,
  # Set training data
  
  nfold = 5,
  # Use 5 fold cross-validation
  
  eta = 0.3,
  # Set learning rate
  max.depth = 7,
  # Set max depth
  min_child_weight = 10,
  # Set minimum number of samples in node to split
  gamma = 0,
  # Set minimum loss reduction for split
  subsample = 0.9,
  # Set proportion of training data to use in tree
  colsample_bytree =  0.9,
  # Set number of variables to use in each tree
  
  nrounds = 1000,
  # Set number of rounds
  early_stopping_rounds = 20,
  # Set number of rounds to stop at if there is no improvement
  
  verbose = 1,
  # 1 - Prints out fit
  nthread = 1,
  # Set number of parallel threads
  print_every_n = 20 # Prints out result every 20th iteration
) # Set evaluation metric to use


set.seed(111111)
bst_mod_2 <- xgb.cv(
  data = dtrain,
  # Set training data
  
  nfold = 5,
  # Use 5 fold cross-validation
  
  eta = 0.1,
  # Set learning rate
  max.depth =  7,
  # Set max depth
  min_child_weight = 10,
  # Set minimum number of samples in node to split
  gamma = 0,
  # Set minimum loss reduction for split
  subsample = 0.9 ,
  # Set proportion of training data to use in tree
  colsample_bytree = 0.9,
  # Set number of variables to use in each tree
  
  nrounds = 1000,
  # Set number of rounds
  early_stopping_rounds = 20,
  # Set number of rounds to stop at if there is no improvement
  
  verbose = 1,
  # 1 - Prints out fit
  nthread = 1,
  # Set number of parallel threads
  print_every_n = 20 # Prints out result every 20th iteration
) # Set evaluation metric to use

set.seed(111111)
bst_mod_3 <- xgb.cv(
  data = dtrain,
  # Set training data
  
  nfold = 5,
  # Use 5 fold cross-validation
  
  eta = 0.05,
  # Set learning rate
  max.depth = 7,
  # Set max depth
  min_child_weight = 10 ,
  # Set minimum number of samples in node to split
  gamma = 0,
  # Set minimum loss reduction for split
  subsample = 0.9 ,
  # Set proportion of training data to use in tree
  colsample_bytree =  0.9,
  # Set number of variables to use in each tree
  
  nrounds = 1000,
  # Set number of rounds
  early_stopping_rounds = 20,
  # Set number of rounds to stop at if there is no improvement
  
  verbose = 1,
  # 1 - Prints out fit
  nthread = 1,
  # Set number of parallel threads
  print_every_n = 20 # Prints out result every 20th iteration
) # Set evaluation metric to use


set.seed(111111)
bst_mod_4 <- xgb.cv(
  data = dtrain,
  # Set training data
  
  nfold = 5,
  # Use 5 fold cross-validation
  
  eta = 0.01,
  # Set learning rate
  max.depth = 7,
  # Set max depth
  min_child_weight = 10,
  # Set minimum number of samples in node to split
  gamma = 0.1,
  # Set minimum loss reduction for split
  subsample = 0.9 ,
  # Set proportion of training data to use in tree
  colsample_bytree = 0.9,
  # Set number of variables to use in each tree
  
  nrounds = 1000,
  # Set number of rounds
  early_stopping_rounds = 20,
  # Set number of rounds to stop at if there is no improvement
  
  verbose = 1,
  # 1 - Prints out fit
  nthread = 1,
  # Set number of parallel threads
  print_every_n = 20 # Prints out result every 20th iteration
) # Set evaluation metric to use



set.seed(111111)
bst_mod_5 <- xgb.cv(
  data = dtrain,
  # Set training data
  
  nfold = 5,
  # Use 5 fold cross-validation
  
  eta = 0.005,
  # Set learning rate
  max.depth = 7,
  # Set max depth
  min_child_weight = 10,
  # Set minimum number of samples in node to split
  gamma = 0,
  # Set minimum loss reduction for split
  subsample = 0.9 ,
  # Set proportion of training data to use in tree
  colsample_bytree = 0.9,
  # Set number of variables to use in each tree
  
  nrounds = 1000,
  # Set number of rounds
  early_stopping_rounds = 20,
  # Set number of rounds to stop at if there is no improvement
  
  verbose = 1,
  # 1 - Prints out fit
  nthread = 1,
  # Set number of parallel threads
  print_every_n = 20 # Prints out result every 20th iteration
  
) # Set evaluation metric to use



# eta plots

# Extract results for model with eta = 0.3
pd1 <-
  cbind.data.frame(bst_mod_1$evaluation_log[, c("iter", "test_rmse_mean")], rep(0.3, nrow(bst_mod_1$evaluation_log)))
names(pd1)[3] <- "eta"
# Extract results for model with eta = 0.1
pd2 <-
  cbind.data.frame(bst_mod_2$evaluation_log[, c("iter", "test_rmse_mean")], rep(0.1, nrow(bst_mod_2$evaluation_log)))
names(pd2)[3] <- "eta"
# Extract results for model with eta = 0.05
pd3 <-
  cbind.data.frame(bst_mod_3$evaluation_log[, c("iter", "test_rmse_mean")], rep(0.05, nrow(bst_mod_3$evaluation_log)))
names(pd3)[3] <- "eta"
# Extract results for model with eta = 0.01
pd4 <-
  cbind.data.frame(bst_mod_4$evaluation_log[, c("iter", "test_rmse_mean")], rep(0.01, nrow(bst_mod_4$evaluation_log)))
names(pd4)[3] <- "eta"
# Extract results for model with eta = 0.005
pd5 <-
  cbind.data.frame(bst_mod_5$evaluation_log[, c("iter", "test_rmse_mean")], rep(0.005, nrow(bst_mod_5$evaluation_log)))
names(pd5)[3] <- "eta"
# Join datasets
plot_data <- rbind.data.frame(pd1, pd2, pd3, pd4, pd5)
# Converty ETA to factor
plot_data$eta <- as.factor(plot_data$eta)
# Plot points
g_6 <-
  ggplot(plot_data, aes(x = iter, y = test_rmse_mean, color = eta)) +
  geom_point(alpha = 0.5) +
  theme_bw() + # Set theme
  theme(
    panel.grid.major = element_blank(),
    # Remove grid
    panel.grid.minor = element_blank(),
    # Remove grid
    panel.border = element_blank(),
    # Remove grid
    panel.background = element_blank()
  ) + # Remove grid
  labs(
    x = "Number of Trees",
    title = "RMSE v Number of Trees",
    y = "RMSE",
    color = "Learning \n Rate"
  )  # Set labels
g_6

# Plot lines
g_7 <-
  ggplot(plot_data, aes(x = iter, y = test_rmse_mean, color = eta)) +
  geom_smooth(alpha = 0.5) +
  theme_bw() + # Set theme
  theme(
    panel.grid.major = element_blank(),
    # Remove grid
    panel.grid.minor = element_blank(),
    # Remove grid
    panel.border = element_blank(),
    # Remove grid
    panel.background = element_blank()
  ) + # Remove grid
  labs(
    x = "Number of Trees",
    title = "RMSE v Number of Trees",
    y = "RMSE",
    color = "Learning \n Rate"
  )  # Set labels
g_7




# fit final xgb model
set.seed(111111)
bst_final <- xgboost(
  data = dtrain,
  # Set training data
  
  
  eta = 0.05,
  # maybe 0.3 as it learns quickest? or 0.1 as it is a happy compromise?
  # Set learning rate
  max.depth = 3,
  # Set max depth
  min_child_weight = 15,
  # Set minimum number of samples in node to split
  gamma = 0,
  # Set minimum loss reduction for split
  subsample = 0.6,
  # Set proportion of training data to use in tree
  colsample_bytree = 0.9,
  # Set number of variables to use in each tree
  
  nrounds = 1000,
  # Set number of rounds
  early_stopping_rounds = 100,
  # Set number of rounds to stop at if there is no improvement
  
  verbose = 1,
  # 1 - Prints out fit
  nthread = 1,
  # Set number of parallel threads
  print_every_n = 20 # Prints out result every 20th iteration
  
) # Set evaluation metric to use

##########################MB Code Ends Here################################

# Make predictions
xgb_predictions <- predict(bst_final, dtest, type = "response")

# Choose a threshold for classification, e.g., 0
xgb_threshold <- 0
xgb_predicted_class <-
  ifelse(xgb_predictions > xgb_threshold, "Positive", "Negative")
xgb_actual_class <-
  ifelse(test_labels > xgb_threshold, "Positive", "Negative")

# Create factor class for confusion matrix
xgb_predicted_class <-
  factor(xgb_predicted_class, levels = c("Negative", "Positive"))
xgb_actual_class <-
  factor(xgb_actual_class, levels = c("Negative", "Positive"))

# Create and print confusion matrix
xgb_conf_matrix <-
  confusionMatrix(xgb_predicted_class, xgb_actual_class, positive = "Positive")

print(xgb_conf_matrix)

source("C:/Users/jaysa/Downloads/a_insights_shap_functions.r")
# Calculate SHAP importance
shap_result <-
  shap.score.rank(xgb_model = bst_final,
                  X_train = train_data_matrix,
                  shap_approx = F)


shap_long = shap.prep(shap = shap_result,
                      X_train = train_data_matrix,
                      top_n = 10)

plot.shap.summary(data_long = shap_long)

# Extract importance
imp_mat <- xgb.importance(model = bst_final)

# Plot importance
xgb.plot.importance(imp_mat)

# Plot importance (top 10 variables)
xgb.plot.importance(imp_mat, top_n = 10)


rmse(test_labels, xgb_predictions)

plot_data_xgb <- cbind.data.frame(test_labels, xgb_predictions)
names(plot_data_xgb) <- c("actualxgb", "predictedxgb")

ggplot(plot_data_xgb, aes(x = actualxgb, y = predictedxgb)) +
  geom_point() +
  geom_smooth()

##############################################################################
# Random Forest


# Train the Random Forest model
rf_model <-
  randomForest(
    future_return ~ .,
    data = train_data,
    ntree = 200,
    nodesize = 1,
    mtry = 21
  )

# Make predictions on the test set
rf_predictions <-
  predict(rf_model, newdata = test_data, type = "response")

rmse(test_labels, rf_predictions)

plot_data <- cbind.data.frame(test_labels, rf_predictions)
names(plot_data) <- c("actualrf", "predictedrf")

ggplot(plot_data, aes(x = actualrf, y = predictedrf)) +
  geom_point() +
  geom_smooth()

# Choose a threshold for classification, e.g., 0
rf_threshold <- 0
rf_predicted_class <-
  ifelse(rf_predictions > rf_threshold, "Positive", "Negative")
rf_actual_class <-
  ifelse(test_labels > rf_threshold, "Positive", "Negative")

# Create factor class for confusion matrix
rf_predicted_class <-
  factor(rf_predicted_class, levels = c("Negative", "Positive"))
rf_actual_class <-
  factor(rf_actual_class, levels = c("Negative", "Positive"))

# Create and print confusion matrix
rf_conf_matrix <-
  confusionMatrix(rf_predicted_class, rf_actual_class, positive = "Positive")
print(rf_conf_matrix)

#############################################################################
# Lasso
# Standardize the features
preProcValues <-
  preProcess(train_data, method = c("center", "scale"))

train_data_lasso <- predict(preProcValues, train_data)
test_data_lasso <- predict(preProcValues, test_data)

# Extracting the predictors and response from the training data
x_train <-
  as.matrix(train_data_lasso[, -which(names(train_data_lasso) == "future_return")])
y_train <- train_data_lasso$future_return

# Fitting a Lasso model
lasso_model <- glmnet(x_train, y_train, alpha = 1)

# Optionally, use cross-validation to find the optimal lambda
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda <- cv_lasso$lambda.min

# Refit the model using the best lambda
lasso_model <-
  glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

# Making predictions on the test data
x_test <-
  as.matrix(test_data_lasso[, -which(names(test_data_lasso) == "future_return")])
predictions_lasso <-
  predict(lasso_model,
          s = best_lambda,
          newx = x_test,
          type = "response")

# Evaluate the model (e.g., using RMSE)
y_test <- test_data$future_return
rmse <- sqrt(mean((predictions_lasso - y_test) ^ 2))
print(rmse)


rmse(test_labels, predictions_lasso)

plot_data_lasso <- cbind.data.frame(test_labels, predictions_lasso)
names(plot_data_lasso) <- c("actuallasso", "predictedlasso")

ggplot(plot_data_lasso, aes(x = actuallasso, y = predictedlasso)) +
  geom_point() +
  geom_smooth()

#########################################################################
# bagging
# Train the bagging model
set.seed(111111) # Set random number generator seed for reproducability
# Use random forest to do bagging
bagging_model <- randomForest(future_return ~ .,
                              # Set tree formula
                              data = train_data,
                              # Set dataset
                              mtry = 21,
                              # Set mtry to number of variables
                              ntree = 200) # Set number of trees to use
# bagging_model # View model

# Make predictions on the test set
bag_predictions <-
  predict(bagging_model, newdata = test_data, type = "response")

# Choose a threshold for classification, e.g., 0
bag_threshold <- 0
bag_predicted_class <-
  ifelse(bag_predictions > bag_threshold, "Positive", "Negative")
bag_actual_class <-
  ifelse(test_labels > bag_threshold, "Positive", "Negative")

# Create factor class for confusion matrix
bag_predicted_class <-
  factor(bag_predicted_class, levels = c("Negative", "Positive"))
bag_actual_class <-
  factor(bag_actual_class, levels = c("Negative", "Positive"))

# Create and print confusion matrix
bag_conf_matrix <-
  confusionMatrix(bag_predicted_class, bag_actual_class, positive = "Positive")
print(bag_conf_matrix)

rmse(test_labels, bag_predictions)

plot_data_bagging <- cbind.data.frame(test_labels, bag_predictions)
names(plot_data_bagging) <- c("actualbag", "predictedbag")

ggplot(plot_data_bagging, aes(x = actualbag, y = predictedbag)) +
  geom_point() +
  geom_smooth()

#########################################################################
# SVM

# Train the SVM model
svm_model <-
  svm(train_data_matrix, train_labels, type = 'eps-regression')

# Make predictions on the test set
svm_predictions <-
  predict(svm_model, test_data_matrix, type = "response")

# Choose a threshold for classification
svm_threshold <- 0
svm_predicted_class <-
  ifelse(svm_predictions > svm_threshold, "Positive", "Negative")
svm_actual_class <-
  ifelse(test_labels > svm_threshold, "Positive", "Negative")

# Create factor class for confusion matrix
svm_predicted_class <-
  factor(svm_predicted_class, levels = c("Negative", "Positive"))
svm_actual_class <-
  factor(svm_actual_class, levels = c("Negative", "Positive"))

# Create and print confusion matrix
svm_conf_matrix <-
  confusionMatrix(svm_predicted_class, svm_actual_class, positive = "Positive")
print(svm_conf_matrix)


rmse(test_labels, svm_predictions)

plot_data_svm <- cbind.data.frame(test_labels, svm_predictions)
names(plot_data_svm) <- c("actualsvm", "predictedsvm")

ggplot(plot_data_svm, aes(x = actualsvm, y = predictedsvm)) +
  geom_point() +
  geom_smooth()

##############################################################################
# Linear regression
model <-
  lm(future_return ~ ., data = train_data)

# Summarize the model
summary(model)

# Predict on the test set
lm_predictions <-
  predict(model, newdata = test_data, type = "response")

# Evaluate the model
# Here we can use Root Mean Squared Error (RMSE) as a simple metric
rmse <- sqrt(mean((test_data$future_return - lm_predictions) ^ 2))
print(paste("Root Mean Squared Error: ", rmse))


fit_1 <- lm(future_return ~ ., data = train_data)
summary(fit_1)

lm_bwd <- step(fit_1, direction = 'backward', k = log(train_obs))
summary(lm_bwd)


rmse(test_labels, lm_predictions)

plot_data_lm <- cbind.data.frame(test_labels, lm_predictions)
names(plot_data_lm) <- c("actuallm", "predictedlm")

ggplot(plot_data_lm, aes(x = actuallm, y = predictedlm)) +
  geom_point() +
  geom_smooth()

# accuracy(actual = test_data$future_return, predicted = lm_predictions)
