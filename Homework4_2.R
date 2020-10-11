install.packages("glmnet")
library(glmnet)
library(GA)

#1

Data2<-KBO1
Y<-log(Data2[,1],base=10)
y<-Data2[,1]
Data_all<-cbind(Data2[,-c(1,9,10)],Y)
Data_kbo <-cbind(Data2[,-c(1)],Y)

Data_x1<-cbind(Data_kbo[,c(1)],Y)
Data_x2<-cbind(Data_kbo[,c(2)],Y)
Data_x3<-cbind(Data_kbo[,c(3)],Y)
Data_x4<-cbind(Data_kbo[,c(4)],Y)
Data_x5<-cbind(Data_kbo[,c(5)],Y)
Data_x6<-cbind(Data_kbo[,c(6)],Y)
Data_x7<-cbind(Data_kbo[,c(7)],Y)

data<-cbind(Data_kbo[,c(6)],y)
pairs(Data_all)
pairs(Data_x1)
pairs(Data_x2)
pairs(Data_x3)
pairs(Data_x4)
pairs(Data_x5)
pairs(Data_x6)
pairs(Data_x7)

pairs(data)

#2

perf_eval_reg <- function(tgt_y, pre_y){
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
}
perf_mat <- matrix(0, nrow = 6, ncol = 3)
# Initialize a performance summary
rownames(perf_mat) <- c("Forward", "Backward","Stepwise","Ridge","Lasso","Elastic Net")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat

# Split the data into the training/validation sets
set.seed(12345)
trn_idx <- sample(1:nrow(Data_all), round(0.7*nrow(Data_all)))
kbo_trn <- Data_all[trn_idx,]
kbo_tst <- Data_all[-trn_idx,]

# Shrinkage method_Ridge

kbo_trn_x <- as.matrix(kbo_trn[,-c(8)])

kbo_trn_y <- as.numeric(kbo_trn[,8])
kbo_trn_all<-as.data.frame(cbind(kbo_trn_x,kbo_trn_y))
kbo_tst_x <- as.matrix(kbo_tst[,-c(8)])
kbo_tst_y <- as.numeric(kbo_tst[,8])
kbo_tst_all<-as.data.frame(cbind(kbo_tst_x,kbo_tst_y))

Ridge_model <- glmnet(kbo_trn_x, kbo_trn_y ,alpha = 0) #alpha=0 ridge
plot(Ridge_model, xvar = "lambda")

CV_Ridge <- cv.glmnet(kbo_trn_x, kbo_trn_y, alpha = 0)
plot(CV_Ridge)
best_lambda <- CV_Ridge$lambda.min
best_lambda
Ridge_model <- glmnet(kbo_trn_x, kbo_trn_y, alpha = 0, lambda = best_lambda)


# Check the coefficients
Ridge_model_coeff <- predict(Ridge_model, s = best_lambda, newx = kbo_tst_x, type = "coefficient")
Ridge_model_coeff


# Make predictions
Ridge_model_prey <- predict(Ridge_model, s = best_lambda, newx = kbo_tst_x)

# Performance Measure
perf_mat[4,] <- perf_eval_reg(kbo_tst_y, Ridge_model_prey)
perf_mat

# Shrinkage method_Lasso

lasso_model <- glmnet(kbo_trn_x, kbo_trn_y,alpha = 1)
plot(lasso_model, xvar = "lambda")

CV_lasso <- cv.glmnet(kbo_trn_x, kbo_trn_y, alpha = 1)
plot(CV_lasso)
best_lambda <- CV_lasso$lambda.min
best_lambda
# Check the coefficients
lasso_model_coeff <- predict(lasso_model, s = best_lambda, newx = kbo_tst_x, type = "coefficient")
lasso_model_coeff

# Make predictions
lasso_model_prey <- predict(lasso_model, s = best_lambda, newx = kbo_tst_x)

# Performance Measure
perf_mat[5,] <- perf_eval_reg(kbo_tst_y, lasso_model_prey)
perf_mat

# Shrinkage method 3: Elastic net regression
Elastic_model <- glmnet(kbo_trn_x, kbo_trn_y, alpha = 0.5)
plot(Elastic_model, xvar = "lambda")

# Find the best lambda based in 5-fold cross validation
CV_Elastic <- cv.glmnet(kbo_trn_x, kbo_trn_y, alpha = 0.5)
plot(CV_Elastic)
best_lambda <- CV_Elastic$lambda.min
Elastic_model <- glmnet(kbo_trn_x, kbo_trn_y, alpha = 0.5, lambda = best_lambda)
Elastic_model$beta
# Check the coefficients
Elastic_model_coeff <- predict(Elastic_model, s = best_lambda, newx = kbo_tst_x, type = "coefficient")
Elastic_model_coeff

# Make predictions
Elastic_model_prey <- predict(Elastic_model, s = best_lambda, newx = kbo_tst_x)
# Performance Measure
perf_mat[6,] <- perf_eval_reg(kbo_tst_y, Elastic_model_prey)
perf_mat

# Variable selection method 1: Forward selection
tmp_x <- paste(colnames(Data_all)[-c(8)], collapse=" + ")
tmp_xy <- paste("Y ~ ", tmp_x, collapse = "")
as.formula(tmp_xy)

forward_model <- step(glm(Y ~ 1, data = Data_all), 
                      scope = list(upper = as.formula(tmp_xy), lower = Y ~ 1), 
                      direction="forward", trace = 1)
summary(forward_model)
forward_model_coeff <- as.matrix(forward_model$coefficients,6, 1)
forward_model_coeff
# Make predictions
forward_prey <- predict(forward_model,newdata = kbo_tst_all)
# Performance Measure
perf_mat[1,] <- perf_eval_reg(kbo_tst_y, forward_prey)
perf_mat

# Variable selection method 2: Backward elimination
full_model<-glm(Y~ .,data = Data_all)
backward_model <- step(full_model, 
                       scope = list(upper = as.formula(tmp_xy), lower = Y ~ 1),
                       direction = "backward", trace = 1)
summary(backward_model)
backward_model_coeff <- as.matrix(backward_model$coefficients, 6, 1)
backward_model_coeff
# Make predictions
backward_prey <- predict(backward_model,newdata = kbo_tst_all)
# Performance Measure
perf_mat[2,] <- perf_eval_reg(kbo_tst_y, backward_prey)
perf_mat

# Variable selection method 3: Stepwise selection
stepwise_model <- step(glm(Y ~ 1, data = Data_all), 
                       scope = list(upper = as.formula(tmp_xy), lower = Y ~ 1), 
                       direction="both", trace = 1)
summary(stepwise_model)
stepwise_model_coeff <- as.matrix(stepwise_model$coefficients, 6, 1)
stepwise_model_coeff
# Make predictions
stepwise_prey <- predict(stepwise_model,newdata = kbo_tst_all)
# Performance Measure
perf_mat[3,] <- perf_eval_reg(kbo_tst_y, stepwise_prey)
perf_mat

forward_acc<-c()
forward_time<-c()
forward_reduce<-c()
backward_acc<-c()
backward_time<-c()
backward_reduce<-c()
stepwise_acc<-c()
stepwise_time<-c()
stepwise_reduce<-c()
ridge_acc<-c()
ridge_time<-c()
ridge_reduce<-c()
lasso_acc<-c()
lasso_time<-c()
lasso_reduce<-c()
elastic_acc<-c()
elastic_time<-c()
elastic_reduce<-c()
perf_mat <- matrix(0, nrow = 6, ncol = 3)
for (i in seq(from=1,to=50)) {
  set.seed(i)
  idx<-sample(1:nrow(Data_all), round(0.7*nrow(Data_all)))
  train <- Data_all[idx,]
  test <- Data_all[-idx,]
  #################################################################################
  start_time <- proc.time()
  forward_model <- step(glm(Y ~ 1, data = train), 
                        scope = list(upper = as.formula(tmp_xy), lower = Y ~ 1), 
                        direction="forward", trace = 1)
  forward_prey <- predict(forward_model,newdata = test)
  perf_mat[1,] <- perf_eval_reg(test[,1], forward_prey)
  end_time <- proc.time()
  forward_time<-c(forward_time,(end_time - start_time)[3])
  forward_reduce<-c(forward_reduce,(ncol(train)-length(forward_model$coefficients))/7)
  
  # Variable selection method 2: Backward elimination
  start_time <- proc.time()
  full_model<-glm(Y~ .,data = train)
  backward_model <- step(full_model, 
                         scope = list(upper = as.formula(tmp_xy), lower = Y ~ 1),
                         direction = "backward", trace = 1)
  backward_prey <- predict(backward_model,newdata = test)
  perf_mat[2,] <- perf_eval_reg(test[,1], backward_prey)
  
  end_time <- proc.time()
  backward_time<-c(backward_time,(end_time - start_time)[3])
  backward_reduce<-c(backward_reduce,(ncol(train)-length(backward_model$coefficients))/7)
  
  # Variable selection method 3: Stepwise selection
  start_time <- proc.time()
  stepwise_model <- step(glm(Y ~ 1, data = train), 
                         scope = list(upper = as.formula(tmp_xy), lower = Y ~ 1), 
                         direction="both", trace = 1)
  stepwise_prey <- predict(stepwise_model,newdata = test)
  perf_mat[3,] <- perf_eval_reg(test[,1], stepwise_prey)
  end_time <- proc.time()
  stepwise_time<-c(stepwise_time,(end_time - start_time)[3])
  stepwise_reduce<-c(stepwise_reduce,(ncol(train)-length(stepwise_model$coefficients))/7)
  
  # Variable selection method 4: Ridge 
  start_time <- proc.time()
  Ridge_model <- glmnet(data.matrix(train[,-8]), data.matrix(train[,8]), alpha = 0)
  CV_Ridge <- cv.glmnet(data.matrix(train[,-8]),  data.matrix(train[,8]),  alpha = 0)
  best_lambda <- CV_Ridge$lambda.min
  Ridge_model <- glmnet(data.matrix(train[,-8]), data.matrix(train[,8]), alpha = 0, lambda = best_lambda)
  Ridge_model_prey <- predict(Ridge_model, s = best_lambda, newx = data.matrix(test[,-8]))
  perf_mat[4,] <- perf_eval_reg(test[,8], Ridge_model_prey)
  end_time <- proc.time()
  ridge_time<-c(ridge_time,(end_time - start_time)[3])
  ridge_reduce<-c(ridge_reduce,(7- Ridge_model[["df"]])/7)
  
  # Variable selection method 5: Lasso 
  start_time <- proc.time()
  Lasso_model <- glmnet(data.matrix(train[,-8]),  data.matrix(train[,8]),  alpha = 1)
  CV_Lasso <- cv.glmnet(data.matrix(train[,-8]), data.matrix(train[,8]),  alpha = 1)
  best_lambda <- CV_Lasso$lambda.min
  Lasso_model <- glmnet(data.matrix(train[,-8]), data.matrix(train[,8]), alpha = 1, lambda = best_lambda)
  Lasso_model_prey <- predict(Lasso_model, s = best_lambda, newx = data.matrix(test[,-8]))
  perf_mat[5,] <- perf_eval_reg(test[,8], Lasso_model_prey)
  end_time <- proc.time()
  lasso_time<-c(lasso_time,(end_time - start_time)[3])
  lasso_reduce<-c(lasso_reduce,(7- Lasso_model[["df"]])/7)
  
  # Variable selection method 6: Elatic Net
  start_time <- proc.time()
  Elastic_model <- glmnet(data.matrix(train[,-8]), data.matrix(train[,8]), alpha = 0.5)
  CV_Elastic <- cv.glmnet(data.matrix(train[,-8]), data.matrix(train[,8]), alpha = 0.5)
  best_lambda <- CV_Elastic$lambda.min
  Elastic_model <- glmnet(data.matrix(train[,-8]), data.matrix(train[,8]), alpha = 0.5, lambda = best_lambda)
  Elastic_model_prey <- predict(Elastic_model, s = best_lambda, newx = data.matrix(test[,-8]))
  perf_mat[6,] <- perf_eval_reg(test[,1], Elastic_model_prey)
  end_time <- proc.time()
  elastic_time<-c(elastic_time,(end_time - start_time)[3])
  elastic_reduce<-c(elastic_reduce,(7- Elastic_model[["df"]])/7)
  
  
  


# RMSE, MAE, MAPE
forward_acc<-c(forward_acc,(perf_mat[1,1]+perf_mat[1,2]+perf_mat[1,3])/3)
backward_acc<-c(backward_acc,(perf_mat[2,1]+perf_mat[2,2]+perf_mat[2,3])/3)
stepwise_acc<-c(stepwise_acc,(perf_mat[3,1]+perf_mat[3,2]+perf_mat[3,3])/3)
ridge_acc<-c(ridge_acc,(perf_mat[4,1]+perf_mat[4,2]+perf_mat[4,3])/3)
lasso_acc<-c(lasso_acc,(perf_mat[5,1]+perf_mat[5,2]+perf_mat[5,3])/3)
elastic_acc<-c(elastic_acc,(perf_mat[6,1]+perf_mat[6,2]+perf_mat[6,3])/3)
}
boxplot(forward_acc,backward_acc,stepwise_acc,ridge_acc,lasso_acc,elastic_acc,
        names = c("Forward","Backward","Stepwise","Ridge","Lasso","Elastic"),xlab="MEAN OF ERROR" )
boxplot(forward_time,backward_time,stepwise_time,ridge_time,lasso_time,elastic_time,
        names = c("Forward","Backward","Stepwise","Ridge","Lasso","Elastic"),xlab="TIME")
boxplot(forward_reduce,backward_reduce,stepwise_reduce,ridge_reduce,lasso_reduce,elastic_reduce,
        names = c("Forward","Backward","Stepwise","Ridge","Lasso","Elastic"),xlab="VARIABLE REDUCTION RATE")


result_mat <- matrix(0, nrow = 6, ncol = 3)
rownames(result_mat) <- c("Forward", "Backward","Stepwise","Ridge","Lasso","Elastic Net")
colnames(result_mat) <- c("Mean Error", "Time", "Variable Reduction Rate")
result_mat[,1]<-rank(-c(mean(forward_acc),mean(backward_acc),mean(stepwise_acc)
                       ,mean(ridge_acc),mean(lasso_acc),mean(elastic_acc)))
result_mat[,2]<-rank(-c(mean(forward_time),mean(backward_time),mean(stepwise_time),
                       mean(ridge_time),mean(lasso_time),mean(elastic_time)))
result_mat[,3]<-rank(-c(mean(forward_reduce),mean(backward_reduce),mean(stepwise_reduce)
                       ,mean(ridge_reduce),mean(lasso_reduce),mean(elastic_reduce)))
result_mat
