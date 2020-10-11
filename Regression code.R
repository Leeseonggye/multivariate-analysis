install.packages("moments")
library(moments)

# Performance evaluation function for regression --------------------------
perf_eval_reg <- function(tgt_y, pre_y){
  
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape)) #함수화 한것들
  
}

perf_mat <- matrix(0, nrow = 2, ncol = 3)

# Initialize a performance summary
rownames(perf_mat) <- c("Toyota Corolla", "Boston Housing")
colnames(perf_mat) <- c("RMSE", "MAE", "MAPE")
perf_mat

# Dataset 1: Toyota Corolla
corolla <- read.csv("C://Users//이성계//Desktop//수업//4-1//다변량//ToyotaCorolla.csv")

# Indices for the activated input variables
nCar <- nrow(corolla)
nVar <- ncol(corolla)

id_idx <- c(1,2)
category_idx <- 8

# Transform a categorical variable into a set of binary variables
dummy_p <- rep(0,nCar)
dummy_d <- rep(0,nCar)
dummy_c <- rep(0,nCar)

p_idx <- which(corolla$Fuel_Type == "Petrol")
d_idx <- which(corolla$Fuel_Type == "Diesel")
c_idx <- which(corolla$Fuel_Type == "CNG")

dummy_p[p_idx] <- 1
dummy_d[d_idx] <- 1
dummy_c[c_idx] <- 1

Fuel <- data.frame(dummy_p, dummy_d, dummy_c)
names(Fuel) <- c("Petrol","Diesel","CNG")

# Prepare the data for MLR
corolla_mlr_data <- cbind(corolla[,-c(id_idx, category_idx)], Fuel) #데이터를 옆으로 붙이는것

# Split the data into the training/validation sets
set.seed(12345) #난수생성을 할때 똑같이 나오는 세팅
corolla_trn_idx <- sample(1:nCar, round(0.7*nCar)) #round는 반올림
corolla_trn_data <- corolla_mlr_data[corolla_trn_idx,]
corolla_val_data <- corolla_mlr_data[-corolla_trn_idx,]

# Train the MLR
mlr_corolla <- lm(Price ~ ., data = corolla_trn_data) #FOrmula (Target ~ Input), lm=linear model
# (Y~ . -> 마침표는 y 빼고 모두 설명변수라는 의미)
mlr_corolla
summary(mlr_corolla)
plot(mlr_corolla)

# Plot the result
plot(corolla_trn_data$Price, fitted(mlr_corolla), # y, y hat의 관계
     xlim = c(4000,35000), ylim = c(4000,35000))
abline(0,1,lty=3)

# normality test of residuals
corolla_resid <- resid(mlr_corolla)

m <- mean(corolla_resid)
std <- sqrt(var(corolla_resid))

hist(corolla_resid, density=20, breaks=50, prob=TRUE, 
     xlab="x-variable", main="normal curve over histogram") #density: 막대의 진하기
#breaks: 막대수 

curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n") #add=true -> 원래그림에 덧붙혀라

skewness(corolla_resid) #얼마나 치우쳐져 있는가
kurtosis(corolla_resid) #얼마나 뾰족한가

# Performance Measure
mlr_corolla_haty <- predict(mlr_corolla, newdata = corolla_val_data)
#predict-> 예측하는 것, 검증용 데이터와 비교
perf_mat[1,] <- perf_eval_reg(corolla_val_data$Price, mlr_corolla_haty)
perf_mat

# Dataset 2: Boston Housing
boston_housing <- read.csv("C:/Users/이성계/Desktop/수업/4-1/다변량/BostonHousing.csv")

nHome <- nrow(boston_housing)
nVar <- ncol(boston_housing)

# Split the data into the training/validation sets
boston_trn_idx <- sample(1:nHome, round(0.7*nHome))
boston_trn_data <- boston_housing[boston_trn_idx,]
boston_val_data <- boston_housing[-boston_trn_idx,]

# Train the MLR
mlr_boston <- lm(MEDV ~ ., data = boston_trn_data)
mlr_boston

# Plot the result
plot(boston_trn_data$MEDV, fitted(mlr_boston), 
     xlim = c(-5,50), ylim = c(-5,50))
abline(0,1,lty=3)

plot(fitted(mlr_boston), resid(mlr_boston), 
     xlab="Fitted values", ylab="Residuals")

# normality test of residuals
boston_resid <- resid(mlr_boston)

m <- mean(boston_resid)
std <- sqrt(var(boston_resid))

hist(boston_resid, density=20, breaks=50, prob=TRUE, 
     xlab="x-variable", main="normal curve over histogram")

curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

skewness(boston_resid)
kurtosis(boston_resid)

# Performance Measure
mlr_boston_haty <- predict(mlr_boston, newdata = boston_val_data)

perf_mat[2,] <- perf_eval_reg(boston_val_data$MEDV, mlr_boston_haty)
perf_mat