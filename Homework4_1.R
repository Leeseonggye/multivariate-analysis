library(MASS) 
Data1<- data(Cars93)
install.packages("Hmisc")
library(Hmisc)
#1-1
Car<-as.data.frame(Cars93)
MPG.h<-Car[,8]
enginesize<-Car[,12]
price<-Car[,5]
weight<-Car[,25]
width<-Car[,21]
length<-Car[,19]
horsepower<-Car[,13]
wheelbase<-Car[,20]


Car_c <- data.frame(enginesize, price, weight, width,length,horsepower,wheelbase)
cor(Car_c)
Car_al<-data.frame(MPG.h,enginesize, price, weight, width,length,horsepower,wheelbase)
Car_target<-Car_al$MPG.h
cor(Car_al)
cor(Car_al)>0.8
cor(Car_al)< -0.8

#1-2

full_model <-glm(MPG.h~.,data=Car_al)
full_model
summary(full_model)


  #1-3
  
  # Variable selection method 1: Forward selection
tmp_x <- paste(colnames(Car_al)[-1], collapse=" + ")
tmp_xy <- paste("MPG.h ~ ", tmp_x, collapse = "")
as.formula(tmp_xy)

forward_model <- step(glm(MPG.h ~ 1, data = Car_al), #상수항만 있는 항을 만들어라 
                      scope = list(upper = as.formula(tmp_xy), lower = MPG.h ~ 1),#탐색하고 싶은 범위가 어디 
                      direction="forward", trace = 1) #stepwise-> direction=both
summary(forward_model)
forward_model_coeff <- as.matrix(forward_model$coefficients, 7, 1)
forward_model_coeff



# Variable selection method 2: Backward elimination
backward_model <- step(full_model, 
                       scope = list(upper = as.formula(tmp_xy), lower = MPG.h ~ 1),
                       direction = "backward", trace = 1)
summary(backward_model)
backward_model_coeff <- as.matrix(backward_model$coefficients, 7, 1)
backward_model_coeff


# Variable selection method 3: Stepwise selection
tmp_x <- paste(colnames(Car_al)[-1], collapse=" + ")
tmp_xy <- paste("MPG.h ~ ", tmp_x, collapse = "")
as.formula(tmp_xy)

stepwise_model <- step(glm(MPG.h ~ 1, data = Car_al), 
                       scope = list(upper = as.formula(tmp_xy), lower = Car_target ~ 1), 
                       direction="both", trace = 1)
summary(stepwise_model)
stepwise_model_coeff <- as.matrix(stepwise_model$coefficients, 8, 1)
stepwise_model_coeff






