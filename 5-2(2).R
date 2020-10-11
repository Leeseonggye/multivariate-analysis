library(party)
library(rpart)
# Performance evaluation function for regression --------------------------

perf_eval_reg <- function(tgt_y, pre_y){
  #½ÇÁ¦y ,yÇÞ
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  
}

perf_mat <- matrix(0, nrow = 2, ncol = 3)
#data
head(iris)
str(iris)
data<- iris

str(data)
trn_idx <- sample(1:dim(data)[1], round(0.7*dim(data)[1]))
data.trn <- data[trn_idx,]
val_idx <- sample(1:nrow(data.trn), round(0.7*nrow(data.trn)))
data.val.trn <- data.trn[val_idx,]
data.val <- data.trn[-val_idx,]
data.tst <- data[-trn_idx,]
data.all <- rbind(data.trn, data.tst)

#full_tree
fullTree_reg <- rpart(Sepal.Length ~., data = data.trn, method = "poisson", control = rpart.control( minsplit = 0 ,minbucket = 1, cp=-1))
fullTree_reg
plot(fullTree_reg)
fullreg <- predict(fullTree_reg, newdata = data.tst)

perf_mat[1,] <- perf_eval_reg(data.tst$Sepal.Length, fullreg)
perf_mat[1,]

###purend tree
prun_reg<-rpart(formula = Sepal.Length ~ ., data = data.val.trn, method = "poisson")
plotcp(prun_reg) 
prun_reg$cptable[which.min(prun_reg$cptable[,"xerror"]),"CP"]
prune_reg_tree<-rpart(formula = Sepal.Length ~ ., data = data.trn, method = "poisson", cp=0.01)
plot(prune_reg_tree, margin = 0.1); text(prune_reg_tree, use.n=TRUE) 
#3
prunedreg<-predict(prune_reg_tree, newdata = data.tst)
perf_mat[2,] <- perf_eval_reg(data.tst$Sepal.Length, prunedreg)
perf_mat[2,]
perf_mat
