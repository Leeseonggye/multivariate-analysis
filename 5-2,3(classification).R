library(rpart)
library(party)
tin<-read.csv("C:/Users/이성계/Desktop/titanic.csv")
str(tin)
tin<-tin[,-c(1)]
tin$Survived<-as.factor(tin$Survived)
tin.x <- tin[,-c(1,3,8,10)]
tin.y <- tin[c(1)]
str(tin)
trn_idx <- sample(1:dim(tin.y)[1], round(0.7*dim(tin.y)[1]))
tin.trn <- cbind(tin.x[trn_idx,], Survived= tin.y[trn_idx,])
val_idx <- sample(1:nrow(tin.trn), round(0.7*nrow(tin.trn)))
tin.val.trn <- tin.trn[val_idx,]
tin.val <- tin.trn[-val_idx,]
tin.tst <- cbind(tin.x[-trn_idx,], Survived= tin.y[-trn_idx,])
tin.all <- rbind(tin.trn, tin.tst)
#############FULL-TREE
fullTree_tin <- rpart(Survived ~., data = tin.trn, method = "class", minsplit = 0, minbucket = 1, cp=-1)
fullTree_tin
fulltreeprediction <- predict(fullTree_tin, newdata = tin.tst,type='class')

fulltree_cm <- table(tin.tst$Survived, fulltreeprediction)
fulltree_cm



full_result <- matrix(0,1,7)
colnames(full_result) <- c("TPR", "Precision", "TNR", "ACC", "BCR", "F1", "N_leaves")

plot(fullTree_tin)
plot(fullTree_tin, type="simple")
plot(fullTree_tin, margin = 0.1); text(fullTree_tin, use.n=TRUE)

# Evaluate the performance
full_result[1,1:6] = perf_eval(fulltree_cm)

# Number of leaf nodes
full_result[1,7] = 1309
full_result

###prun-tree
# construct single tree and evaluation
# tree parameter settings #제약요건걸어서 트리가 커지지 않게 하는것 
min_criterion = c(0.9, 0.95, 0.99) #통계적으로 유의미하다고 판단할때만 분기
min_split = c(10, 30, 50, 100)#영역에 최소한 이만큼 있을 때 분기
max_depth = c(0, 10, 5)#트리의 깊이
tree_result = matrix(0,length(min_criterion)*length(min_split)*length(max_depth),10)
colnames(tree_result) <- c("min_criterion", "min_split", "max_depth", 
                           "TPR", "Precision", "TNR", "ACC", "BCR", "F1", "N_leaves")

iter_cnt = 1

for (i in 1:length(min_criterion))
{
  for ( j in 1:length(min_split))
  {
    for ( k in 1:length(max_depth))
    {
      
      cat("CART Min criterion:", min_criterion[i], ", Min split:", min_split[j], ", Max depth:", max_depth[k], "\n")
      tmp_control = ctree_control(mincriterion = min_criterion[i], minsplit = min_split[j], maxdepth = max_depth[k])
      tmp_tree <- ctree(Survived ~ ., data = tin.val.trn, controls = tmp_control)
      tmp_tree_tst_prediction <- predict(tmp_tree, newdata = tin.val)
      
      tmp_tree_tst_cm <- table(tin.val$Survived, tmp_tree_tst_prediction)
      
      # parameters
      tree_result[iter_cnt,1] <- min_criterion[i]
      tree_result[iter_cnt,2] <- min_split[j]
      tree_result[iter_cnt,3] <- max_depth[k]
      
      tree_result[iter_cnt, 4:9] <- perf_eval(tmp_tree_tst_cm)
      
      # Number of leaf nodes
      tree_result[iter_cnt,10] = length(nodes(tmp_tree, unique(where(tmp_tree))))
      iter_cnt = iter_cnt + 1
    }
  }
}

# Find the best set of parameters
tree_result <- tree_result[order(tree_result[,9], decreasing = T),]
tree_result

best_criterion <- tree_result[1,1]
best_split <- tree_result[1,2]
best_depth <- tree_result[1,3]

# Construct the best tree
tree_control = ctree_control(mincriterion = best_criterion, minsplit = best_split, maxdepth = best_depth)
tree <- ctree(Survived ~ ., data = tin.trn, controls = tree_control)
tree_all_prediction <- predict(tree, newdata = tin.tst)

# Performance of the best tree
tree_all_cm <- table(tin.tst$Survived, tree_all_prediction)

# Initialize the performance matrix
best_result <- matrix(0,1,7)
colnames(best_result) <- c("TPR", "Precision", "TNR", "ACC", "BCR", "F1", "N_leaves")

# Evaluate the performance
best_result[1,1:6] = perf_eval(tree_all_cm)

# Number of leaf nodes
best_result[1,7] = length(nodes(tree, unique(where(tree))))
c(best_criterion,best_split,best_depth)
best_result
tree_all_cm
###3번내용
# Plot the best tree 
plot(tree)
plot(tree, type="simple")

#additional package
rpartmod<-rpart(Survived~. , data=tin.trn, method="class")
ptree<-prune(rpartmod, cp= rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
plotcp(rpartmod) 
plot(ptree)
text(ptree)
rpartpred<-predict(ptree, tin.tst, type='class')
tree_all_cm1 <- table(tin.tst$Survived, rpartpred)
tree_all_cm1
best_result1 <- matrix(0,1,7)
colnames(best_result1) <- c("TPR", "Precision", "TNR", "ACC", "BCR", "F1", "N_leaves")
best_result1[1,1:6] = perf_eval(tree_all_cm1)
best_result1

###egression tree
# Performance evaluation function for regression --------------------------
perf_eval_reg <- function(tgt_y, pre_y){
  #실제y ,y 
  # RMSE
  rmse <- sqrt(mean((tgt_y - pre_y)^2))
  # MAE
  mae <- mean(abs(tgt_y - pre_y))
  # MAPE
  mape <- 100*mean(abs((tgt_y - pre_y)/tgt_y))
  
  return(c(rmse, mae, mape))
  