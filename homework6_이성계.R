install.packages("neuralnet")
library(neuralnet)
?neuralnet
#Problem 1 (1) DATA shuffle # sample
set.seed(1234)
uiris<- iris[sample(nrow(iris), nrow(iris)),]
head(uiris)
#Problem 1 (2) one-hot-vector
levels(uiris$Species) <- c(levels(uiris$Species),"fake")
uiris$Species <- relevel(uiris$Species,ref = "fake")
iris_multinom <- model.matrix(~Species+Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=uiris)[,-1]
colnames(iris_multinom)[1:3] <- c("setosa","versicolor","virginica")
head(iris_multinom)
#Problem 1 (3) 
trn_idx <- sample(1:dim(iris)[1], round(0.6*dim(iris)[1]), replace = FALSE)
iris_trn <- iris_multinom[trn_idx,]
val_tst <- iris_multinom[-trn_idx,]
val_idx <- sample(1:nrow(val_tst), round(0.5*nrow(val_tst)), replace = FALSE)
iris_val <- val_tst[val_idx,]                  
iris_tst <- val_tst[-val_idx,]
#Problem 2 (1)
paste_iris <- paste(colnames(uiris)[1:4], collapse = "+")
tmp_iris <- paste("setosa+versicolor+virginica ~", paste_iris, collapse = "")
iris_formula <- as.formula(tmp_iris)
iris_formula
#Problem 2 (2) neuralnet
nn <- neuralnet(formula = iris_formula, data = iris_trn, hidden = 5,
                act.fct = 'logistic', algorithm = 'backprop', 
                learningrate = 0.01, stepmax = 100000, err.fct = 'ce', linear.output = FALSE)
#Problem 2 (3) 
cat('cross-entropy:', nn$result.matrix[1],'/n')
nn_res <- nn$net.result[[1]]
nn_res
head(nn_res)
min(apply(nn_res,1,max))
iris_label <- max.col(nn_res)
iris_label
#Problem 2 (4) 
plot(nn)
#Problem 3 (1) 
nn$generalized.weights

#Problem 3 (2) 
tmp_res <- compute(nn, iris_tst[,-c(1:3)])
tmp_nn_res <- tmp_res$net.result
min(apply(tmp_nn_res,1,max))
max.col(tmp_nn_res)
par( mfrow =c(4 ,3))
for (i in 1:4){
  for (j in 1:3){
    gwplot(nn,selected.covariate = colnames(iris_trn)[i+3],
           selected.response = colnames(iris_trn)[j],
           min = -0.0001 , max =0.0001)
  }
}
#Problem 4 
iris_class <- max.col(iris_trn[,c(1:3)])
iris_cm <- table(iris_class, iris_label)
rownames(iris_cm) <- c("setosa","versicolor","virginica")
colnames(iris_cm) <- c("setosa","versicolor","virginica")
iris_cm


micro_F1 <- function(cm){
  TP1 = cm [1,1]
  TP2 = cm [2,2]
  TP3 = cm [3,3]
  FP1 = sum(cm [2 ,1], cm [3 ,1])
  FP2 = sum(cm [1 ,2], cm [3 ,2])
  FP3 = sum(cm [1 ,3], cm [2 ,3])
  FN1 = sum(cm [1 ,2], cm [1 ,3])
  FN2 = sum(cm [2 ,1], cm [2 ,3])
  FN3 = sum(cm [3 ,1], cm [3 ,2])
  micro_recall =(TP1+TP2+TP3)/(TP1+TP2+TP3+FN1+FN2+FN3)
  micro_precision = (TP1+TP2+TP3)/(TP1+TP2+TP3+FP1+FP2+FP3)
  micro_F1_measure = 2*micro_precision*micro_recall/(micro_precision+micro_recall)
  return (c(micro_recall,micro_precision ,micro_F1_measure))
}

micro_F1(iris_cm)
#Problem 5(1)
howmany <- list(c(8,16,32), list(c(8,8), c(16,16), c(32,32)), list(c(8,8,8), c(16,16,16), c(32,32,32)))
perf_table <- matrix(0, nrow = 9, ncol = 3)
rownames(perf_table) <- c(1:9)
colnames(perf_table) <- c("hidden layer", "hidden node", "F1")
k<-0
for(i in 1:3){
  for(j in 1:3){
    tmp_nn <- neuralnet(formula = iris_formula, data = iris_trn, hidden = howmany[[i]][[j]], act.fct = 'logistic', 
                        algorithm = 'backprop', learningrate = 0.01, stepmax = 1e6, err.fct = 'ce', linear.output = FALSE)
    tmp_com <- compute(tmp_nn, iris_val[,-c(1:3)])
    tmp_res <- tmp_com$net.result
    tmp_val_class <- max.col(iris_val[,c(1:3)])
    tmp_label <- max.col(tmp_res)
    tmp_cm <- table(tmp_val_class, tmp_label)
    perf_table[k+j,3] <- micro_F1(tmp_cm)[3]
    perf_table[k+j,2] <- howmany[[1]][j]
    perf_table[k+j,1] <- i
  }
  k <- k+3
}
perf_table
#Problem 5(2)
opt_nn <- neuralnet(formula = iris_formula, data = iris_trn, hidden = 8, act.fct = 'logistic', algorithm = 'backprop',
                    learningrate = 0.01,
                    stepmax = 1e6, err.fct = 'ce', linear.output = FALSE)
tst_com <- compute(opt_nn, iris_tst[,-c(1:3)])
tst_res <- tst_com$net.result
tst_cm <- table(max.col(iris_tst[,c(1:3)]), max.col(tst_res))
rownames(tst_cm) <- c("setosa","versicolor","virginica")
colnames(tst_cm) <- c("setosa","versicolor","virginica")
tst_cm

micro_F1(tst_cm)
plot(opt_nn)
