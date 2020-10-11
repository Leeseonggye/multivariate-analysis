install.packages("arules", dependencies = TRUE) 
install.packages("arulesViz", dependencies = TRUE) 

library(arules) 
library(arulesViz)

#1.(A)
transactions<- matrix(c(1,1,0,0,0,1,1,1,0,0,1,1,1,1,1,1,1,0,0,0,1,0,0,1,1,1,0,0,0,1,1,0,0),nrow=11)
rownames(transactions) <- c("재환","다니엘","지훈","성우","민현","진영","우진","지성","우진","관린","성운")
colnames(transactions) <- c("ipad","iphone","Airpods")
transactions
                

#1.(B) c_idx <- which(titanic_ar$Age < 20) 
transactions_ar <- transactions
transactions_ar$ipad = as.character(transactions_ar$ipad)
ipad_idx<-which(as.numeric(transactions_ar$ipad > 0)
                