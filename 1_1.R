install.packages("arules", dependencies = TRUE)
install.packages("arulesViz", dependencies = TRUE)
install.packages("wordcloud", dependencies = TRUE)

library(arules)
library(arulesViz)
library(wordcloud)

#1_a
iphone<-c(1,1,1,1,1,1,0,0,0,1,0)
ipad<-c(1,1,0,0,0,1,1,1,0,0,1)
Airpods<-c(0,1,1,1,0,0,0,1,1,0,0)

TransactionID<-c("재환", "다니엘", "지훈", "성우", "민현", "진영", "우진", "지성", "우진", "관린", "성운")
Transaction<-data.frame(iphone,ipad,Airpods,TransactionID)
Transaction

Transaction_ar<-Transaction[1:3]

Transaction_ar$iphone <- as.factor(Transaction_ar$iphone)
Transaction_ar$ipad <- as.factor(Transaction_ar$ipad)
Transaction_ar$Airpods <- as.factor(Transaction_ar$Airpods)
rules <- apriori(Transaction_ar)
inspect(rules)


#1_b
rules_1 <- apriori(Transaction_ar,  parameter = list(minlen=2, support = 0.01, conf = 0.01),
                   appearance = list(rhs = c("ipad=1"), lhs=c("iphone=1")))
inspect(rules_1)


rules_2<- apriori(Transaction_ar,  parameter = list(minlen=2, support = 0.01, conf = 0.01),
                  appearance = list(rhs = "iphone=1", lhs = "ipad=1"))
inspect(rules_2)


#1_c

rules_all <- apriori(Transaction_ar, parameter = list(minlen=2, support= 0.01, conf = 0.01))

inspect(rules_all)

rules_3<- apriori(Transaction_ar, parameter = list(minlen=3, support = 0.01, conf = 0.01),
                  appearance = list(lhs = c("iphone=1", "ipad=1"), rhs = c("Airpods=1")))
inspect(rules_3)


