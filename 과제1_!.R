
install.packages("arules", dependencies = TRUE)
install.packages("arulesViz", dependencies = TRUE)
install.packages("wordcloud", dependencies = TRUE)

library(arules)
library(arulesViz)
library(wordcloud)
#1.(a)
Airpods<-c(0,1,1,1,0,0,0,1,1,0,0)
iPad<-c(1,1,0,0,0,1,1,1,0,0,1)
iPhone<-c(1,1,1,1,1,1,0,0,0,1,0)
TransactionID<-c("재환", "다니엘", "지훈", "성우", "민현", "진영", "우진", "지성", "우진", "관린", "성운")
Transaction<-data.frame(Airpods,iPad,iPhone,TransactionID)
Transaction

#1.(b)



Transaction_ar<-Transaction[1:3]

Transaction_ar$Airpods <- as.factor(Transaction_ar$Airpods)
Transaction_ar$iPad <- as.factor(Transaction_ar$iPad)
Transaction_ar$iPhone <- as.factor(Transaction_ar$iPhone)

rules <- apriori(Transaction_ar, parameter = list(minlen= 2 ), support = 0.01, conf = 0.01)
      appearance = list(rhs= c("iPad=1","iPad=1"), default="rhs") 
inspect(rules)

rules_ed<- apriori(Transaction_ar, appearance = list(rhs = c("iPhone=1"), default="lhs"))
inspect(rules_ed)

rules_de <- apriori(Transaction_ar, appearance = list(rhs = c("iPad=1"), lhs=c("iPhone=1"),default="lhs"))
inspect(rules_de)

#1.(c)

rules <- apriori(Transaction_ar)
rules <- apriori(Transaction_ar, parameter = list(minlen = 6, support = 0, conf = 0)
                 
inspect(rules) 
                 
                 