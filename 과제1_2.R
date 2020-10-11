
library(clValid)
library(plotrix)
library(arules)
library(arulesViz)
library(wordcloud)
#2.(A)
data("Groceries")
summary(Groceries)
str(Groceries)
inspect(Groceries)
apriori
rules <- apriori(Groceries, parameter=list(minlen=3,support=0.01, confidence=0.01))
rules
inspect(rules)
write.table(as(rules, "data.frame"), "rules.txt", row.names = FALSE)
#2.(B)
rules <- apriori(Groceries, parameter=list(minlen=2,support=0.005, confidence=0.005))
rules
rule_bottledbeer <- subset(rules, items %in% "bottled beer")
rule_bottledbeer
inspect(rule_bottledbeer)
plot(rule_bottledbeer)
plot(rule_bottledbeer, method="grouped")