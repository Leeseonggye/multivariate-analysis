install.packages("arules", dependencies = TRUE)
install.packages("arulesViz", dependencies = TRUE)
install.packages("wordcloud", dependencies = TRUE)

library(arules)
library(arulesViz)
library(wordcloud)

#2-(A)
data("Groceries")
summary(Groceries)
str(Groceries)
inspect(Groceries)

Groceryrules <- apriori(Groceries, parameter = list(minlen = 3, support = 0.01, conf = 0.01))
inspect(Groceryrules)
write(Groceryrules, file="Groceryrules.csv", sep=",", quote=TRUE, row.name=FALSE)
write.table(as(rules, "data.frame"), "rules.txt", row.names = FALSE)

#2-(B)
Groceries_beer<-subset(Groceries, items %in% "bottled beer")
Groceries_beer<- apriori(Groceries_beer, parameter = list(minlen = 2, support = 0.01, conf = 0.01))
inspect(Groceries_beer)



plot(Groceries_beer, method="scatterplot")
plot(Groceries_beer, method="graph", control=list(type = "items", alpha = 1))
plot(Groceries_beer, method="grouped")
