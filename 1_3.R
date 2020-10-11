library(wordcloud)

#Top 25까지 포함
stock_data <- read.csv("C:/Users/이성계/Desktop/수업/4-1/다변량/Assignment 1/modified_STOCK_TITLE.csv", stringsAsFactors = FALSE)

#Headline -> list
headline2list <- function(x){
  result_list <- list()
  for(i in 1:nrow(x)){
    for(j in 3:ncol(x)){
      result_list<-c(result_list,strsplit(as.character(x[i,j]),split=" "))
      print(i)
    }
  }
  return(result_list)
}

#Up down
headline2list_updown <- function(x){
  result_list <- list()
  for(i in 1:nrow(x)){
    for(j in 3:ncol(x)){
      result_list<-c(result_list,strsplit(as.character(paste(x[i,j],x[i,2])),split=" "))
      print(i)
    }
  }
  return(result_list)
}




total_data <- headline2list_updown(stock_data)
total_trans<-as(total_data,"transactions")
str(total_trans)
total_rules <- apriori(total_trans, parameter = list(support = 1/500, confidence = 0.01 , minlen = 3),
                       appearance = list(rhs = c("up", "down"), default="lhs"))
total_rules_up <- apriori(total_trans, parameter = list(support = 1/500, confidence = 0.01 , minlen = 3),
                          appearance = list(rhs = c("up"), default="lhs"))

total_rules_down <- apriori(total_trans, parameter = list(support = 1/500, confidence = 0.01 , minlen = 3),
                          appearance = list(rhs = c("down"), default="lhs"))


inspect(total_rules)
inspect(head(sort(total_rules, by="confidence"),20))
inspect(total_rules_up)
inspect(head(sort(total_rules_up, by="confidence"),10))
inspect(total_rules_down)
inspect(head(sort(total_rules_down, by="confidence"),10))




#Word Cloud
for(i in unique(format(as.Date(stock_data$Date), "%Y"))){
  tmp_data <- subset(stock_data, format(as.Date(stock_data$Date),"%Y")==i)
  tmp_headlinelist <- headline2list(tmp_data)
  tmp_trans<-as(tmp_headlinelist,"transactions")
  
  title_year <-paste("Year:",i , sep=" ")
  
  itemName <- itemLabels(tmp_trans)
  itemCount <- itemFrequency(tmp_trans)*nrow(tmp_trans)
  
  col <- brewer.pal(8, "Set2")
  wordcloud(words = itemName, freq = itemCount, min.freq = 70, scale = c(2, 0.2), col = col , random.order = FALSE)
  title(title_year, line = -2)
  
  
  
}

