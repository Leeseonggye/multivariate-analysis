library(party)
###1 information gain
play<-read.csv("C:/Users/ÀÌ¼º°è/Desktop/Assignment 5/Playdata.csv")
str(play)
gain_Table <-  matrix(0, nrow = 10, ncol = 2)
rownames(gain_Table) <- c("rain","overcast","sunny","cool","mind","hot","highhumid","normalhumid","wind","nowind")
colnames(gain_Table) <-  c("gini","entropy")
gain_Table

#Gini-index
gini_process <-function(classes,splitvar = NULL){
  #Assumes Splitvar is a logical vector
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(1-sum(base_prob**2))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Node_Gini <- 1-sum(crossprob[,1]**2)
  Yes_Node_Gini <- 1-sum(crossprob[,2]**2)
  return(sum(base_prob * c(No_Node_Gini,Yes_Node_Gini)))
}
gini_total<-gini_process(play$Play) 
gini_rain<-gini_process(play$Play,play$Outlook=="rain") 
gini_overcast<-gini_process(play$Play,play$Outlook=="overcast") 
gini_sunny<-gini_process(play$Play,play$Outlook=="sunny")

gain_Table[1,1]<-gini_total - gini_rain
gain_Table[2,1]<-gini_total - gini_overcast
gain_Table[3,1]<-gini_total - gini_sunny

a<-gini_process(play$Play,play$Temperature=="cool") 
b<-gini_process(play$Play,play$Temperature=="mild")
c<-gini_process(play$Play,play$Temperature=="hot")

gain_Table[4,1]<-gini_total - a
gain_Table[5,1]<-gini_total - b
gain_Table[6,1]<-gini_total - c

d<-gini_process(play$Play,play$Humidity=="high") 
e<-gini_process(play$Play,play$Humidity=="normal")

gain_Table[7,1]<-gini_total - d
gain_Table[8,1]<-gini_total - e

f<-gini_process(play$Play,play$Wind=="true")
g<-gini_process(play$Play,play$Wind=="false")

gain_Table[9,1]<-gini_total - f
gain_Table[10,1]<-gini_total - g

#Entropy
entropy_process <-function(classes,splitvar = NULL){
  #Assumes Splitvar is a logical vector
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(-sum(base_prob*log(base_prob,2)))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Col <- crossprob[crossprob[,1]>0,1]
  Yes_Col <- crossprob[crossprob[,2]>0,2]
  No_Node_Info <- -sum(No_Col*log(No_Col,2))
  Yes_Node_Info <- -sum(Yes_Col*log(Yes_Col,2))
  return(sum(base_prob * c(No_Node_Info,Yes_Node_Info)))
}

entropy_total<-entropy_process(play$Play) 
aa<-entropy_process(play$Play,play$Outlook=="rain") 
bb<-entropy_process(play$Play,play$Outlook=="overcast") 
cc<-entropy_process(play$Play,play$Outlook=="sunny")

gain_Table[1,2]<-entropy_total - aa
gain_Table[2,2]<-entropy_total - bb
gain_Table[3,2]<-entropy_total - cc

dd<-entropy_process(play$Play,play$Temperature=="cool") 
ee<-entropy_process(play$Play,play$Temperature=="mild")
ff<-entropy_process(play$Play,play$Temperature=="hot")

gain_Table[4,2]<-entropy_total - dd
gain_Table[5,2]<-entropy_total - ee
gain_Table[6,2]<-entropy_total - ff

gg<-entropy_process(play$Play,play$Humidity=="high") 
hh<-entropy_process(play$Play,play$Humidity=="normal")

gain_Table[7,2]<-entropy_total - gg
gain_Table[8,2]<-entropy_total - hh

ii<-entropy_process(play$Play,play$Wind=="true")
jj<-entropy_process(play$Play,play$Wind=="false")

gain_Table[9,2]<-entropy_total - ii
gain_Table[10,2]<-entropy_total - jj
gain_Table

