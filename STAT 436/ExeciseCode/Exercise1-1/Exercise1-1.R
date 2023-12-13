data<-read.csv("bondora_train.csv",header=T)
attach(data)
library(ggplot2)

count_Default=0
count_noDefault=0
for (i in(1:dim(data)[1])){
  if(data$DefaultDate[i]=='')
    count_Default=count_Default+1
  else
    count_noDefault=count_noDefault+1
}

barplot(c(count_Default,count_noDefault),names.arg=("Default,noDefault"))

detach(data)