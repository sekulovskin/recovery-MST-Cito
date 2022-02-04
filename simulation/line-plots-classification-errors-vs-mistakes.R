#============================================================
##Line plots for classification errors vs number of mistakes
#===========================================================
library(ggplot2)
library(ggrepel)
library(directlabels)
load("classification-errors-mistakes-modA2.RData")
#read.csv("errors.data.modA.csv")

classification.classes.error.modA$class <- as.factor(classification.classes.error.modA$class)

ggplot(classification.classes.error.modA, aes(x = mistakes, y = errors, group = class)) +  
  geom_line(aes(color=class), size = 1)+
  geom_point(aes(color=class)) +
  ylim(0, 0.6) +
  labs(x = "Total number of mistakes in modules A (for all three subjects)",
       y = "Classification error", title = "Classification errors vs (total) number of mistakes in Module A")
  
  
  
  #  Adding the labels directly on the lines (ugly)
 # ggplot(classification.classes.error.modA, aes(x = mistakes, y = errors, group = class)) +  
 #   geom_line(aes(color=class))+
 #   geom_point(aes(color=class)) +
 #    geom_dl(aes(label = class), method = list(dl.combine("first.points", "last.points")), cex = 0.8) 
 #  labs(x = "Total number of mistakes in modules A (for all three subjects)",
 #      y = "Classification error")