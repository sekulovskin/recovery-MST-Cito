#============================================================
##Line plot for classification errors vs number of mistakes
#===========================================================
library(ggplot2)
library(ggrepel)
library(directlabels)
load("classification-errors-mistakes-modA2.RData")

classification.classes.error.modA <- read.csv("errors.data.modA.csv")
classification.classes.error.modA$class <- factor(classification.classes.error.modA$class, 
                                                  levels = c("pro/bb", "bb/kb", "kb/gt", "gt/havo", "havo/vwo", "vwo"), 
                                                  labels = c("pro/bb", "bb/kb", "kb/gt", "gt/havo", "havo/vwo", "vwo"))
ggplot(classification.classes.error.modA, aes(x = mistakes, y = errors, group = class)) +  
  geom_line(aes(color=class), size = 1)+
  geom_point(aes(color=class)) +
  theme_classic() +
  ylim(0, 0.4) +
  labs(x = "Total number of mistakes in modules A",
       y = "Classification error")
  
  #  Adding the labels directly on the lines (ugly)
 # ggplot(classification.classes.error.modA, aes(x = mistakes, y = errors, group = class)) +  
 #   geom_line(aes(color=class))+
 #   geom_point(aes(color=class)) +
 #    geom_dl(aes(label = class), method = list(dl.combine("first.points", "last.points")), cex = 0.8) 
 #  labs(x = "Total number of mistakes in modules A (for all three subjects)",
 #      y = "Classification error")