#+++++++++++++++++++++++++++++++++++++++++++++
###Analyses of the experiment data############
#++++++++++++++++++++++++++++++++++++++++++++
library(haven)
library(tidyverse)
library(broom)
library(emmeans)
library(rstatix)
library(lmtest)
library(WRS)
data <- read_sav("Report/experiment_data.sav")
data$subject <- as.factor(data$subject)
#First let's inspect some plots:
ggplot(data, aes(avg.error.modA, avg.theta.diff, colour=subject)) +
  geom_point()

ggplot(data, aes(avg.error.Day1, avg.theta.diff, colour=subject)) +
  geom_point()

ggplot(data, aes(n.paths, avg.theta.diff, colour=subject)) +
  geom_point()

ggplot(data, aes(avg.item.weights.modA, avg.theta.diff, colour=subject)) +
  geom_point()

#Let;s look at the means of the average theta difference for the three subjects

data %>%
  group_by(subject) %>%
  select(avg.theta.diff) %>%
  summarise_all(funs(mean))


###Assumptions:

###1 Linearity between each covariate and the outcome variable at each level of the grouping variable
data$subject <- as.factor(data$subject)
# For average mistakes in module A
scatter <- ggplot(data, aes(avg.error.modA, log10(avg.theta.diff), colour=subject))  
scatter + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

# For average mistakes in day 1
scatter <- ggplot(data, aes(avg.error.Day1, avg.theta.diff, colour=subject))  
scatter + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

# For number of paths
scatter <- ggplot(data, aes(n.paths, avg.theta.diff, colour=subject))  
scatter + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)


####2 Homogeneity of variance
library(car)
leveneTest(avg.theta.diff ~ subject, data)


#### 3 Distribution of the DV

hist(data$avg.theta.diff)
hist(1/data$avg.theta.diff)

##Perform the analysis:  *Note in the analyses below I might use the terms `covariate` and `predictor` interchangeably 

#### an anova only
anova <- aov(1/avg.theta.diff ~ subject, data = data)
summary(anova)


# Now we will fit a full model with all the covariates and then reduce one by one 

# Take out  avg.item.weights.modA
ancova1 <- aov(1/avg.theta.diff ~ avg.error.modA + avg.error.Day1 + n.paths + subject, data = data)
Anova(ancova1, type = "III")

#Take out errors in Mod A
ancova2 <- aov(1/avg.theta.diff ~ avg.error.Day1  +  n.paths + subject, data = data)
Anova(ancova2, type = "III")


#==========================================================
#So we will stick with the model ancova 2 and perform the post hoc using those covariates 

post_hoc <- emmeans_test(1/avg.theta.diff ~ subject, covariate = c(avg.error.Day1, n.paths),
                         p.adjust.method = "bonferroni", data = data) #using a bonfferoni correction
post_hoc


#Lets check the same model with a regression

reg_mod <- lm(avg.theta.diff ~ avg.error.modA + n.paths + subject, data = data)
summary(reg_mod)
plot(reg_mod)

ancova(avg.theta.diff ~ avg.error.modA + n.paths + subject, data)

# Add quadratic term
ancova2 <- aov(1/avg.theta.diff ~ avg.error.Day1^2 + n.paths^2 + subject, data = data)
Anova(ancova2, type = "III")



#=======================================
#Average item weights
#======================================
#Lezen
item_weights_A <- pars_L %>%
  filter(item_id == c("BL00518", "BL00519", "BL00520", "BL00521", "BL02412",  "OP00142")) %>%
  select(item_score) 
 mean(item_weights_A$item_score) #5.33
 
 
#Rekenen
 
 item_weights_A <- pars_R %>%
   filter(item_id == c("RD516087", "RD516456", "RD516A28", "RD516663", "RD516375")) %>%
   select(item_score) 
 mean(item_weights_A$item_score)  #4
 
 #Taal
 
 item_weights_A <- pars_T %>%
   filter(item_id == c("SN00003D", "SN00038", "SW00090D", "IP00074", "GR00148")) %>%
   select(item_score) 
 mean(item_weights_A$item_score) #3.2
 

 # Clear global environment 
 rm(list=ls()) 
