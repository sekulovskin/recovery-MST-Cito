#+++++++++++++++++++++++++++++++++++++++++++++
###Analyses of the experiment data############
#++++++++++++++++++++++++++++++++++++++++++++
library(haven)
library(tidyverse)
library(broom)
library(emmeans)
library(rstatix)
library(lmtest)
data <- read_sav("experiment_data.sav")
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
scatter <- ggplot(data, aes(avg.error.modA, avg.theta.diff, colour=subject))  
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

##Perform the analysis:  *Note in the analyses below I might use the terms `covariate` and `predictor` interchangeably 

#### an anova only
anova <- aov(avg.theta.diff ~ subject, data = data)
summary(anova)


# Now we will fit a full model with all the covariates and then reduce one by one 

ancova1 <- aov(avg.theta.diff ~ avg.error.modA + avg.error.Day1 + n.paths + avg.item.weights.modA + subject, data = data)
Anova(ancova1, type = "III")  #This model cannot be estimated

# Take out  avg.item.weights.modA
ancova2 <- aov(avg.theta.diff ~ avg.error.modA + avg.error.Day1 + n.paths + subject, data = data)
Anova(ancova2, type = "III")

#Take out errors in Mod A
ancova3 <- aov(avg.theta.diff ~ avg.error.Day1  +  n.paths + subject, data = data)
Anova(ancova3, type = "III")

##Test the models with the LR test
lrtest(ancova3, ancova2) #Since we fail to reject H0. we should keep the nested model (i.e, ancova 3)


#==========================================================
#So we will stick with the model ancova 2 and perform the post hoc using those covariates 

post_hoc <- emmeans_test(avg.theta.diff ~ subject, covariate = c(avg.error.Day1, n.paths),
                         p.adjust.method = "bonferroni", data = data) #using a bonfferoni correction
post_hoc

# Get estimated means

get_emmeans(post_hoc)

#Lets check the same model with a regression

reg_mod <- lm(avg.theta.diff ~ avg.error.modA + n.paths + subject, data = data)
summary(reg_mod)
plot(reg_mod)

# Add quadratic term
ancova2 <- aov(avg.theta.diff ~ avg.error.Day1^2 + n.paths + subject, data = data)
Anova(ancova2, type = "III")

