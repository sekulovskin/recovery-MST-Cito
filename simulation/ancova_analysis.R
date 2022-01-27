###Analysis of the experiment data############

#ANCOVA
#CAUTION: WE TAKE THE P-VALUES `WITH A GRAIN OF SALT` SINCE MANY OF THE ASSUMPTIONS ARE VIOLATED AND WE ARE NOT DEALING WITH A LINEAR RELATIONSHIP
#OTHER TECHNIQUES WILL BE CONSIDERED (SUCH AS FITTING SOME TYPE OF A GLM) AND/OR USING THE Johnson-Neyman TECHNIQUE
#https://kenstoyama.wordpress.com/2018/01/21/the-johnson-neyman-tecnique-and-an-r-script-to-apply-it/
library(haven)
library(tidyverse)
library(broom)
library(emmeans)
library(rstatix)
data <- read_sav("experiment_data.sav")

#Let;s look at the mean differences 
data %>%
  group_by(subject) %>%
  select(avg.theta.diff) %>%
  summarise_all(funs(mean))

# Lezen       0.109
# Rekenen     0.127
# Taal        0.127

data$subject <- as.factor(data$subject)

###Assumptions:

###1 Linearity between each covariate and the outcome variable at each level of the grouping variable

# For average mistakes in module A
scatter <- ggplot(data, aes(avg.error.modA, avg.theta.diff, colour=subject))  
scatter + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

# For average mistakes in day 1
scatter <- ggplot(data, aes(avg.error.Day1, avg.theta.diff, colour=subject))  
scatter + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

# For number of paths
scatter <- ggplot(data, aes(n.paths, avg.theta.diff, colour=subject))  
scatter + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Even though there are violations, since the nature of the DV is such that we are interested in conventional linearity
# we will skip it


####2 Homogeneity of variance
library(car)
leveneTest(avg.theta.diff ~ subject, data)

#Levene’s test is not significant (F(2, 152)=1,78, p>,05), 
#which means that the variance is equal across the groups, hence there is no violation of this assumption.

#### 3 Distribution of the DV

hist(data$avg.theta.diff)

##Perform the analysis:

#### an anova only
anova <- aov(avg.theta.diff ~ subject, data = data)
summary(anova)

#              Df Sum Sq  Mean Sq F value Pr(>F)
#subject       2 0.0100 0.004999   1.307  0.274
#Residuals   152 0.5815 0.003826 
#summary(mod)
#No significant differences between the three subjects with respect to the average difference between true and re-restimated theta
####Add the average number of mistakes in module A as a predictor

ancova1 <- aov(avg.theta.diff ~ avg.error.modA + subject, data = data)

summary(ancova1)

#                 Df Sum Sq Mean Sq F value  Pr(>F)    
#avg.error.modA   1 0.0812 0.08117  24.250 2.2e-06 ***
#  subject        2 0.0049 0.00245   0.732   0.483    
#Residuals      151 0.5055 0.00335  

#Now it shows that the Average number of errors in Module A, is a signficant predictor of the average difference between the true and the reestimated theta
#However there is still no significant difference between the subjects

ancova2 <- aov(avg.theta.diff ~ avg.error.modA + avg.error.Day1 + subject, data = data)

summary(ancova2)

#                    Df Sum Sq Mean Sq F value   Pr(>F)    
#   avg.error.modA   1 0.0812 0.08117  25.198 1.45e-06 ***
#   avg.error.Day1   1 0.0033 0.00334   1.037   0.3102    
#   subject          2 0.0238 0.01189   3.691   0.0272 *  
#   Residuals      150 0.4832 0.00322   

#Interesting: By adding the number of mistakes as a covariate, there starts to be a significant difference between the three subjects,
#even though the covariate itself is not significant


ancova3<- aov(avg.theta.diff ~ avg.error.modA + avg.error.Day1 + avg.item.weights.modA + subject, data = data)
summary(ancova3)

#                          Df Sum Sq Mean Sq F value   Pr(>F)    
#  avg.error.modA          1 0.0812 0.08117  25.198 1.45e-06 ***
#  avg.error.Day1          1 0.0033 0.00334   1.037   0.3102    
#  avg.item.weights.modA   1 0.0081 0.00807   2.504   0.1157    
#  subject                 1 0.0157 0.01572   4.878   0.0287 *  
#  Residuals             150 0.4832 0.00322 
#Nothing changes, so the average item weights in each module do not add anything 

###Post hoc (eith Bonfferoni correction)
post_hoc <- emmeans_test(avg.theta.diff ~ subject, covariate = c(avg.error.modA, avg.error.Day1),
                         p.adjust.method = "bonferroni", data = data)
post_hoc
#There is a significant difference between Lezen and Rekenen and Lezen and Taal, when controlling for average errors in mod 1 and day 1 (??)