#+++++++++++++++++++++++++++++++++++++++++++++
###Analyses of the experiment data############
#++++++++++++++++++++++++++++++++++++++++++++
library(haven)
library(tidyverse)
library(broom)
library(emmeans)
library(rstatix)
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

# Lezen       0.109
# Rekenen     0.127
# Taal        0.127

#Lezen has a lower mean difference, compared to Rekenen and Taal who, surprisingly, have the same value.





#ANCOVA
#CAUTION: WE TAKE THE P-VALUES `WITH A GRAIN OF SALT` SINCE MANY OF THE ASSUMPTIONS ARE VIOLATED AND WE ARE NOT DEALING WITH A LINEAR RELATIONSHIP
#OTHER TECHNIQUES WILL BE CONSIDERED (SUCH AS FITTING SOME TYPE OF A GLM) AND/OR USING THE Johnson-Neyman TECHNIQUE
#https://kenstoyama.wordpress.com/2018/01/21/the-johnson-neyman-tecnique-and-an-r-script-to-apply-it/

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

#Even though there are violations, since the nature of the DV is such that we are interested in conventional linearity
# we will skip it


####2 Homogeneity of variance
library(car)
leveneTest(avg.theta.diff ~ subject, data)

#Levenes test is not significant (F(2, 152)=1,78, p>,05), 
#which means that the variance is equal across the groups, hence there is no violation of this assumption.

#### 3 Distribution of the DV

hist(data$avg.theta.diff)

##Perform the analysis:  *Note in the analyses below I might use the terms `covariate` and `predictor` interchangeably 

#### an anova only
anova <- aov(avg.theta.diff ~ subject, data = data)
summary(anova)

#              Df Sum Sq  Mean Sq F value Pr(>F)
#subject       2 0.0100 0.004999   1.307  0.274
#Residuals   152 0.5815 0.003826 
#summary(mod)
#No significant differences between the three subjects with respect to the average difference between true and re-restimated theta


####Add the other variables as covariates and incpect the TYPE III sum of squares results
#for more info see: https://towardsdatascience.com/anovas-three-types-of-estimating-sums-of-squares-don-t-make-the-wrong-choice-91107c77a27a
#Type III sum of squares does not assume equal sample sizes
# We look at the partiacl sum of squares, to see which covariate has a significant contribution 
ancova1 <- aov(avg.theta.diff ~ avg.error.modA + avg.error.Day1 + subject, data = data)
Anova(ancova1, type = "III")
#                  Sum Sq  Df  F value    Pr(>F)    
#  (Intercept)    0.44231   1 137.2974 < 2.2e-16 ***
#  avg.error.modA 0.00475   1   1.4759  0.226321    
#  avg.error.Day1 0.02222   1   6.8984  0.009523 ** 
#  subject        0.02378   2   3.6911  0.027238 *  
#  Residuals      0.48323 150                     

#We can see that after we control for the average number of mistakes in the first day, there appears to be
# a significant difference between the two groups (significant at the .05 level), the average number of mistakes in module 1 doesn't seem to
#be a significant covariate (I even checked this when fitting a model only with that variable as a sovariate
#and no differences between the two groups were observed).


#we will get rid of average number of errors in module A, and add the number of paths as a covariate:
ancova2 <- aov(avg.theta.diff ~ avg.error.Day1 + n.paths + subject, data = data)
Anova(ancova2, type = "III")

#                   Sum Sq  Df  F value    Pr(>F)    
#(Intercept)      0.77927   1 409.1711 < 2.2e-16 ***
#  avg.error.Day1 0.16366   1  85.9313 < 2.2e-16 ***
#  n.paths        0.20231   1 106.2246 < 2.2e-16 ***
#  subject        0.02482   2   6.5151  0.001935 ** 
#  Residuals      0.28568 150 

#After we control for both the average number of errors in day 1 AND in the number of paths we see that the difference
# between the three subjects for the average discrepancy between the two groups is more significant, not on the 0.01 level

#Lastly let's add the average item weights in Module A as a covariate
#We canot add this variable as a predictor since the model becomes singular!!!
data$avg.item.weights.modA <- as.factor(data$avg.item.weights.modA)
ancova3 <- aov(avg.theta.diff ~ avg.item.weights.modA + subject, data = data)
Anova(ancova3, type = "III")

#==========================================================
#So we will stick with the model ancova 2 and perform the post hoc using those covariates 

post_hoc <- emmeans_test(avg.theta.diff ~ subject, covariate = c(avg.error.Day1, n.paths),
                         p.adjust.method = "bonferroni", data = data) #using a bonfferoni correction
post_hoc

#term                           .y.            group1 group2    df statistic       p   p.adj p.adj.signif
#* <chr>                          <chr>          <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
#  1 avg.error.Day1*n.paths*subject avg.theta.diff 1      2        150    -3.00  0.00313 0.00939 **          
#  2 avg.error.Day1*n.paths*subject avg.theta.diff 1      3        150    -3.35  0.00103 0.00309 **          
#  3 avg.error.Day1*n.paths*subject avg.theta.diff 2      3        150    -0.475 0.635   1       ns

#There is a significant difference between Lezen and Rekenen and Lezen and Taal, when controlling for
#average errors onday 1 and the number of paths taken.

#We can roughly conclude that the average number of errors on Day 1 and the number of paths play a significant role
# for the theta discrepancy for Lezen. In other words, when we control for the average number of mistakes on day 1
#and the number of taken paths, we can see a significant difference in the average theta discrepancy between Lezen
#and and Rekenen and Taal (BUT NO DIFFERENCE between Rekenen and Taal).

#Lets check the same model with a regression

reg_mod <- lm(avg.theta.diff ~ avg.error.Day1 + n.paths + subject, data = data)
summary(reg_mod)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     0.1836482  0.0090789  20.228  < 2e-16 ***
#  avg.error.Day1 -0.0042110  0.0004543  -9.270  < 2e-16 ***
#  n.paths        -0.0114413  0.0011101 -10.307  < 2e-16 ***
#  subject2        0.0268332  0.0089349   3.003  0.00313 ** 
#  subject3        0.0308615  0.0092173   3.348  0.00103 ** 

#Same conclusions 

#Let's check the resiudals
plot(reg_mod)
#There are violations, as expected, however, the results we obtained are still 
