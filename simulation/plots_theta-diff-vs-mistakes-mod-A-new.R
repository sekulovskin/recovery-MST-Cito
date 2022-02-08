#=======================================================================
# Plots for the the difference between the true 
#and the estimated theta for different number of mistakes in Module A
#======================================================================
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship/repo/mst/simulation")
library(ggplot2)
library(dexterMST)
library(dplyr)
options(warn = 1) #get rid of the warnings 
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship/repo/mst/simulation")
#As discussed in the email I will use the provided items parameters,
#routing rules and test design from ACET.
test_design <- read.csv('dexter_mst_design_dov.csv')
routing_rules <- read.csv('dexter_mst_rules_dov.csv')
pars <- read.csv('parameters_dov.csv')

N_students <- 200 #We simulate 200 responses
#============================================
# For the reading test (Lezen)
#===========================================

#Filter only with respect to the reading test
test_design_L <-test_design[grepl('^L', test_design$module_id), ]
routing_rules_L <- routing_rules[grepl('^L', routing_rules$module_id), ]
pars_L <- pars[pars[, 'item_id'] %in% test_design_L$item_id, ]

##Based on the observed abilities (see, Simulated-vs-observed-thetas.R)
set.seed(123)
true.theta_L <- sort(rnorm(N_students, 0.219, 0.236), decreasing = FALSE)
#summary(true.theta_L)
#----------------------------------------------------
#Simulate response data and respective abilities n_sim number of times
n_sim <- 100 
#Construct "storage space"
patterns_L <- list()
abilities_L <- list()

#loop over n_sim number of times to obtain different response patterns
for(i in 1:n_sim){
  patterns_L[[i]] <- dexterMST::sim_mst(pars_L, true.theta_L, test_design_L, routing_rules_L, routing = 'all')
  abilities_L[[i]] <- dexter::ability(patterns_L[[i]], parms = pars_L, method = 'WLE')
  abilities_L[[i]]$true_theta <- true.theta_L #The WLE was proposed by Warm (1989) to reduce bias in the MLE and is also known as the Warm estimator
}

#--------------------------------------------------

#Combine it in a data frame
abilities_L <- do.call(rbind, abilities_L)

#store each student's ability estimates in a separate data frame
students_abilities_L <- list()
for(i in 1:length(true.theta_L)){
  students_abilities_L[[i]] <- abilities_L %>%
    filter(person_id == i)
}

rm(abilities_L)

#--------------------------------------------------
# Do the same with the response patterns
patterns_L <- do.call(rbind, patterns_L)

#store each student's ability estimates in a separate data frame
students_patterns_L <- list()

for(i in 1:length(true.theta_L)){
  students_patterns_L[[i]] <- patterns_L %>%
    filter(person_id == i)
}

rm(patterns_L)

## Repeat all of this for the other subjects that follor an MST design (Rekenen en Taal)


#============================================
# For the Math test (Rekenen)
#===========================================

#Filter only with respect to the Math test
test_design_R <-test_design[grepl('^R', test_design$module_id), ]
routing_rules_R <- routing_rules[grepl('^R', routing_rules$module_id), ]
pars_R <- pars[pars[, 'item_id'] %in% test_design_R$item_id, ]

##Based on the observed abilities (see, Simulated-vs-observed-thetas.R)
set.seed(123)
true.theta_R <- sort(rnorm(N_students, 0.326, 0.389), decreasing = FALSE)
#----------------------------------------------------

#Construct "storage space"
patterns_R <- list()
abilities_R <- list()

#loop over n_sim number of times to obtain different response patterns
for(i in 1:n_sim){
  patterns_R[[i]] <- dexterMST::sim_mst(pars_R, true.theta_R, test_design_R, routing_rules_R, routing = 'all')
  abilities_R[[i]] <- dexter::ability(patterns_R[[i]], parms = pars_R, method = 'WLE')
  abilities_R[[i]]$true_theta <- true.theta_R #The WLE was proposed by Warm (1989) to reduce bias in the MLE and is also known as the Warm estimator
}

#--------------------------------------------------

#Combine it in a data frame
abilities_R <- do.call(rbind, abilities_R)

#store each student in a separate data frame
students_abilities_R <- list()
for(i in 1:length(true.theta_R)){
  students_abilities_R[[i]] <- abilities_R %>%
    filter(person_id == i)
}

rm(abilities_R)

#--------------------------------------------------
# Do the same with the response patterns
patterns_R <- do.call(rbind, patterns_R)

#store each student's ability estimates in a separate data frame
students_patterns_R <- list()

for(i in 1:length(true.theta_R)){
  students_patterns_R[[i]] <- patterns_R %>%
    filter(person_id == i)
}

rm(patterns_R)

#============================================
# For the Language test (Taal)
#===========================================

#Filter only with respect to the Language test
test_design_T <-test_design[grepl('^T', test_design$module_id), ]
routing_rules_T <- routing_rules[grepl('^T', routing_rules$module_id), ]
pars_T <- pars[pars[, 'item_id'] %in% test_design_T$item_id, ]

##Based on the observed abilities (see, Simulated-vs-observed-thetas.R)
set.seed(123)
true.theta_T <- sort(rnorm(N_students, 0.273, 0.301), decreasing = FALSE)
#----------------------------------------------------

#Construct "storage space"
patterns_T <- list()
abilities_T <- list()

#loop over n_sim number of times to obtain different response patterns
for(i in 1:n_sim){
  patterns_T[[i]] <- dexterMST::sim_mst(pars_T, true.theta_T, test_design_T, routing_rules_T, routing = 'all')
  abilities_T[[i]] <- dexter::ability(patterns_T[[i]], parms = pars_T, method = 'WLE')
  abilities_T[[i]]$true_theta <- true.theta_T 
}

#--------------------------------------------------

#Combine it in a data frame
abilities_T <- do.call(rbind, abilities_T)

#store each student in a separate data frame
students_abilities_T <- list()
for(i in 1:length(true.theta_T)){
  students_abilities_T[[i]] <- abilities_T %>%
    filter(person_id == i)
}

rm(abilities_T)
--------------------------------------------------
  # Do the same with the response patterns
patterns_T <- do.call(rbind, patterns_T)

#store each student's ability estimates in a separate data frame
students_patterns_T <- list()

for(i in 1:length(true.theta_T)){
  students_patterns_T[[i]] <- patterns_T %>%
    filter(person_id == i)
}

rm(patterns_T)


#======================================================================
# RUN THE `errors_module_A.R` scripit to calculate the number of errors
#======================================================================

#=I will take the lowest 40, highest achieving 50 students and the middle students
#++++++++++++++++++++++++++
#       Lezen
#+++++++++++++++++++++++++

# Calculate the differnece
for (i in 1:length(true.theta_L)){
  students_abilities_L[[i]]$diff <-  abs(students_abilities_L[[i]]$true_theta) - abs(students_abilities_L[[i]]$theta)
}

#Split with respect to the number of mistakes 
students_abilities_L_split <- list()
for(i in 1:length(true.theta_L)){
  students_abilities_L_split[[i]] <- split(students_abilities_L[[i]], students_abilities_L[[i]]$error)
}


##### Low achieving students

#Take the 50 least proficient students  NOTE: WE WILL BE LOOKING ONLY AT 6, 5, and 4 MISTAKES (SINCE ALL STUDENTS HAVE THOSE)

least_proficient_L <- list()

for(i in 1:15){
  least_proficient_L[[i]] <- students_abilities_L_split[[i]]
}

#take out the mean differences for each student per mistake

#6 mistakes

six_mistakes_L <- c()

for(i in 1:15){
  six_mistakes_L[i] <- mean(least_proficient_L[[i]]$`6`$diff)
}

#5 mistakes

five_mistakes_L <- c()

for(i in 1:15){
  five_mistakes_L[i] <- mean(least_proficient_L[[i]]$`5`$diff)
}


#4 mistakes

four_mistakes_L <- c()

for(i in 1:15){
  four_mistakes_L[i] <- mean(least_proficient_L[[i]]$`4`$diff)
}

#Combine in one DF

n.errors <- rep(6:4, each = 15)
theta.diff <- c(six_mistakes_L, five_mistakes_L, four_mistakes_L)
df.low_L <- data.frame(theta.diff, n.errors)
df.low_L$n.errors <- as.factor(df.low_L$n.errors)

#Plot

ggplot(df.low_L, aes(x = theta.diff, fill = n.errors)) +  
  geom_histogram(alpha = 0.5) + 
  xlim(-0.1, 0.1) +
  labs(title = "Lezen (Module A), least proficient students",
       x = "Difference between true and estimated ability")


####Medium achieving students
medium_proficient_L <- list()

for(i in 16:185){
  medium_proficient_L[[i]] <- students_abilities_L_split[[i]]
}


# 2 mistakes 

two_mistakes_L <- c()

for(i in 16:185){
  two_mistakes_L[i] <- mean(medium_proficient_L[[i]]$`2`$diff)
}

two_mistakes_L <- na.omit(two_mistakes_L)

# one mistake

one_mistake_L <- c()

for(i in 16:185){
  one_mistake_L[i] <- mean(medium_proficient_L[[i]]$`1`$diff)
}

one_mistake_L <- na.omit(one_mistake_L)
#Combine in one DF

n.errors <- rep(1:2, each =length(16:185))
theta.diff <- c(one_mistake_L, two_mistakes_L)
df.medium_L <- data.frame(theta.diff, n.errors)
df.medium_L$n.errors <- as.factor(df.medium_L$n.errors)

#Plot

ggplot(df.medium_L, aes(x = theta.diff, fill = n.errors)) +  
  geom_histogram(alpha=0.5)+ 
  xlim(-0.1, 0.1) +
  labs(title = "Lezen (Module A), medium proficient students",
       x = "Difference between true and estimated ability")



##### High achieving students

most_proficient_L <- list()

for(i in 186:200){
  most_proficient_L[[i]] <- students_abilities_L_split[[i]]
}


# 0 mistakes 

zero_mistakes_L <- c()

for(i in 186:200){
  zero_mistakes_L[i] <- mean(most_proficient_L[[i]]$`0`$diff)
}

zero_mistakes_L <- na.omit(zero_mistakes_L)
# one mistake

one_mistake_L <- c()

for(i in 186:200){
  one_mistake_L[i] <- mean(most_proficient_L[[i]]$`1`$diff)
}

one_mistake_L <- na.omit(one_mistake_L)
#Combine in one DF

n.errors <- rep(0:1, each = 15)
theta.diff <- c(zero_mistakes_L, one_mistake_L)
df.high_L <- data.frame(theta.diff, n.errors)
df.high_L$n.errors <- as.factor(df.high_L$n.errors)

#Plot

ggplot(df.high_L, aes(x = theta.diff, fill = n.errors)) +  
  geom_histogram(alpha=0.5)+ 
  xlim(-0.1, 0.5) +
  labs(title = "Lezen (Module A), most proficient students",
       x = "Difference between true and estimated ability")

#Conclusion: Discrepancies are higher with the most proficient students
#++++++++++++++++++++++++++
#       Rekenen
#+++++++++++++++++++++++++

# Calculate the differnece
for (i in 1:length(true.theta_R)){
  students_abilities_R[[i]]$diff <-  abs(students_abilities_R[[i]]$true_theta) - abs(students_abilities_R[[i]]$theta)
}

#Split with respect to the number of mistakes 
students_abilities_R_split <- list()
for(i in 1:length(true.theta_R)){
  students_abilities_R_split[[i]] <- split(students_abilities_R[[i]], students_abilities_R[[i]]$error)
}


##### Low achieving students

#Take the 15 least proficient students  NOTE: WE WILL BE LOOKING ONLY AT 5 AND 4 MISTAKES (SINCE ALL STUDENTS HAVE THOSE)

least_proficient_R <- list()

for(i in 1:15){
  least_proficient_R[[i]] <- students_abilities_R_split[[i]]
}

#take out the mean differences for each student per mistake

#5 mistakes

five_mistakes_R <- c()

for(i in 1:15){
  five_mistakes_R[i] <- mean(least_proficient_R[[i]]$`5`$diff)
}

#4 mistakes

four_mistakes_R <- c()

for(i in 1:15){
  four_mistakes_R[i] <- mean(least_proficient_R[[i]]$`4`$diff)
}


#Combine in one DF

n.errors <- rep(5:4, each = 15)
theta.diff <- c(five_mistakes_R, four_mistakes_R)
df.low_R <- data.frame(theta.diff, n.errors)
df.low_R$n.errors <- as.factor(df.low_R$n.errors)

#Plot

ggplot(df.low_R, aes(x = theta.diff, fill = n.errors)) +  
  geom_histogram(alpha=0.5)+ 
  xlim(-0.1, 0.5) +
  labs(title = "Rekenen (Module A), least proficient students",
       x = "Difference between true and estimated ability")

####Medium achieving students
medium_proficient_R <- list()

for(i in 16:45){
  medium_proficient_R[[i]] <- students_abilities_R_split[[i]]
}


# 3 mistakes
three_mistakes_R <- c()

for(i in 16:45){
  three_mistakes_R[i] <- mean(medium_proficient_R[[i]]$`3`$diff)
}

three_mistakes_R <- na.omit(three_mistakes_R)

# 2 mistakes 

two_mistakes_R <- c()

for(i in 16:45){
  two_mistakes_R[i] <- mean(medium_proficient_R[[i]]$`2`$diff)
}

two_mistakes_R <- na.omit(two_mistakes_R)
two_mistakes_R <- two_mistakes_R[1:20]

# one mistake

one_mistake_R <- c()

for(i in 16:45){
  one_mistake_R[i] <- mean(medium_proficient_R[[i]]$`1`$diff)
}

one_mistake_R <- na.omit(one_mistake_R)
one_mistake_R <- one_mistake_R[1:20]
#Combine in one DF

n.errors <- rep(c(1,2,3), each = 20)
theta.diff <- c(one_mistake_R, two_mistakes_R, three_mistakes_R)
df.medium_R <- data.frame(theta.diff, n.errors)
df.medium_R$n.errors <- as.factor(df.medium_R$n.errors)

#Plot

ggplot(df.medium_R, aes(x = theta.diff, fill = n.errors)) +  
  geom_density(alpha=0.4)+ 
  xlim(-0.1, 0.5) +
  labs(title = "Rekenen (Module A), medium proficient students",
       x = "Difference between true and estimated ability")


##### High achieving students
#Since students 61, 62 and 63 have only 0 mistakes we will start with student 60

most_proficient_R <- list()

for(i in 45:60){  #student 50 has 0 mistakes only! )that's why they here I start from 45
  most_proficient_R[[i]] <- students_abilities_R_split[[i]]
}


# 0 mistakes 

zero_mistakes_R <- c()

for(i in 46:60){
  zero_mistakes_R[i] <- mean(most_proficient_R[[i]]$`0`$diff)
}

zero_mistakes_R <- na.omit(zero_mistakes_R)
# one mistake

one_mistake_R <- c()

for(i in 45:60){
  one_mistake_R[i] <- mean(most_proficient_R[[i]]$`1`$diff)   #student 50 has 0 mistakes only! )that's why they here I start from 45
}

one_mistake_R <- na.omit(one_mistake_R)
#Combine in one DF

n.errors <- rep(0:1, each = 15)
theta.diff <- c(zero_mistakes_R, one_mistake_R)
df.high_R <- data.frame(theta.diff, n.errors)
df.high_R$n.errors <- as.factor(df.high_R$n.errors)

#Plot

ggplot(df.high_R, aes(x = theta.diff, fill = n.errors)) +   #LOOK INTO THIS PLOT
  geom_density(alpha=0.4)+ 
  xlim(-0.5, 0.5) +
  labs(title = "Rekenen (Module A), most proficient students",
       x = "Difference between true and estimated ability")

#++++++++++++++++++++++++++
#     Taal
#+++++++++++++++++++++++++

# Calculate the differnece
for (i in 1:length(true.theta_T)){
  students_abilities_T[[i]]$diff <-  abs(students_abilities_T[[i]]$true_theta - students_abilities_T[[i]]$theta)
}

#Split with respect to the number of mistakes 
students_abilities_T_split <- list()
for(i in 1:length(true.theta_T)){
  students_abilities_T_split[[i]] <- split(students_abilities_T[[i]], students_abilities_T[[i]]$error)
}


##### Low achieving students

#Take the 15 least proficient students  NOTE: WE WILL BE LOOKING ONLY AT 5, 4 and 3 MISTAKES (SINCE ALL STUDENTS HAVE THOSE)

least_proficient_T <- list()

for(i in 1:15){
  least_proficient_T[[i]] <- students_abilities_T_split[[i]]
}

#take out the mean differences for each student per mistake

#5 mistakes

five_mistakes_T <- c()

for(i in 1:15){
  five_mistakes_T[i] <- mean(least_proficient_T[[i]]$`5`$diff)
}

#4 mistakes

four_mistakes_T <- c()

for(i in 1:15){
  four_mistakes_T[i] <- mean(least_proficient_T[[i]]$`4`$diff)
}

# 3 mistakes
three_mistakes_T <- c()

for(i in 1:15){
  three_mistakes_T[i] <- mean(least_proficient_T[[i]]$`3`$diff)
}
#Combine in one DF

n.errors <- rep(5:3, each = 15)
theta.diff <- c(five_mistakes_T, four_mistakes_T, three_mistakes_T)
df.low_T <- data.frame(theta.diff, n.errors)
df.low_T$n.errors <- as.factor(df.low_T$n.errors)

#Plot

ggplot(df.low_T, aes(x = theta.diff, fill = n.errors)) +  
  geom_density(alpha=0.4)+ 
  xlim(0, 0.3) +
  labs(title = "Taal (Module A), least proficient students",
       x = "Difference between true and estimated ability")

####Medium achieving students
medium_proficient_T <- list()

for(i in 16:36){
  medium_proficient_T[[i]] <- students_abilities_T_split[[i]]
}


# 3 mistakes
three_mistakes_T <- c()

for(i in 16:36){
  three_mistakes_T[i] <- mean(medium_proficient_T[[i]]$`3`$diff)
}

three_mistakes_T <- na.omit(three_mistakes_T)

# 2 mistakes 

two_mistakes_T <- c()

for(i in 16:36){
  two_mistakes_T[i] <- mean(medium_proficient_T[[i]]$`2`$diff)
}

two_mistakes_T <- na.omit(two_mistakes_T)
two_mistakes_T <- two_mistakes_T[1:20]

# one mistake

one_mistake_T <- c()

for(i in 16:36){
  one_mistake_T[i] <- mean(medium_proficient_T[[i]]$`1`$diff)
}

one_mistake_T <- na.omit(one_mistake_T)
one_mistake_T <- one_mistake_T[1:20]
#Combine in one DF

n.errors <- rep(c(1,2,3), each = 20)
theta.diff <- c(one_mistake_T, two_mistakes_T, three_mistakes_T)
df.medium_T <- data.frame(theta.diff, n.errors)
df.medium_T$n.errors <- as.factor(df.medium_T$n.errors)

#Plot

ggplot(df.medium_T, aes(x = theta.diff, fill = n.errors)) +  
  geom_density(alpha=0.4)+ 
  xlim(-0.1, 0.5) +
  labs(title = "Taal (Module A), medium proficient students",
       x = "Difference between true and estimated ability")

##### High achieving students


most_proficient_T <- list()

for(i in 37:51){ 
  most_proficient_T[[i]] <- students_abilities_T_split[[i]]
}


# 0 mistakes 

zero_mistakes_T <- c()

for(i in 37:51){
  zero_mistakes_T[i] <- mean(most_proficient_T[[i]]$`0`$diff)
}

zero_mistakes_T <- na.omit(zero_mistakes_T)

# one mistake

one_mistake_T <- c()

for(i in 37:51){
  one_mistake_T[i] <- mean(most_proficient_T[[i]]$`1`$diff)   #student 50 has 0 mistakes only! )that's why they here I start from 45
}

one_mistake_T <- na.omit(one_mistake_T)
#Combine in one DF

n.errors <- rep(0:1, each = 15)
theta.diff <- c(zero_mistakes_R, one_mistake_T)
df.high_T <- data.frame(theta.diff, n.errors)
df.high_T$n.errors <- as.factor(df.high_T$n.errors)

#Plot

ggplot(df.high_T, aes(x = theta.diff, fill = n.errors)) +  
  geom_density(alpha=0.4)+ 
  xlim(-0, 0.45) +
  labs(title = "Taal (Module A), most proficient students",
       x = "Difference between true and estimated ability")

