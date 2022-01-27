#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Experiment data for differences between the three courses
#NOTE: YOU NEED TO first load in the simulation patterns, and execute the code in 
# `errors_module_A.R` and `errors_day_1_Lezen`, `errors_day_1_Rekenen` and `errors_day_1_Taal`
#before running the code in this script
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#load("simulated.responses.RData")
#+http://www.dwoll.de/rexrepos/posts/anovaMixed.html
#+https://stats.stackexchange.com/questions/58745/using-lmer-for-repeated-measures-linear-mixed-effect-model
#+
#+https://statmodeling.stat.columbia.edu/2007/08/16/no_you_dont_nee/
#
# Outcome variables are: The average absolute difference between the estimated and real theta; The number of mistakes in mod A
# and the number of mistakes on the first day. 
#The factor is the subject;
# The covariates are:
#- number of forbidden paths;
#- weights of the items in the first module (either the sum or the average)
#- How well the information function follow the abilities of students (this will be per student) 


########Lezen---------------------------------------------------------------------

#1. DV: Define a column indicating the (absolute)difference between true and estimated theta
for (i in 1:length(true.theta_L)){
  students_abilities_L[[i]]$diff <-  abs(students_abilities_L[[i]]$true_theta - students_abilities_L[[i]]$theta)
}

avg.theta.diff <- sapply(students_abilities_L, function(x){mean(x$diff)})  #construct the variable

#COVARIATES:

#Average number of errors in module 1
avg.error.modA <- sapply(students_abilities_L, function(x){mean(x$errors)})  #construct the variable

#Average errors on the first day
avg.error.Day1 <- sapply(students_abilities_L, function(x){mean(x$errors.Day1)}) 

# Number of paths for each student
n.paths <- NULL
for(i in 1:length(true.theta_L)){
  n.paths[i] <- length(unique(students_abilities_L[[i]]$booklet_id))
}

#Weights of the items in the first module (I am taking the average here, since I think it's better than taking the sum):
#this will be a "disaggregated" level 2 variable
item_weights_A <- pars_L %>%
  filter(item_id == c("BL00518", "BL00519", "BL00520", "BL00521", "BL02412",  "OP00142")) %>%
  select(item_score) 
avg.item.weights.modA <- mean(item_weights_A$item_score)
avg.item.weights.modA <- rep(avg.item.weights.modA , length(true.theta_L))
subject <- rep("Lezen", length(true.theta_L))

data_L <- data.frame(avg.theta.diff, avg.error.modA, avg.error.Day1, n.paths, avg.item.weights.modA, subject)

########Rekenen---------------------------------------------------------------------

#1. DV: Define a column indicating the (absolute)difference between true and estimated theta
for (i in 1:length(true.theta_R)){
  students_abilities_R[[i]]$diff <-  abs(students_abilities_R[[i]]$true_theta - students_abilities_R[[i]]$theta)
}

avg.theta.diff <- sapply(students_abilities_R, function(x){mean(x$diff)})  #construct the variable

#COVARIATES:

#Average number of errors in module 1
avg.error.modA <- sapply(students_abilities_R, function(x){mean(x$errors)})  #construct the variable

#Average errors on the first day
avg.error.Day1 <- sapply(students_abilities_R, function(x){mean(x$errors.Day1)}) 

# Number of paths for each student
n.paths <- NULL
for(i in 1:length(true.theta_R)){
  n.paths[i] <- length(unique(students_abilities_R[[i]]$booklet_id))
}

#Weights of the items in the first module (I am taking the average here, since I think it's better than taking the sum):
#this will be a "disaggregated" level 2 variable
item_weights_A <- pars_R %>%
  filter(item_id == c("RD516087", "RD516456", "RD516A28", "RD516663", "RD516375")) %>%
  select(item_score) 
avg.item.weights.modA <- mean(item_weights_A$item_score)
avg.item.weights.modA <- rep(avg.item.weights.modA , length(true.theta_R))
subject <- rep("Rekenen", length(true.theta_R))

data_R <- data.frame(avg.theta.diff, avg.error.modA, avg.error.Day1, n.paths, avg.item.weights.modA, subject)

########Taal---------------------------------------------------------------------

#1. DV: Define a column indicating the (absolute)difference between true and estimated theta
for (i in 1:length(true.theta_T)){
  students_abilities_T[[i]]$diff <-  abs(students_abilities_T[[i]]$true_theta - students_abilities_T[[i]]$theta)
}

avg.theta.diff <- sapply(students_abilities_T, function(x){mean(x$diff)})  #construct the variable

#COVARIATES:

#Average number of errors in module 1
avg.error.modA <- sapply(students_abilities_T, function(x){mean(x$errors)})  #construct the variable

#Average errors on the first day
avg.error.Day1<- sapply(students_abilities_T, function(x){mean(x$errors.Day1)}) 

# Number of paths for each student
n.paths <- NULL
for(i in 1:length(true.theta_T)){
  n.paths[i] <- length(unique(students_abilities_T[[i]]$booklet_id))
}

#Weights of the items in the first module (I am taking the average here, since I think it's better than taking the sum):
#this will be a "disaggregated" level 2 variable
item_weights_A <- pars_T %>%
  filter(item_id == c("SN00003D", "SN00038", "SW00090D", "IP00074", "GR00148")) %>%
  select(item_score) 
avg.item.weights.modA <- mean(item_weights_A$item_score)
avg.item.weights.modA <- rep(avg.item.weights.modA , length(true.theta_T))
subject <- rep("Taal", length(true.theta_T))

data_T <- data.frame(avg.theta.diff, avg.error.modA, avg.error.Day1, n.paths, avg.item.weights.modA, subject)

#================================================================
#Now combine
experiment_data <- rbind(data_L, data_R, data_T)
experiment_data$subject <- as.factor(experiment_data$subject)
#save it as a sav file
library(haven)
write_sav(experiment_data, "experiment_data.sav")
#================================================================


