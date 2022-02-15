#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Experiment data for differences between the three courses
#NOTE: YOU NEED TO first load in the simulation patterns, and execute the code in 
# `errors_module_A.R` and `errors_day_1_Lezen`, `errors_day_1_Rekenen` and `errors_day_1_Taal`
#before running the code in this script
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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


subject <- rep("Lezen", length(true.theta_L))

data_L <- data.frame(avg.theta.diff, avg.error.modA, avg.error.Day1, n.paths, subject)

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

subject <- rep("Rekenen", length(true.theta_R))

data_R <- data.frame(avg.theta.diff, avg.error.modA, avg.error.Day1, n.paths, subject)

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

subject <- rep("Taal", length(true.theta_T))

data_T <- data.frame(avg.theta.diff, avg.error.modA, avg.error.Day1, n.paths, subject)

#================================================================
#Now combine
experiment_data <- rbind(data_L, data_R, data_T)
experiment_data$subject <- as.factor(experiment_data$subject)
#save it as a sav file
library(haven)
write_sav(experiment_data, "experiment_data.sav")
#================================================================


