#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##### Plots relating the true theta and the number of errors in module A###
#(Note, in order to run this code you need to first run thr `errors_module_A.R` script)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(ggplot2)
library(mice)
options(scipen=999)
#++++++++++++++++++++
#Lezen
#++++++++++++++++++
#students_abilities_L <- lapply(students_abilities_L, function(x){x[, -c(1,2,3,5)]})
students_abilities_L_split <- list()
for(i in 1:length(true.theta_L)){
  students_abilities_L_split[[i]] <-  split(students_abilities_L[[i]], students_abilities_L[[i]]$error)
}

#Get the mean re-resimated theta for each student and each number of mistakes
mean_thetas_L <- list()
for(i in 1:length(true.theta_L)){
  mean_thetas_L[[i]] <- lapply(students_abilities_L_split[[i]], function(x){mean(x[, 4])})
}
#store them in a seperate df
for(i in 1:length(true.theta_L)){
  mean_thetas_L[[i]] <- as.data.frame(mean_thetas_L[[i]])
}

# Combine
thetas.errors_L <- do.call(dplyr::bind_rows, mean_thetas_L)

#sort
thetas.errors_L <- thetas.errors_L[, c("X0", "X1", "X2", "X3", "X4", "X5", "X6")]
#Add true thetas as a column
thetas.errors_L$true.theta <- true.theta_L

# Since not all students have all the number of errors, when, a student doesn't have an error
# we can take two approaches:

#######1#######
# Treat the unobserved number of errors as missing values and impute them using 
# Bayesian multiple regression (the precise method can be aditionally discussed)
#Mice
imp <- mice(thetas.errors_L, m=1, seed = 789, meth= "norm.predict")
thetas.errors_L_imp <- complete(imp,1) #obtain the imputed data set
# Add the true thetas as a column

#######2#######
#Add the true theta values everywhere where we do not have an observed error
#(I do not know how wise this is, so I am not doing it for now)

##Plot###

#First coerce everything into one column and add a ariable indicating the number of errors
x <- unlist(thetas.errors_L_imp)
errors <- rep(c(0, 1, 2, 3, 4, 5, 6, "true theta"), each = length(true.theta_L))
errors_L_combined <- data.frame(x, errors)

#Now plot
ggplot(errors_L_combined, aes(x = x, fill = errors)) +  #we can do it with color as well
  geom_density(alpha=0.4)+ 
  xlim(-1.5, 2) +
  labs(title = "Lezen, number of errors in Module A",
       x = "Ability")

# Since there are too many levels of errors, maybe it is best to plot them 3 by 4

# Zero through 2 mistakes (this is more important)

errors_L_zero.two <- errors_L_combined %>%
  filter(errors == c(0, 1, 2, "true theta"))

ggplot(errors_L_zero.two, aes(x = x, fill = errors)) +
  geom_density(alpha=0.4)+ 
  xlim(-1.5, 2) +
  labs(title = "Lezen, number of errors in Module A (0:2)",
       x = "Ability")
  
 

# Three through 6 mistakes 

errors_L_three.six <- errors_L_combined %>%
  filter(errors == c(3, 4, 5, 6, "true theta"))

ggplot(errors_L_three.six, aes(x = x, fill = errors)) +
  geom_density(alpha=0.4)+ 
  xlim(-1.5, 2) +
  labs(title = "Lezen, number of errors in Module A (3:6)",
       x = "Ability")

#++++++++++++++++++++
#Rekenen
#++++++++++++++++++
students_abilities_R_split <- list()
for(i in 1:length(true.theta_R)){
  students_abilities_R_split[[i]] <-  split(students_abilities_R[[i]], students_abilities_R[[i]]$error)
}

#Get the mean re-resimated theta for each student and each number of mistakes
mean_thetas_R <- list()
for(i in 1:length(true.theta_R)){
  mean_thetas_R[[i]] <- lapply(students_abilities_R_split[[i]], function(x){mean(x[, 4])})
}
#store them in a seperate df
for(i in 1:length(true.theta_R)){
  mean_thetas_R[[i]] <- as.data.frame(mean_thetas_R[[i]])
}

# Combine
thetas.errors_R <- do.call(dplyr::bind_rows, mean_thetas_R)

#sort
thetas.errors_R <- thetas.errors_R[, c("X0", "X1", "X2", "X3", "X4", "X5")]
#Add true thetas as a column
thetas.errors_R$true.theta <- true.theta_R

# Since not all students have all the number of errors, when, a student doesn't have an error
# we can take two approaches:

#######1#######
# Treat the unobserved number of errors as missing values and impute them using 
# Bayesian multiple regression (the precise method can be aditionally discussed)
#Mice
imp <- mice(thetas.errors_R, m=1, seed = 2344, meth= "norm.predict")
thetas.errors_R_imp <- complete(imp,1) #obtain the imputed data set
# Add the true thetas as a column

#######2#######
#Add the true theta values everywhere where we do not have an observed error
#(I do not know how wise this is, so I am not doing it for now)

##Plot###

#First coerce everything into one column and add a ariable indicating the number of errors
x <- unlist(thetas.errors_R_imp)
errors <- rep(c(0, 1, 2, 3, 4, 5, "true theta"), each = length(true.theta_R))
errors_R_combined <- data.frame(x, errors)

#Now plot
ggplot(errors_R_combined, aes(x = x, fill = errors)) +  #we can do it with color as well
  geom_density(alpha=0.4)+ 
  xlim(-3, 3) +
  labs(title = "Rekenen, number of errors in Module A",
       x = "Ability")

# Since there are too many levels of errors, maybe it is best to plot them 3 by 4

# Zero through 2 mistakes (this is more important)

errors_R_zero.two <- errors_R_combined %>%
  filter(errors == c(0, 1, 2, "true theta"))

ggplot(errors_R_zero.two, aes(x = x, fill = errors)) +
  geom_density(alpha=0.4)+ 
  xlim(-3, 3) +
  labs(title = "Rekenen, number of errors in Module A (0:2)",
       x = "Ability")



# Three through 5 mistakes 

errors_R_three.six <- errors_R_combined %>%
  filter(errors == c(3, 4, 5, "true theta"))

ggplot(errors_R_three.six, aes(x = x, fill = errors)) +
  geom_density(alpha=0.4)+ 
  xlim(-3, 3) +
  labs(title = "Rekenen, number of errors in Module A (3:5)",
       x = "Ability")

#++++++++++++++++++++
#Taal
#++++++++++++++++++
students_abilities_T_split <- list()
for(i in 1:length(true.theta_T)){
  students_abilities_T_split[[i]] <-  split(students_abilities_T[[i]], students_abilities_T[[i]]$error)
}

#Get the mean re-resimated theta for each student and each number of mistakes
mean_thetas_T <- list()
for(i in 1:length(true.theta_T)){
  mean_thetas_T[[i]] <- lapply(students_abilities_T_split[[i]], function(x){mean(x[, 4])})
}
#store them in a seperate df
for(i in 1:length(true.theta_T)){
  mean_thetas_T[[i]] <- as.data.frame(mean_thetas_T[[i]])
}

# Combine
thetas.errors_T <- do.call(dplyr::bind_rows, mean_thetas_T)

#sort
thetas.errors_T <- thetas.errors_T[, c("X0", "X1", "X2", "X3", "X4", "X5")]
#Add true thetas as a column
thetas.errors_T$true.theta <- true.theta_T

# Since not all students have all the number of errors, when, a student doesn't have an error
# we can take two approaches:

#######1#######
# Treat the unobserved number of errors as missing values and impute them using 
# Bayesian multiple regression (the precise method can be aditionally discussed)
#Mice
imp <- mice(thetas.errors_T, m=1, seed = 2344, meth= "norm.predict")
thetas.errors_T_imp <- complete(imp,1) #obtain the imputed data set
# Add the true thetas as a column

#######2#######
#Add the true theta values everywhere where we do not have an observed error
#(I do not know how wise this is, so I am not doing it for now)

##Plot###

#First coerce everything into one column and add a ariable indicating the number of errors
x <- unlist(thetas.errors_T_imp)
errors <- rep(c(0, 1, 2, 3, 4, 5, "true theta"), each = length(true.theta_T))
errors_T_combined <- data.frame(x, errors)

#Now plot
ggplot(errors_T_combined, aes(x = x, fill = errors)) +  #we can do it with color as well
  geom_density(alpha=0.4)+ 
  xlim(-3, 3) +
  labs(title = "Taal, number of errors in Module A",
       x = "Ability")

# Since there are too many levels of errors, maybe it is best to plot them 3 by 4

# Zero through 2 mistakes (this is more important)

errors_T_zero.two <- errors_T_combined %>%
  filter(errors == c(0, 1, 2, "true theta"))

ggplot(errors_T_zero.two, aes(x = x, fill = errors)) +
  geom_density(alpha=0.4)+ 
  xlim(-3, 3) +
  labs(title = "Taal, number of errors in Module A (0:2)",
       x = "Ability")



# Three through 5 mistakes 

errors_T_three.six <- errors_T_combined %>%
  filter(errors == c(3, 4, 5, "true theta"))

ggplot(errors_T_three.six, aes(x = x, fill = errors)) +
  geom_density(alpha=0.4)+ 
  xlim(-3, 3) +
  labs(title = "Taal, number of errors in Module A (3:5)",
       x = "Ability")
