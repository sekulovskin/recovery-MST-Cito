#=======================================================================
# Plots for the the difference between the true 
#and the estimated theta for different number of mistakes in Module A
#======================================================================
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship/repo/mst/simulation")
load("simulated.responses.RData")
library(ggplot2)
#Again I will take the lowest15, highest achieving 15 students and the middle students and take their average theta_difference for all different number of mistakes
#For example I will plot the distribution of the lowest achieving  students for different number of errors, and will do the same (IN A SEPARATE)
#plot for the highest achieving students, and repeat this for all three subjects.
#NOTE: in order to run this script, you first need to run the script(s) `errors_module_A.R`...


#++++++++++++++++++++++++++
#       Lezen
#+++++++++++++++++++++++++

# Calculate the differnece
  for (i in 1:length(true.theta_L)){
    students_abilities_L[[i]]$diff <-  abs(students_abilities_L[[i]]$true_theta - students_abilities_L[[i]]$theta)
  }

#Split with respect to the number of mistakes 
students_abilities_L_split <- list()
  for(i in 1:length(true.theta_L)){
    students_abilities_L_split[[i]] <- split(students_abilities_L[[i]], students_abilities_L[[i]]$error)
  }


##### Low achieving students

#Take the 15 least proficient students  NOTE: WE WILL BE LOOKING ONLY AT 6, 5, and 4 MISTAKES (SINCE ALL STUDENTS HAVE THOSE)

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
  geom_density(alpha=0.4)+ 
  xlim(-0.1, 0.3) +
  labs(title = "Lezen (Module A), least proficient students",
       x = "Difference between true and estimated ability")


####Medium achieving students
medium_proficient_L <- list()

for(i in 16:27){
  medium_proficient_L[[i]] <- students_abilities_L_split[[i]]
}


# 2 mistakes 

two_mistakes_L <- c()

for(i in 16:27){
  two_mistakes_L[i] <- mean(medium_proficient_L[[i]]$`2`$diff)
}

two_mistakes_L <- na.omit(two_mistakes_L)

# one mistake

one_mistake_L <- c()

for(i in 16:27){
  one_mistake_L[i] <- mean(medium_proficient_L[[i]]$`1`$diff)
}

one_mistake_L <- na.omit(one_mistake_L)
#Combine in one DF

n.errors <- rep(1:2, each = 12)
theta.diff <- c(one_mistake_L, two_mistakes_L)
df.medium_L <- data.frame(theta.diff, n.errors)
df.medium_L$n.errors <- as.factor(df.medium_L$n.errors)

#Plot

ggplot(df.medium_L, aes(x = theta.diff, fill = n.errors)) +  
  geom_density(alpha=0.4)+ 
  xlim(-0.1, 0.5) +
  labs(title = "Lezen (Module A), medium proficient students",
       x = "Difference between true and estimated ability")




##### High achieving students

most_proficient_L <- list()

  for(i in 27:41){
    most_proficient_L[[i]] <- students_abilities_L_split[[i]]
  }


# 0 mistakes 

zero_mistakes_L <- c()

  for(i in 27:41){
    zero_mistakes_L[i] <- mean(most_proficient_L[[i]]$`0`$diff)
  }

zero_mistakes_L <- na.omit(zero_mistakes_L)
# one mistake

one_mistake_L <- c()

  for(i in 27:41){
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
  geom_density(alpha=0.4)+ 
  xlim(-0.1, 0.5) +
  labs(title = "Lezen (Module A), most proficient students",
       x = "Difference between true and estimated ability")

#Conclusion: Discrepancies are higher with the most proficient students
#++++++++++++++++++++++++++
#       Rekenen
#+++++++++++++++++++++++++

# Calculate the differnece
  for (i in 1:length(true.theta_R)){
    students_abilities_R[[i]]$diff <-  abs(students_abilities_R[[i]]$true_theta - students_abilities_R[[i]]$theta)
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
  geom_density(alpha=0.4)+ 
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

