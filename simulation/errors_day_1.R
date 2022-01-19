#==================================================
# Number of errors (On the first day!!)
#=================================================
library(dplyr)
#==============================================================================
#Lezen
#=============================================================================
#Look at which items are administered in day 1
#I will do it seperately just in case
test_design_L %>%
  filter(module_id == "LDA1")
test_design_L %>%
  filter(module_id == "LDB1")
test_design_L %>%
  filter(module_id == "LDC1")
test_design_L %>%
  filter(module_id == "LDD1")
test_design_L %>%
  filter(module_id == "LDE1")
test_design_L %>%
  filter(module_id == "LDF1")


# Filter only these items:

# for each student there are 600 items in total: since we have 6 items in module A and 100 repeated measurements
#So we can add a factor variable `repeat` indicating from which measurement does a particular item come from

for( i in 1:length(true.theta_L)){
  patterns_module_A_L[[i]]$occ <- rep(1:100, each = 6)
}

#Add a variable indicating whether there is an error

for( i in 1:length(true.theta_L)){
  patterns_module_A_L[[i]]$error <- ifelse(patterns_module_A_L[[i]]$item_score == 0, 1, 0)
}

#Exclude all other variables

for(i in 1:length(true.theta_L)){
  
  patterns_module_A_L[[i]] <- patterns_module_A_L[[i]] %>%
    select(occ, error)
}

# Separate each measurement in a separate data frame (or in this case tibble)
errors_module_A_L <- list()
for(i in 1:length(true.theta_L)){
  errors_module_A_L[[i]] <- split(patterns_module_A_L[[i]], patterns_module_A_L[[i]]$occ)
}

#Calculate the sum of errors for each repeated measurement across all students
n_errors_module_A_L <-list()
for(i in 1:length(true.theta_L)){
  n_errors_module_A_L[[i]] <- lapply(errors_module_A_L[[i]], function(x){sum(x$error)})
}

#Unlist
for(i in 1:length(true.theta_L)){
  n_errors_module_A_L[[i]] <- unlist(n_errors_module_A_L[i])
}

#put in separate once columnt (df) i.e., vector
for(i in 1:length(true.theta_L)){
  n_errors_module_A_L[i] <- as.data.frame(n_errors_module_A_L[i])
}

#Now combine it with each repeated theta re-estimate
for(i in 1:length(true.theta_L)){
  students_abilities_L[[i]]$errors <- n_errors_module_A_L[[i]]
}

# Remove unnecessary elements that we will not use anymore 
rm(errors_module_A_L, n_errors_module_A_L, patterns_module_A_L, students_patterns_L)

#------------------------------------------------------
#Analyses (repeating the same analyses as in `initial_analyses.R`,
#but while controlling for the error)
#------------------------------------------------------

#We will only look at the situations where significant differences appeared in the
#initial analyses, namely for students with theta 1.3 and 1.2 i.e., students 41 and 39

student_41_L <- students_abilities_L[[41]]
student_39_L <- students_abilities_L[[39]]
student_41_L$errors <- as.factor(student_41_L$errors)
student_39_L$errors <- as.factor(student_39_L$errors)
unique(student_41_L$errors)
unique(student_39_L$errors)  #in both cases there are only 0 and 1

#--------------------------------------------------------------------------------
# First we will see whether there is a significant difference between the average 
#estimated thetas, for instances when having different errors on module A

# For true_theta = 1.3
t.test(theta ~ errors, data = student_41_L)
#t = 4.6781, df = 12.251, p-value = 0.0005054
#mean in group 0 mean in group 1 
#1.2112101       0.9532306 
# There is a significant difference between situations where the student made 1 mistake,
#as opposed to a situation where the student didn't make a mistake. With the average
# re-estimated theta of 1.21 for students with no mistake.
#---------------------------
# For true_theta = 1.2
#t = 6.3959, df = 22.338, p-value = 1.818e-06
#mean in group 0 mean in group 1 
#1.1981191       0.8978614 
# There is a significant difference between situations where the student made 1 mistake,
#as opposed to a situation where the student didn't make a mistake. With the average
# re-estimated theta of 1.19 (very close to the true theta) for students with no mistake.
#----------------------------------------------------------------------------------

#Secondly, we will repeat the same analysis as in the `initia_analyses.R` script
#In order to do this we will need to split the data (NOTE THIS ANALYSIS IS NOT
#AS INFORMATIVE AS THE PREVIOUS, DUE TO VERY UNEQUAL SAMPLE SIZES (REPEATED MEASUREMENTS) 
#IN THE SPLIT DATA SETS)

# For true_theta = 1.3
student_41_L_split <- split(student_41_L, student_41_L$errors)
# First for 0 errors
t.test(student_41_L_split[[1]]$theta, student_41_L_split[[1]]$true_theta)
#t = -3.2382, df = 91, p-value = 0.001679
# mean of x mean of y 
#1.21121   1.30000 

#Secondly, for 1 error
t.test(student_41_L_split[[2]]$theta, student_41_L_split[[2]]$true_theta)
#t = -7.2476, df = 7, p-value = 0.0001703
#mean of x mean of y 
#0.9532306 1.3000000

#Conclusion: Even though the size is to small, to say anything about the p-vales
#and even though in both situations, we had a significant p-value,
#it is evident by looking at the means of both situations that there is a way bigger
#discrepancy between the true and the r-restimated averge theta in the case where
#the student makes 1 mistake as opposed to no mistakes in the first module (A)

# For true_theta = 1.2
student_39_L_split <- split(student_39_L, student_39_L$errors)

# First for 0 errors
t.test(student_39_L_split[[1]]$theta, student_39_L_split[[1]]$true_theta)
#t = -0.068624, df = 88, p-value = 0.9454
# mean of x mean of y 
#1.198119  1.200000 

# No significant difference, and also very small difference when observing the means!

#Secondly, for 1 error
t.test(student_39_L_split[[2]]$theta, student_39_L_split[[2]]$true_theta)
#t = -7.9273, df = 10, p-value = 1.275e-05
#mean of x mean of y 
#0.8978614 1.2000000 

#OVERALL CONCLUSION: The number of mistakes in the first module of the reading test,
#for students having a high "true" theta value (in this case 1.3 and 1.2), influence the
#final theta estimate. These significant differences were observed both when
#testing whether there is a difference between the average *ESTIMATED* theta
#between situations when a student made 0 mistakes and 1 mistake in module 1;
#ANd also in the situation when controlling for the number of mistakes and
#measuring whether there is a significant difference between the average 
#ESTIMATED theta and the TRUE theta!

#===============================================================================
#Rekenen
#==============================================================================
#Look at which items are in module A

test_design_R %>%
  filter(module_id == "RDA1")

#module_id  item_id item_position
#1      RDA1 RD516087             1
#2      RDA1 RD516456             2
#3      RDA1 RD516A28             3
#4      RDA1 RD516663             4
#5      RDA1 RD516375             5

# Filter only these items:
patterns_module_A_R <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == c("RD516087", "RD516456", "RD516A28", "RD516663", "RD516375")) 
}

#PROBLEM: EACH ITEM IS PRESENT 20 TIMES INSTEAD OF A 100!!

#students_patterns_L[[1]] %>%
#filter(item_id == c("BL00518", "BL00519", "BL00520", "BL00521", "BL02412",  "OP00142"))

#students_patterns_R[[1]] %>%
#filter(item_id == c("RD516087", "RD516456", "RD516A28", "RD516663", "RD516375"))

for(i in 1:length(true.theta_R)){
  patterns_module_A_R[[i]]$occ <- rep(1:20, each = 5)
}

#Add a variable indicating whether there is an error

for( i in 1:length(true.theta_R)){
  patterns_module_A_R[[i]]$error <- ifelse(patterns_module_A_R[[i]]$item_score == 0, 1, 0)
}

#Exclude all other variables

for(i in 1:length(true.theta_R)){
  
  patterns_module_A_R[[i]] <- patterns_module_A_R[[i]] %>%
    select(occ, error)
}

# Separate each measurement in a separate data frame
errors_module_A_R <- list()
for(i in 1:length(true.theta_R)){
  errors_module_A_R[[i]] <- split(patterns_module_A_R[[i]], patterns_module_A_R[[i]]$occ)
}

#convert to data frames
n_errors_module_A_R <-list()
for(i in 1:length(true.theta_R)){
  n_errors_module_A_R[[i]] <- lapply(errors_module_A_R[[i]], function(x){sum(x$error)})
}

#Unlist
for(i in 1:length(true.theta_R)){
  n_errors_module_A_R[[i]] <- unlist(n_errors_module_A_R[i])
}

#put in separate vector
for(i in 1:length(true.theta_R)){
  n_errors_module_A_R[i] <- as.data.frame(n_errors_module_A_R[i])
}

#Now combine it with each repeated theta re-estimate
for(i in 1:length(true.theta_R)){
  students_abilities_R[[i]]$errors <- n_errors_module_A_R[[i]]
}

# Remove unnecessary elements that we will not use anymore 
rm(errors_module_A_R, n_errors_module_A_R, patterns_module_A_R, students_patterns_R)


#===============================================================================
#Taal
#==============================================================================
#Look at which items are in module A

test_design_T %>%
  filter(module_id == "TDA1")

#module_id  item_id item_position
#1      TDA1 SN00003D             1
#2      TDA1  SN00038             2
#3      TDA1 SW00090D             3
#4      TDA1  IP00074             4
#5      TDA1  GR00148             5

# Filter only these items:
patterns_module_A_T <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == c("SN00003D", "SN00038", "SW00090D", "IP00074", "GR00148")) 
}

# for each student there are 500 items in total: since we have 5 items in module A and 100 repeated measurements
#So we can add a factor variable `repeat` indicating from which measurement does a particular item come from

for(i in 1:length(true.theta_R)){
  patterns_module_A_R[[i]]$occ <- rep(1:100, each = 5)
}

#Add a variable indicating whether there is an error

for( i in 1:length(true.theta_L)){
  patterns_module_A_L[[i]]$error <- ifelse(patterns_module_A_L[[i]]$item_score == 0, 1, 0)
}

#Exclude all other variables

for(i in 1:length(true.theta_L)){
  
  patterns_module_A_L[[i]] <- patterns_module_A_L[[i]] %>%
    select(occ, error)
}

# Separate each measurement in a separate data frame
errors_module_A_L <- list()
for(i in 1:length(true.theta_L)){
  errors_module_A_L[[i]] <- split(patterns_module_A_L[[i]], patterns_module_A_L[[i]]$occ)
}

#convert to data frames
n_errors_module_A_L <-list()
for(i in 1:length(true.theta_L)){
  n_errors_module_A_L[[i]] <- lapply(errors_module_A_L[[i]], function(x){sum(x$error)})
}

#Unlist
for(i in 1:length(true.theta_L)){
  n_errors_module_A_L[[i]] <- unlist(n_errors_module_A_L[i])
}

#put in separate vector
for(i in 1:length(true.theta_L)){
  n_errors_module_A_L[i] <- as.data.frame(n_errors_module_A_L[i])
}

#Now combine it with each repeated theta re-estimate
for(i in 1:length(true.theta_L)){
  students_abilities_L[[i]]$errors <- n_errors_module_A_L[[i]]
}

# Remove unnecessary elements that we will not use anymore 
rm(errors_module_A_L, n_errors_module_A_L, patterns_module_A_L, students_patterns_L)



