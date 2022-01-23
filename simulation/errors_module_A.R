#==================================================
# Number of errors (on first  module only!!)
#=================================================
library(dplyr)
#==============================================================================
#Lezen
#=============================================================================
#Look at which items are in module A

test_design_L %>%
  filter(module_id == "LDA1")

#module_id item_id item_position
#1      LDA1 BL00518             1
#2      LDA1 BL00519             2
#3      LDA1 BL00520             3
#4      LDA1 BL00521             4
#5      LDA1 BL02412             5
#6      LDA1 OP00142             6

# Filter only these items:
patterns_module_A_L <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_A_L[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == c("BL00518", "BL00519", "BL00520", "BL00521", "BL02412",  "OP00142")) 
  
}

#subset(students_patterns_L[[30]], subset = item_id == c("BL00518", "BL00519", "BL00520", "BL00521", "BL02412",  "OP00142"))
#students_patterns_L[[1]] %>%
 # filter(item_id == "BL00518")
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
#patterns_module_A_R <- list()
#for (i in 1:length(true.theta_R)){
#  patterns_module_A_R[[i]] <- students_patterns_R[[i]] %>%
#    filter(item_id == c("RD516087", "RD516456", "RD516A28", "RD516663", "RD516375")) 
#}

#Illustrating the problem:
#subset(students_patterns_R[[30]], subset = item_id == c("RD516087", "RD516456", "RD516A28", "RD516663", "RD516375"))
#x <- students_patterns_R[[1]] %>%
#  filter(item_id == c("RD516087", "RD516456", "RD516A28", "RD516663", "RD516375")) 
#y <- students_patterns_R[[1]] %>%
#  filter(item_id == "RD516456")
#students_patterns_L[[1]] %>%
#filter(item_id == c("BL00518", "BL00519", "BL00520", "BL00521", "BL02412",  "OP00142"))

#students_patterns_R[[1]] %>%
#filter(item_id == c("RD516087", "RD516456", "RD516A28", "RD516663", "RD516375"))

# Since we have this persistent probe=le, we are going to take a detour and ging to repeat it for each item separately
#item 1
patterns_module_A_R.1 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.1[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516087") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.1[[i]]$occ <- seq(1:100)
}

#item 2
patterns_module_A_R.2 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.2[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516456") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.2[[i]]$occ <- seq(1:100)
}

#item 3
patterns_module_A_R.3 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.3[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516A28") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.3[[i]]$occ <- seq(1:100)
}

#item 4
patterns_module_A_R.4 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.4[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516663") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.4[[i]]$occ <- seq(1:100)
}

#item 5
patterns_module_A_R.5 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.5[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516375") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.5[[i]]$occ <- seq(1:100)
}

##NOW combine
patterns_module_A_R <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R[[i]] <- rbind(patterns_module_A_R.1[[i]], patterns_module_A_R.2[[i]], patterns_module_A_R.3[[i]], patterns_module_A_R.4[[i]], patterns_module_A_R.5[[i]]) 
}

rm(patterns_module_A_R.1, patterns_module_A_R.2, patterns_module_A_R.3, patterns_module_A_R.4, patterns_module_A_R.5)
#The rest is the same

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
#patterns_module_A_T <- list()
#for (i in 1:length(true.theta_T)){
#  patterns_module_A_T[[i]] <- students_patterns_T[[i]] %>%
#    filter(item_id == c("SN00003D", "SN00038", "SW00090D", "IP00074", "GR00148")) 
#}


#students_patterns_T[[1]] %>%
#  filter(item_id == "SN00003D") 
patterns_module_A_T.1 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.1[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00003D") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.1[[i]]$occ <- seq(1:100)
}

#item 2
patterns_module_A_T.2 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.2[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00038") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.2[[i]]$occ <- seq(1:100)
}

#item 3
patterns_module_A_T.3 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.3[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW00090D") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.3[[i]]$occ <- seq(1:100)
}

#item 4
patterns_module_A_T.4 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.4[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00074") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.4[[i]]$occ <- seq(1:100)
}

#item 5
patterns_module_A_T.5 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.5[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00148") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.5[[i]]$occ <- seq(1:100)
}

##NOW combine
patterns_module_A_T <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T[[i]] <- rbind(patterns_module_A_T.1[[i]], patterns_module_A_T.2[[i]], patterns_module_A_T.3[[i]], patterns_module_A_T.4[[i]], patterns_module_A_T.5[[i]]) 
}

rm(patterns_module_A_T.1, patterns_module_A_T.2, patterns_module_A_T.3, patterns_module_A_T.4, patterns_module_A_T.5)


#the rest is the same
#add a variable indicating whether there is an error

for( i in 1:length(true.theta_T)){
  patterns_module_A_T[[i]]$error <- ifelse(patterns_module_A_T[[i]]$item_score == 0, 1, 0)
}

#Exclude all other variables

for(i in 1:length(true.theta_T)){
  
  patterns_module_A_T[[i]] <- patterns_module_A_T[[i]] %>%
    select(occ, error)
}

# Separate each measurement in a separate data frame
errors_module_A_T <- list()
for(i in 1:length(true.theta_T)){
  errors_module_A_T[[i]] <- split(patterns_module_A_T[[i]], patterns_module_A_T[[i]]$occ)
}

#convert to data frames
n_errors_module_A_T <-list()
for(i in 1:length(true.theta_T)){
  n_errors_module_A_T[[i]] <- lapply(errors_module_A_T[[i]], function(x){sum(x$error)})
}

#Unlist
for(i in 1:length(true.theta_T)){
  n_errors_module_A_T[[i]] <- unlist(n_errors_module_A_T[i])
}

#put in separate vector
for(i in 1:length(true.theta_T)){
  n_errors_module_A_T[i] <- as.data.frame(n_errors_module_A_T[i])
}

#Now combine it with each repeated theta re-estimate
for(i in 1:length(true.theta_T)){
  students_abilities_T[[i]]$errors <- n_errors_module_A_T[[i]]
}

# Remove unnecessary elements that we will not use anymore 
rm(errors_module_A_T, n_errors_module_A_T, patterns_module_A_T, students_patterns_T)


#========================================================================================
#ANALYSES(repeating the same analyses as in `initial_analyses.R`,
#but while controlling for the error)
#=======================================================================================

#------------------------------------------------------
# LEZEN
#------------------------------------------------------

#We will only look at the situations where significant differences appeared in the
#initial analyses (based on more repetitions of the simulation!!!!), 
#namely for students with theta 1.3 and 1.2 i.e., students 41 and 39

#Extract these students in separate data frames
student_41_L <- students_abilities_L[[41]]
student_39_L <- students_abilities_L[[39]]
#Transform the `errors` variable into a factor
student_41_L$errors <- as.factor(student_41_L$errors)
student_39_L$errors <- as.factor(student_39_L$errors)
#check how many different number of errors are there
unique(student_41_L$errors)
unique(student_39_L$errors)  #in both cases there are only 0 and 1

#--------------------------------------------------------------------------------
# First we will see whether there is a significant difference between the average 
#estimated thetas, for instances when having different errors on module A
#NOTE we are doing a Welch test, since there are only two groups (zero mistakes vs one mistake, this is due to the fact
#that these students have a high true theta)
# For true_theta = 1.3
t.test(theta ~ errors, data = student_41_L)
#t = 3.3965, df = 7.6199, p-value = 0.01012
#mean in group 0 mean in group 1 
#1.247428        1.108579
# There is a significant difference between situations where the student made 1 mistake,
#as opposed to a situation where the student didn't make a mistake. With the average
# re-estimated theta of 1.24 for students with no mistake.
#---------------------------
# For true_theta = 1.2
t.test(theta ~ errors, data = student_39_L)
#t = 5.2064, df = 4.7933, p-value = 0.003898
#mean in group 0 mean in group 1 
#1.2008832       0.9202921  
# There is a significant difference between situations where the student made 1 mistake,
#as opposed to a situation where the student didn't make a mistake. With the average
# re-estimated theta of 1.2 (very close to the true theta on average) for students with no mistake.
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
#discrepancy between the true and the r-estimated average theta in the case where
#the student makes 1 mistake as opposed to no mistakes in the first module (A)

# For true_theta = 1.2
student_39_L_split <- split(student_39_L, student_39_L$errors)

# First for 0 errors
t.test(student_39_L_split[[1]]$theta, student_39_L_split[[1]]$true_theta)
#t = 0.035781, df = 95, p-value = 0.9715
# mean of x mean of y 
#1.200883  1.200000    #NO DIFFERENCE

# No significant difference, and also very small difference when observing the means!

#Secondly, for 1 error
t.test(student_39_L_split[[2]]$theta, student_39_L_split[[2]]$true_theta)
#t = -5.8383, df = 3, p-value = 0.01001
#mean of x mean of y 
#0.9202921 1.2000000

#OVERALL CONCLUSION: The number of mistakes in the first module of the reading test,
#for students having a high "true" theta value (in this case 1.3 and 1.2), influence the
#final theta estimate. These significant differences were observed both when
#testing whether there is a difference between the average *ESTIMATED* theta
#between situations when a student made 0 mistakes and 1 mistake in module 1;
#AND also in the situation when controlling for the number of mistakes and
#measuring whether there is a significant difference between the average 
#ESTIMATED theta and the TRUE theta!


#------------------------------------------------------
# REKENEN
#------------------------------------------------------

#We will only look at the situations where significant differences appeared in the
#initial analyses, namely for students with tue theta 1.8; 1.75; 1.7; 1.65; 0.25; -1.2; -1.25; -1.3
#ie., students: 63, 62, 61, 60, 32(?), 3, 2, 1
#Extract these students in separate data frames
student_63_R <- students_abilities_R[[63]]
student_62_R <- students_abilities_R[[62]]
student_61_R <- students_abilities_R[[61]]
student_60_R <- students_abilities_R[[60]]
student_32_R <- students_abilities_R[[32]]
student_3_R <- students_abilities_R[[3]]
student_2_R <- students_abilities_R[[2]]
student_1_R <- students_abilities_R[[1]]
#Transform the variable `errors` into a factor
student_63_R$errors <- as.factor(student_63_R$errors)
student_62_R$errors <- as.factor(student_62_R$errors)
student_61_R$errors <- as.factor(student_61_R$errors)
student_60_R$errors <- as.factor(student_60_R$errors)
student_32_R$errors <- as.factor(student_32_R$errors)
student_3_R$errors <- as.factor(student_3_R$errors)
student_2_R$errors <- as.factor(student_2_R$errors)
student_1_R$errors <- as.factor(student_1_R$errors)

#Now check how many different number of errors are there and how many students within each error category 
unique(student_63_R$errors)  #0:98 ,1:2   
unique(student_62_R$errors) #0:99 ,1:1
unique(student_61_R$errors) #0:98,1:2
unique(student_60_R$errors) # No mistakes, only 0
unique(student_32_R$errors) # 0:30, 1:48, 2:19, 3:3
unique(student_3_R$errors)  # 4:12, 5:88
unique(student_2_R$errors)  # 4:8,5:92
unique(student_1_R$errors)  # 3:1, 4:15, 5:84

#--------------------------------------------------------------------------------
# First we will see whether there is a significant difference between the average 
#estimated thetas, for instances when having different number of errors on module A

#NOTE WE DO NOT ONLY LOOK AT THE P-VALUE (and don't take it too seriously) SINCE THERE IS A HUGE DISCREPANCY IN SIZE (BUT MORE AT THE AVERAGE ESTIMATES)
# FOR THETA = 1.8
t.test(theta ~ errors, data = student_63_R)
#t = 11.129, df = 2.3426, p-value = 0.004428
#mean in group 0 mean in group 1 
# 1.697439        1.147979 
#There is a difference as expected

# FOR THETA = 1.75  (here we will only look at the difference, without doing a t-test)
student_62_R %>%
  filter(errors==1)

student_62_R %>%
  filter(errors==0) %>%
  summary()
#mean in group 0 mean in group 1 
# 1.678        1.3485
#There is a difference as expected

# FOR THETA = 1.7
t.test(theta ~ errors, data = student_61_R)
#t = 1.7353, df = 1.0583, p-value = 0.3224
#mean in group 0 mean in group 1 
#1.648288        1.327997 
#There is still a discrepancy  (when looking at the means)

# FOR THETA = 1.65
# We skip this, since there are no mistakes 

#FOR THETA = 0.25  (we do an ANOVA since there are more than 2 categories)
anova <- aov(theta ~ errors, data = student_32_R)
summary(anova)
# Df Sum Sq  Mean Sq F value Pr(>F)  
#errors       3 0.0426 0.014193   2.421 0.0707 
#There is no "significant" differences, but let's look at the means per error level
student_32_R %>%
  filter(errors==0) %>%
  summary()          #0.2777   #In the first case we have a slight OVERESTIMATE of theta!! (LOOK INTO THIS FOR MORE STUDENTS)

student_32_R %>%
  filter(errors==1) %>%
  summary()      #0.23764 


student_32_R %>%
  filter(errors==2) %>%
  summary()      #0.2255

student_32_R %>%
  filter(errors==3) %>%
  summary()     #0.2258   #So in general, for a medium theta, there is no big difference

#FOR THETA -1.2
t.test(theta ~ errors, data = student_3_R)
#t = 9.1856, df = 47.147, p-value = 4.482e-12
#mean in group 4 mean in group 5 
#-0.9325235      -1.179111


#FOR THETA -1.25
t.test(theta ~ errors, data = student_2_R)
#t = 5.285, df = 9.8939, p-value = 0.0003679
#mean in group 4 mean in group 5 
#-0.9859082      -1.2427683

#For both these, it is obvious that the higher the number of errors, the closer the average estimate to the true theta

#FOR THETA -1.3
#since there is only 1 instance for theta = 3 we will filter it out
student_1_R %>%
  filter(errors !=3) %>%
  t.test(theta ~ errors, data = .)

#t = 6.9549, df = 31.718, p-value = 7.409e-08
#mean in group 4 mean in group 5 
#-1.024322       -1.265712   #same conclusion as previously  
student_1_R %>%
  filter(errors ==3) %>%
  summary()  #theta = -0.8662, 

#same conclusion as previously

#SECONDLY WE WILL LOOK AT WHETHER THERE ARE DIFFERENCES BETWEEN THE TRUE AND THE ESTIMATED THETA WHEN CONTROLLING FOR THE ERRORS
# FOR THETA = 1.8
student_63_R_split <- split(student_63_R, student_63_R$errors)

# For 0 errors
t.test(student_63_R_split[[1]]$theta, student_63_R_split[[1]]$true_theta)
#t = -3.5235, df = 97, p-value = 0.0006515
#1.697439  1.800000  (not a very big difference, eveen though "significant")

#For 1 error (we will not do a test, only look at values, since there are only 2 obs)
summary(student_63_R_split[[2]])
#avg.theta: 1.148  (1.8 - 1.148 = 0.652 )
#We can conclude that the discrepancy is way higher when having one error as opposed to 0 erros

# FOR THETA = 1.75
student_62_R_split <- split(student_62_R, student_62_R$errors)

# For 0 errors
t.test(student_62_R_split[[1]]$theta, student_62_R_split[[1]]$true_theta)
#t = -2.5462, df = 98, p-value = 0.01245
#1.678359  1.750000   (not a very big difference)

#For 1 error (we will not do a test, only look at values, since there is only 1 obs)
summary(student_62_R_split[[2]])
#avg.theta: 1.349 (1.75 - 1.349 = 0.401)
#We can conclude that the discrepancy is way higher when having one error as opposed to 0 erros (but not as high as with the 
#student having "true" theta = 1.8)


# FOR THETA = 1.7
student_61_R_split <- split(student_61_R, student_61_R$errors)

# For 0 errors
t.test(student_61_R_split[[1]]$theta, student_61_R_split[[1]]$true_theta)
#t = -1.6758, df = 97, p-value = 0.097
#1.648288  1.700000    (not a very big difference)

#For 1 error (we will not do a test, only look at values, since there are only 2 obs)
summary(student_61_R_split[[2]])
#avg.theta: 1.328 (1.7 - 1.328 = 0.372 )
#We can conclude that the discrepancy is way higher when having one error as opposed to 0 erros (but not as high as with the 
#student having "true" theta = 1.8 and 1,75)
#OBSERVATIIN: so as true theta get's smaller, the difference between the true and the (average) estimates
#theta get's also smaller as the number of mistakes increases. Clearly proving, once more, that these mismatches.
#tend to appear on the upper extreme

#FOR THETA = 1.65
#NO NEED FOR ANALYSES SINCE WE HAVE NO ERRORS

#FOR THETA = 0.25  (FOUR SITUATIONS)
student_32_R_split <- split(student_32_R, student_32_R$errors)

#FOR 0 errors
t.test(student_32_R_split[[1]]$theta, student_32_R_split[[1]]$true_theta)
#t = 2.0054, df = 29, p-value = 0.05434
#0.2776954 0.2500000  (AGAIN AN OVERESTIMATE OF THE TRUE THETA)

#fOR 1 error
t.test(student_32_R_split[[2]]$theta, student_32_R_split[[2]]$true_theta)
#t = -1.0614, df = 47, p-value = 0.2939
#mean of x mean of y    (no big difference, and no no overestimate)
#0.2376425 0.2500000 

#for 2 errors

t.test(student_32_R_split[[3]]$theta, student_32_R_split[[3]]$true_theta)
#t = -1.5343, df = 18, p-value = 0.1423
#mean of x mean of y    (no big difference, and no no overestimate)
#0.2255281 0.2500000  (slightly bigger, but as expected) 

#for 3 errors
t.test(student_32_R_split[[4]]$theta, student_32_R_split[[4]]$true_theta) #COMPLETELY IGNORING THE P-VALUE
#t = -0.93816, df = 2, p-value = 0.4472
#mean of x mean of y 
#0.2257963 0.2500000 (THE DIFFERENCE DEOSN;T GET VERY BIGGER AS the number of errors increase)

#CONCLUSION: At the middle of the 'true' theta continuum there seems to be an overestimate of thew ability
#when the student doesn't make any mistakes, as opposed to the extremes.

#FOR THETA = -1.2
student_3_R_split <- split(student_3_R, student_3_R$errors)

# For 4 errors
t.test(student_3_R_split[[1]]$theta, student_3_R_split[[1]]$true_theta)
#t = 15.019, df = 11, p-value = 1.124e-0
#-0.9325235 -1.200000    (there is a difference, ignoring the p-value)

#For 5 errors
t.test(student_3_R_split[[2]]$theta, student_3_R_split[[2]]$true_theta)
#t = 1.0399, df = 87, p-value = 0.3
#-1.179111 -1.200000    (very small difference, as expected)

#FOR THETA = -1.25
student_2_R_split <- split(student_2_R, student_2_R$errors)

# For 4 errors
t.test(student_2_R_split[[1]]$theta, student_2_R_split[[1]]$true_theta)
#t = 5.9289, df = 7, p-value = 0.0005823
#-0.9859082 -1.2500000  (non-negiligable difference)

#For 5 errors
t.test(student_2_R_split[[2]]$theta, student_2_R_split[[2]]$true_theta)
#t = 0.37195, df = 91, p-value = 0.7108
#-1.242768 -1.250000     (very small difference, as expected)

#FOR THETA = -1.3
student_1_R_split <- split(student_1_R, student_1_R$errors)

#for 1 error
summary(student_1_R_split[[1]])

#-0.866 - 1.3  a big difference of 0.434 (as expected)

# For 4 errors
t.test(student_1_R_split[[2]]$theta, student_1_R_split[[2]]$true_theta)
#t = 9.8633, df = 14, p-value = 1.108e-07
#-1.024322 -1.300000 (not a small difference)

#For 5 errors
t.test(student_1_R_split[[3]]$theta, student_1_R_split[[3]]$true_theta)
#t = 1.6663, df = 83, p-value = 0.09943
#-1.265712 -1.300000  (not so small of a difference)

#OVERALL CONCLUSUON: WE USUALLY HAVE AN OVER-UNDER ESTIMATE OF THETA, ON THE POSITIVE-NEGATIVE EXTREMES
#OF THE ABILITY CONTINUUM. And we have a slight over-estimate od theta in the middle of the continuum


#------------------------------------------------------
# Taal
#------------------------------------------------------
# Only for student with "true" ability of 1.35 (i.e., student 49)

student_49_T <- students_abilities_T[[49]]
student_49_T$errors <- as.factor(student_49_T$errors)
summary(student_49_T)  # 0:93; 1:5; 2:2

# First we will see whether there is a significant difference between the average 
#estimated thetas, for instances when having different errors on module A

anova <- aov(student_49_T$theta ~ student_49_T$errors)
summary(anova)
#                    Df Sum Sq Mean Sq F value Pr(>F)
#student_49_T$errors  2  0.160 0.08010   1.124  0.329

student_49_T %>%
  filter(errors == 0) %>%
  summary()  #1.307

student_49_T %>%
  filter(errors == 1) %>%
  summary()  #1.165

student_49_T %>%
  filter(errors == 2) %>%
  summary()  #1.120

#There tends to be an underestimate (as seen wirth the previous subjects)

#SECONDLY WE WILL LOOK AT WHETHER THERE ARE DIFFERENCES BETWEEN THE TRUE AND THE ESTIMATED THETA WHEN CONTROLLING FOR THE ERRORS
student_49_T_split <- split(student_49_T, student_49_T$errors)

# For 0 errors
t.test(student_49_T_split[[1]]$theta, student_49_T_split[[1]]$true_theta)
#t = -1.502, df = 92, p-value = 0.1365
#1.307468  1.350000   (there is a difference even after controlling for the number of errors!!!!!!)

# For 1 error
t.test(student_49_T_split[[2]]$theta, student_49_T_split[[2]]$true_theta) #(Not even looking at the p-value)
#mean of x mean of y 
#1.16496   1.35000   (the differene increases by a lot wheb the the number of errors incereases)
#For 2 errors

summary(student_49_T_split[[2]])
#1.165 vs 1.35 (a smaller difference actually)

#OVERALL CONCLUSION: BIG DIFFERENCES TEND TO BE PRESENT EVEN AFTER CONTROLLING FOR THE NUMBER OF MISTAKES.

#========================================================================================
#Repeat the initial analyses for ALL student, controlling for the number of errors 
#=====================================================================================

#For Lezen

#split each student by the number of errors
students_abilities_L_split <- list()
for(i in 1:length(true.theta_L)){
  students_abilities_L_split[[i]] <-  split(students_abilities_L[[i]], students_abilities_L[[i]]$error)
}

summaries_L <- list()
for(i in 1:length(true.theta_L)){
  summaries_L[[i]] <- lapply(students_abilities_L_split[[i]], summary)
}

#For Rekenen

#split each student by the number of errors
students_abilities_R_split <- list()
for(i in 1:length(true.theta_R)){
  students_abilities_R_split[[i]] <-  split(students_abilities_R[[i]], students_abilities_R[[i]]$error)
}

summaries_R <- list()
for(i in 1:length(true.theta_R)){
  summaries_R[[i]] <- lapply(students_abilities_R_split[[i]], summary)
}

#For Taal

#split each student by the number of errors
students_abilities_T_split <- list()
for(i in 1:length(true.theta_T)){
  students_abilities_T_split[[i]] <-  split(students_abilities_T[[i]], students_abilities_T[[i]]$error)
}

summaries_R <- list()
for(i in 1:length(true.theta_R)){
  summaries_R[[i]] <- lapply(students_abilities_R_split[[i]], summary)
}

#Conclusions will follow