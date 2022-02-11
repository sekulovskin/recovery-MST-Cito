#==================================================
# Number of errors (on first  module only!!)
#=================================================
library(dplyr)
#==============================================================================
#Lezen
#=============================================================================
#Look at which items are in module A
n_sim <- nrow(students_abilities_L[[1]])
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


for( i in 1:length(true.theta_L)){
  patterns_module_A_L[[i]]$occ <- rep(1:n_sim, each = 6)
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
rm(errors_module_A_L, n_errors_module_A_L)


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


#item 1
patterns_module_A_R.1 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.1[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516087") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.1[[i]]$occ <- seq(1:n_sim)
}

#item 2
patterns_module_A_R.2 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.2[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516456") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.2[[i]]$occ <- seq(1:n_sim)
}

#item 3
patterns_module_A_R.3 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.3[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516A28") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.3[[i]]$occ <- seq(1:n_sim)
}

#item 4
patterns_module_A_R.4 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.4[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516663") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.4[[i]]$occ <- seq(1:n_sim)
}

#item 5
patterns_module_A_R.5 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.5[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516375") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.5[[i]]$occ <- seq(1:n_sim)
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
rm(errors_module_A_R, n_errors_module_A_R)


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

 
patterns_module_A_T.1 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.1[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00003D") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.1[[i]]$occ <- seq(1:n_sim)
}

#item 2
patterns_module_A_T.2 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.2[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00038") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.2[[i]]$occ <- seq(1:n_sim)
}

#item 3
patterns_module_A_T.3 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.3[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW00090D") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.3[[i]]$occ <- seq(1:n_sim)
}

#item 4
patterns_module_A_T.4 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.4[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00074") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.4[[i]]$occ <- seq(1:n_sim)
}

#item 5
patterns_module_A_T.5 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.5[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00148") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.5[[i]]$occ <- seq(1:n_sim)
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
rm(errors_module_A_T, n_errors_module_A_T)


#==========================================
#Split with respect to the number of errors
#==========================================

students_abilities_L_split <- list()
for(i in 1:length(true.theta_L)){
  students_abilities_L_split[[i]] <- split(students_abilities_L[[i]], students_abilities_L[[i]]$error)
}

students_abilities_R_split <- list()
for(i in 1:length(true.theta_R)){
  students_abilities_R_split[[i]] <- split(students_abilities_R[[i]], students_abilities_R[[i]]$error)
}

students_abilities_T_split <- list()
for(i in 1:length(true.theta_T)){
  students_abilities_T_split[[i]] <- split(students_abilities_T[[i]], students_abilities_T[[i]]$error)
}