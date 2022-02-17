#=====================================================================
# Initial simulation of response patterns and ability (re)estimates
#====================================================================
library(dexterMST)
library(dplyr)
options(warn = 1) #get rid of the warnings 
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship/repo/mst/simulation")
test_design <- read.csv('dexter_mst_design_dov.csv')
routing_rules <- read.csv('dexter_mst_rules_dov.csv')
pars <- read.csv('parameters_dov.csv')



n_sim <- 1000  #number of simulated responses per true theta (student)
#============================================
# For the reading test (Lezen)
#===========================================
#Filter only with respect to the reading test
test_design_L <-test_design[grepl('^L', test_design$module_id), ]
routing_rules_L <- routing_rules[grepl('^L', routing_rules$module_id), ]
pars_L <- pars[pars[, 'item_id'] %in% test_design_L$item_id, ]
##Based on the observed abilities (see, Simulated-vs-observed-thetas.R)
set.seed(123)
true.theta_L <- rnorm(185, 0.219, 0.236)
#Add the extreme (theoretically possible values)
extremes <- c(-2.54, -1.8, -1.5, -1.3, -1.2, -1, -0.9, -0.8, -0.6, -0.4, 1.2, 1.4, 1.7, 2.2, 3.4)
true.theta_L <- sort(c(true.theta_L, extremes), decreasing = FALSE)
#----------------------------------------------------
#Construct "storage space"
patterns_L <- list()
abilities_L <- list()
#loop over n_sim number of times to obtain different response patterns
  for(i in 1:n_sim){
    patterns_L[[i]] <- dexterMST::sim_mst(pars_L, true.theta_L, test_design_L, routing_rules_L, routing = 'all')
    abilities_L[[i]] <- dexter::ability(patterns_L[[i]], parms = pars_L, method = 'WLE')
    abilities_L[[i]]$true_theta <- true.theta_L 
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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Repeat all of this for the other subjects that follow an MST design (Renee and Taal)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#============================================
# For the Math test (Rekenen)
#===========================================
#Filter only with respect to the Math test
test_design_R <-test_design[grepl('^R', test_design$module_id), ]
routing_rules_R <- routing_rules[grepl('^R', routing_rules$module_id), ]
pars_R <- pars[pars[, 'item_id'] %in% test_design_R$item_id, ]
set.seed(123)
true.theta_R <- sort(rnorm(190, 0.326, 0.389), decreasing = FALSE)
extremes <- c(-1.97, -1.43, -1.17, -0.8, -0.7, -0.6, -0.5, 1.16, 1.38, 1.85)
true.theta_R <- sort(c(true.theta_R, extremes), decreasing = FALSE)
#----------------------------------------------------
#Construct "storage space"
patterns_R <- list()
abilities_R <- list()
#loop over n_sim number of times to obtain different response patterns
  for(i in 1:n_sim){
    patterns_R[[i]] <- dexterMST::sim_mst(pars_R, true.theta_R, test_design_R, routing_rules_R, routing = 'all')
    abilities_R[[i]] <- dexter::ability(patterns_R[[i]], parms = pars_R, method = 'WLE')
    abilities_R[[i]]$true_theta <- true.theta_R 
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
set.seed(123)
true.theta_T <- sort(rnorm(190, 0.273, 0.301), decreasing = FALSE)
extremes <- c(-1.7, -1.3, -1.1, -0.9, -0.7, -0.5, 1.4, 1.6, 1.8, 2.12)
true.theta_T <- sort(c(true.theta_T, extremes), decreasing = FALSE)
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
rm(patterns_T, extremes)


#save.image(file = "initial.simulated.responses.RData")


