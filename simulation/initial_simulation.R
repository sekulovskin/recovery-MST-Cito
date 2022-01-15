#===========================================================
# Initial simulation
#==========================================================
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship")
library(dexterMST)
library(dplyr)
options(warn = 1) #get rid of the warnings 

#As discussed in the email I will use the provided items parameters,
#routing rules and test design from ACET.
test_design <- read.csv('dexter_mst_design_dov.csv')
routing_rules <- read.csv('dexter_mst_rules_dov.csv')
pars <- read.csv('parameters_dov.csv')

#============================================
# For the reading test (Lezen)
#===========================================

#Filter only with respect to the reading test
test_design_L <-test_design[grepl('^L', test_design$module_id), ]
routing_rules_L <- routing_rules[grepl('^L', routing_rules$module_id), ]
pars_L <- pars[pars[, 'item_id'] %in% test_design_L$item_id, ]

#construct the true reading latent ability. Note for now I assume that theta ranges roughly from
# -1.5 to 1.5 with an 0.1 increase. This results in 31 students
true.theta_L <- seq(-1.5, 1.5, 0.1)
#----------------------------------------------------
#Simulate response data and respective abilities n_sim number of times
n_sim <- 100 # We need to talk about how big this should be!

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

#store each student in a separate data frame
students_L <- list()
 for(i in 1:length(true.theta_L)){
   students_L[[i]] <- abilities_L %>%
  filter(person_id == i)
}

## Repeat all of this for the other subjects that follor an MST design (Rekenen en Taal)


#============================================
# For the Math test (Rekenen)
#===========================================

#Filter only with respect to the Math test
test_design_R <-test_design[grepl('^R', test_design$module_id), ]
routing_rules_R <- routing_rules[grepl('^R', routing_rules$module_id), ]
pars_R <- pars[pars[, 'item_id'] %in% test_design_R$item_id, ]

# Maybe I should change these theta values to be slightly different for each test
true.theta_R <- seq(-1.5, 1.5, 0.1) 
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
students_R <- list()
for(i in 1:length(true.theta_R)){
  students_R[[i]] <- abilities_R %>%
    filter(person_id == i)
}

#============================================
# For the Language test (Taal)
#===========================================

#Filter only with respect to the Language test
test_design_T <-test_design[grepl('^T', test_design$module_id), ]
routing_rules_T <- routing_rules[grepl('^T', routing_rules$module_id), ]
pars_T <- pars[pars[, 'item_id'] %in% test_design_T$item_id, ]

# Maybe I should change these theta values to be slightly different for each test
true.theta_T <- seq(-1.5, 1.5, 0.1) 
#----------------------------------------------------

#Construct "storage space"
patterns_T <- list()
abilities_T <- list()

#loop over n_sim number of times to obtain different response patterns
for(i in 1:n_sim){
  patterns_T[[i]] <- dexterMST::sim_mst(pars_T, true.theta_T, test_design_T, routing_rules_T, routing = 'all')
  abilities_T[[i]] <- dexter::ability(patterns_T[[i]], parms = pars_T, method = 'WLE')
  abilities_T[[i]]$true_theta <- true.theta_T #The WLE was proposed by Warm (1989) to reduce bias in the MLE and is also known as the Warm estimator
}

#--------------------------------------------------

#Combine it in a data frame
abilities_T <- do.call(rbind, abilities_T)

#store each student in a separate data frame
students_T <- list()
for(i in 1:length(true.theta_T)){
  students_T[[i]] <- abilities_T %>%
    filter(person_id == i)
}




#The next steps should be finding for which student (true ability) there is most variability
#and then incpect the patterns
#Think about the EARLY MISTAKES PART!!

