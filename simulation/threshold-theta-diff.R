#========================================================
#Threshold theta values for each subject
#======================================================

setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship/repo/mst/simulation")
library(dplyr)
source("ref-score&classification-functions.R")
options(warn = -1)
ACET_2021 <- read.csv("acet_data_2021.csv")

#Now I am going to create the typical student from each class by taking the average
#of the observed classes

ACET_2021 <- ACET_2021 %>%
  arrange(test_score)

# So we have 6 classes and we will randomly sample 50 students from each class

pro_bb <- ACET_2021 %>%
  filter(class == "PRO/VMBO BB") %>%
  select(c(ability_lezen, ability_rekenen, ability_taal, ability_schrijven)) %>%
  summarise_all(funs(mean))

bb_kb <- ACET_2021 %>%
  filter(class == "VMBO BB/KB") %>%
  select(c(ability_lezen, ability_rekenen, ability_taal, ability_schrijven)) %>%
  summarise_all(funs(mean))

kb_gt <- ACET_2021 %>%
  filter(class == "VMBO KB/GL-TL") %>%
  select(c(ability_lezen, ability_rekenen, ability_taal, ability_schrijven)) %>%
  summarise_all(funs(mean))

gt_havo <- ACET_2021 %>%
  filter(class == "VMBO GL-TL/HAVO") %>%
  select(c(ability_lezen, ability_rekenen, ability_taal, ability_schrijven)) %>%
  summarise_all(funs(mean))

havo_vwo <- ACET_2021 %>%
  filter(class == "HAVO/VWO") %>%
  select(c(ability_lezen, ability_rekenen, ability_taal, ability_schrijven)) %>%
  summarise_all(funs(mean))

vwo <- ACET_2021 %>%
  filter(class == "VWO") %>%
  select(c(ability_lezen, ability_rekenen, ability_taal, ability_schrijven)) %>%
  summarise_all(funs(mean))


#combine
typical_students <- rbind(pro_bb, bb_kb, kb_gt, gt_havo, havo_vwo, vwo)
rm(pro_bb, bb_kb, kb_gt, gt_havo, havo_vwo, vwo)


#===========================================================================
#Simulate responses for each student, treating their theta as the true theta
#==========================================================================
#Import the MST design, routing rules and item parameters
test_design <- read.csv('dexter_mst_design_dov.csv')
routing_rules <- read.csv('dexter_mst_rules_dov.csv')
pars <- read.csv('parameters_dov.csv')
n_sim <- 1000 #a 100 simulation for each typical student
#++++++++++++
#Lezen
#+++++++++++
true.theta_L <- typical_students$ability_lezen
test_design <- read.csv('dexter_mst_design_dov.csv')
routing_rules <- read.csv('dexter_mst_rules_dov.csv')
pars <- read.csv('parameters_dov.csv')
test_design_L <-test_design[grepl('^L', test_design$module_id), ]
routing_rules_L <- routing_rules[grepl('^L', routing_rules$module_id), ]
pars_L <- pars[pars[, 'item_id'] %in% test_design_L$item_id, ]

patterns_L <- list()
abilities_L <- list()

for(i in 1:n_sim){
  patterns_L[[i]] <- dexterMST::sim_mst(pars_L, true.theta_L, test_design_L, routing_rules_L, routing = 'all')
  abilities_L[[i]] <- dexter::ability(patterns_L[[i]], parms = pars_L, method = 'WLE')
  abilities_L[[i]]$true_theta <- true.theta_L 
}

abilities_L <- do.call(rbind, abilities_L)

students_abilities_L <- list()
for(i in 1:length(true.theta_L)){
  students_abilities_L[[i]] <- abilities_L %>%
    filter(person_id == i)
}
rm(abilities_L)

patterns_L <- do.call(rbind, patterns_L)

students_patterns_L <- list()

for(i in 1:length(true.theta_L)){
  students_patterns_L[[i]] <- patterns_L %>%
    filter(person_id == i)
}

rm(patterns_L)

#++++++++++++
#Rekenen
#+++++++++++
true.theta_R <- typical_students$ability_rekenen
test_design_R <-test_design[grepl('^R', test_design$module_id), ]
routing_rules_R <- routing_rules[grepl('^R', routing_rules$module_id), ]
pars_R <- pars[pars[, 'item_id'] %in% test_design_R$item_id, ]
patterns_R <- list()
abilities_R <- list()

for(i in 1:n_sim){
  patterns_R[[i]] <- dexterMST::sim_mst(pars_R, true.theta_R, test_design_R, routing_rules_R, routing = 'all')
  abilities_R[[i]] <- dexter::ability(patterns_R[[i]], parms = pars_R, method = 'WLE')
  abilities_R[[i]]$true_theta <- true.theta_R 
}

abilities_R <- do.call(rbind, abilities_R)


students_abilities_R <- list()
for(i in 1:length(true.theta_R)){
  students_abilities_R[[i]] <- abilities_R %>%
    filter(person_id == i)
}

rm(abilities_R)

patterns_R <- do.call(rbind, patterns_R)

students_patterns_R <- list()

for(i in 1:length(true.theta_R)){
  students_patterns_R[[i]] <- patterns_R %>%
    filter(person_id == i)
}

rm(patterns_R)

#++++++++++++
#Taal
#+++++++++++
true.theta_T <- typical_students$ability_taal

test_design_T <-test_design[grepl('^T', test_design$module_id), ]
routing_rules_T <- routing_rules[grepl('^T', routing_rules$module_id), ]
pars_T <- pars[pars[, 'item_id'] %in% test_design_T$item_id, ]
patterns_T <- list()
abilities_T <- list()

for(i in 1:n_sim){
  patterns_T[[i]] <- dexterMST::sim_mst(pars_T, true.theta_T, test_design_T, routing_rules_T, routing = 'all')
  abilities_T[[i]] <- dexter::ability(patterns_T[[i]], parms = pars_T, method = 'WLE')
  abilities_T[[i]]$true_theta <- true.theta_T 
}

abilities_T <- do.call(rbind, abilities_T)

students_abilities_T <- list()
for(i in 1:length(true.theta_T)){
  students_abilities_T[[i]] <- abilities_T %>%
    filter(person_id == i)
}

rm(abilities_T)

patterns_T <- do.call(rbind, patterns_T)

students_patterns_T <- list()

for(i in 1:length(true.theta_T)){
  students_patterns_T[[i]] <- patterns_T %>%
    filter(person_id == i)
}

rm(patterns_T)


#====================================
#obtain the average theta difference
#===================================

#Lezen
for (i in 1:length(true.theta_L)){
  students_abilities_L[[i]]$diff <-  abs(students_abilities_L[[i]]$true_theta) - abs(students_abilities_L[[i]]$theta)
}

#Rekenen
for (i in 1:length(true.theta_R)){
  students_abilities_R[[i]]$diff <-  abs(students_abilities_R[[i]]$true_theta) - abs(students_abilities_R[[i]]$theta)
}

#Taal
for (i in 1:length(true.theta_T)){
  students_abilities_T[[i]]$diff <-  abs(students_abilities_T[[i]]$true_theta) - abs(students_abilities_T[[i]]$theta)
}


#Now for each of the 6 typical students we re-calculate their reference score and re-classify them, while varying the thetas (and consequently
#the theta differences). We repeat this for each subject separately, keeping the other two subjects constant (and using the true theta)


#First transform the "true theta's" into reference scores

true.scores_L <- transform.ref.score.lezen(true.theta_L)

true.scores_R <- transform.ref.score.rekenen(true.theta_R)

true.scores_T <- transform.ref.score.taal(true.theta_T)

theta.schrijven <- typical_students[,4]

scores_S <- transform.ref.score.schrijven(theta.schrijven)

#++++++++++++++++++++++
#Lezen
#++++++++++++++++++++


#1. pro/bb
true_classification <- "pro/bb"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
  for(i in 1:100){
    re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[1]][i,4])
    re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                             true.scores_T[1] * 0.232847623 + 
                                             true.scores_R[1] * 0.706118502 + 
                                             scores_S[1] * 0.127383676 + 484.75816)
    
    classification[i] <- secondary.ed(re_est_test_score[i])
    misclassifications[i] <- classification[i] == true_classification
    theta.diff <- students_abilities_L[[1]][c(which(misclassifications == FALSE)), 6]
    critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
 }

print(theta.diff)
#0.2208830 0.1810001 0.1911589  These should be considered threshold theta difference values, above which, missclassifications tend to appear
range(students_abilities_L[[1]][,6]) #0.000132293 0.287072578
print(critical_scores)  #39 37 37
 
#2. bb/kb
true_classification <- "bb/kb"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[2]][i,4])
  re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                  true.scores_T[2] * 0.232847623 + 
                                  true.scores_R[2] * 0.706118502 + 
                                  scores_S[2] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_L[[2]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

print(theta.diff)  #No such value present, all classification were TRUE
range(students_abilities_L[[2]][,6])  #-0.27329743  0.04349776
print(critical_scores)

#3. kb/gt
true_classification <- "kb/gt"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[3]][i,4])
  re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                  true.scores_T[3] * 0.232847623 + 
                                  true.scores_R[3] * 0.706118502 + 
                                  scores_S[3] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_L[[3]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff)  #No such value present, all classification were TRUE
range(students_abilities_L[[3]][,6]) #-0.2850636  0.1270095
print(critical_scores)

#4. gt/havo
true_classification <- "gt/havo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[4]][i,4])
  re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                  true.scores_T[4] * 0.232847623 + 
                                  true.scores_R[4] * 0.706118502 + 
                                  scores_S[4] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_L[[4]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff)  ##No such value present, all classification were TRUE
range(students_abilities_L[[4]][,6]) #-0.3881127  0.2280978
print(critical_scores)

#5. havo/vwo
true_classification <- "havo/vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[5]][i,4])
  re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                  true.scores_T[5] * 0.232847623 + 
                                  true.scores_R[5] * 0.706118502 + 
                                  scores_S[5] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_L[[5]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff)   # -0.2617950 -0.2453919
range(students_abilities_L[[5]][,6])  #-0.3375041  0.2740336
print(critical_scores) # 72 72

#6. vwo
true_classification <- "vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[6]][i,4])
  re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                  true.scores_T[6] * 0.232847623 + 
                                  true.scores_R[6] * 0.706118502 + 
                                  scores_S[6] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_L[[6]][c(which(misclassifications== FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff)  #No missclassificatins
range(students_abilities_L[[6]][,6]) #-0.9872288  0.2876861
print(critical_scores)
#++++++++++++++++++++++
#Rekenen
#++++++++++++++++++++

#1. pro/bb
true_classification <- "pro/bb"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[1]][i,4])
  re_est_test_score[i] <- round(true.scores_L[1] * 0.234368499 +
                                  true.scores_T[1] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[1] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[1]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff)  #0.1862854 0.2468439 0.2563819
range(students_abilities_R[[1]][,6]) #-0.3690190  0.256381
print(critical_scores) #13 12 13

#2.  bb/kb
true_classification <- "bb/kb"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[2]][i,4])
  re_est_test_score[i] <- round(true.scores_L[2] * 0.234368499 +
                                  true.scores_T[2] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[2] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[2]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) #No missclassifications
range(students_abilities_R[[2]][,6]) #-0.2267449  0.1512991
print(critical_scores)

#3.  kb/gt
true_classification <- "kb/gt"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[3]][i,4])
  re_est_test_score[i] <- round(true.scores_L[3] * 0.234368499 +
                                  true.scores_T[3] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[3] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[3]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) #0.1811092 
range(students_abilities_R[[3]][,6])  #0.000239411 0.181109228
print(critical_scores)

#4.  gt/havo
true_classification <- "gt/havo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[4]][i,4])
  re_est_test_score[i] <- round(true.scores_L[4] * 0.234368499 +
                                  true.scores_T[4] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[4] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[4]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) #0.2174139 0.2239834 0.2309973 0.2794335
range(students_abilities_R[[4]][,6])  #0.000184858 0.279433461
print(critical_scores)

#5.  havo/vwo
true_classification <- "havo/vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[5]][i,4])
  re_est_test_score[i] <- round(true.scores_L[5] * 0.234368499 +
                                  true.scores_T[5] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[5] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[5]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) #0.1811065 0.1811065 0.1811065 0.1877943 0.1877943 0.1877943 0.1997075 0.1997075 0.2190695 0.2392707 0.2825664 0.3305173 0.5188249
range(students_abilities_R[[5]][,6]) #0.0004134134 0.5188248888
print(critical_scores)

#6.  havo/vwo
true_classification <- "vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[6]][i,4])
  re_est_test_score[i] <- round(true.scores_L[6] * 0.234368499 +
                                  true.scores_T[6] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[6] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[6]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) #No missclassification
range(students_abilities_R[[6]][,6])  #0.6404295 1.3485786
print(critical_scores)


#++++++++++++++++++++++
#Taal
#++++++++++++++++++++

#1. pro/bb
true_classification <- "pro/bb"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[1]][i,4])
  re_est_test_score[i] <- round(true.scores_L[1] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[1] * 0.706118502 + 
                                  scores_S[1] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[1]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff)  #No difference
range(students_abilities_T[[1]][,6]) #0.002745995 0.424571149
print(critical_scores)


#2. bb/kb
true_classification <- "bb/kb"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[2]][i,4])
  re_est_test_score[i] <- round(true.scores_L[2] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[2] * 0.706118502 + 
                                  scores_S[2] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[2]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) #No
range(students_abilities_T[[2]][,6]) #0.0006395509 0.2625308497
print(critical_scores)

#3. kb/gt
true_classification <- "kb/gt"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[3]][i,4])
  re_est_test_score[i] <- round(true.scores_L[3] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[3] * 0.706118502 + 
                                  scores_S[3] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[3]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) #0.3151979
range(students_abilities_T[[3]][,6]) #0.001342973 0.315197881
print(critical_scores)

#4. gt/havo
true_classification <- "gt/havo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[4]][i,4])
  re_est_test_score[i] <- round(true.scores_L[4] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[4] * 0.706118502 + 
                                  scores_S[4] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[4]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) # No diff
range(students_abilities_T[[4]][,6]) #0.002449343 0.318973850
print(critical_scores)

#5. havo/vwo
true_classification <- "havo/vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[5]][i,4])
  re_est_test_score[i] <- round(true.scores_L[5] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[5] * 0.706118502 + 
                                  scores_S[5] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[5]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) #ANo diff
range(students_abilities_T[[5]][,6]) #0.0007068031 0.3244042899
print(critical_scores)

#6. vwo
true_classification <- "vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:100){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[6]][i,4])
  re_est_test_score[i] <- round(true.scores_L[6] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[6] * 0.706118502 + 
                                  scores_S[6] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[6]][c(which(misclassifications == FALSE)), 6]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) #No diff
range(students_abilities_T[[6]][,6]) #0.0007068031 0.3244042899
print(critical_scores)

#save.image('threshold_values.RData')
