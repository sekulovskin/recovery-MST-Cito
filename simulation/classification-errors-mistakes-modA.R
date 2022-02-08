#=========================================================
#Classification errors based on the ACET results from 2021
#========================================================
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship/repo/mst/simulation")
library(dplyr)
source("ref-score&classification-functions.R")
options(warn = -1)
ACET_2021 <- read.csv("acet_data_2021.csv")
summary(ACET_2021)
#sort in an increasing order based on the final test score
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

#Combine them

typical_students <- rbind(pro_bb, bb_kb, kb_gt, gt_havo, havo_vwo, vwo)
rm(pro_bb, bb_kb, kb_gt, gt_havo, havo_vwo, vwo)

#===========================================================================
#Simulate responses for each student, treating their theta as the true theta
#==========================================================================
#Import the MST design, routing rules and item parameters
test_design <- read.csv('dexter_mst_design_dov.csv')
routing_rules <- read.csv('dexter_mst_rules_dov.csv')
pars <- read.csv('parameters_dov.csv')
n_sim <- 1000  #a 1000 simulation for each typical student
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

#==================================================================================================
#Now we run the scripit `errors_module_A.R` to obtain the number of mistakes  in Module A
#==================================================================================================

#==============================================================
#Calculate classification errors
#=============================================================

#+++++++++++++++++++
#1. Pro-bb
#+++++++++++++++++
#First for the true abilities 

#Lezen 
true.score_L_pro.bb <- transform.ref.score.lezen(typical_students[1,1])
#Rekenen
true.score_R_pro.bb <- transform.ref.score.rekenen(typical_students[1,2])
#Taal
true.score_T_pro.bb <- transform.ref.score.taal(typical_students[1,3])
#Schjrijven (THIS WILL BE USE THROUGHOUT)
score_S_pro.bb <- transform.ref.score.schrijven(typical_students[1,4])

#Classify
true.classification_pro.bb  <- "pro/bb"

#Now for the different number of errors

#====Max number of errors (6+5+5 = 16)

score_L_pro.bb.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[1]]$`6`[,4])

score_R_pro.bb.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[1]]$`5`[,4])

score_T_pro.bb.mistakes <- transform.ref.score.taal(students_abilities_T_split[[1]]$`5`[,4])


#Test score
test_score_pro.bb.mistakes <- round(score_L_pro.bb.mistakes[1:80] * 0.234368499 + score_T_pro.bb.mistakes[1:80] * 0.232847623 + score_R_pro.bb.mistakes[1:80] * 0.706118502 + score_S_pro.bb * 0.127383676 + 484.75816)

# Classify 
classification_pro.bb.mistakes <- secondary.ed(test_score_pro.bb.mistakes)

#See mistmatches
mismatches <- true.classification_pro.bb == classification_pro.bb.mistakes

# Calculate classification error
error.pro.bb.16mistakes <- length(mismatches[mismatches==FALSE])/length(classification_pro.bb.mistakes) 

#==== 5 + 4 + 4 = 13 

score_L_pro.bb.mistake <- transform.ref.score.lezen(students_abilities_L_split[[1]]$`5`[,4])

score_R_pro.bb.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[1]]$`4`[,4])

score_T_pro.bb.mistakes <- transform.ref.score.taal(students_abilities_T_split[[1]]$`4`[,4])


#Test score
test_score_pro.bb.mistakes<- round(score_L_pro.bb.mistakes[1:200] * 0.234368499 + score_T_pro.bb.mistakes[1:200] * 0.232847623 + score_R_pro.bb.mistakes[1:200] * 0.706118502 + score_S_pro.bb * 0.127383676 + 484.75816)

# Classify 
classification_pro.bb.mistakes <- secondary.ed(test_score_pro.bb.mistakes)

#See mistmatches
mismatches <- true.classification_pro.bb == classification_pro.bb.mistakes

# Calculate classification error
error.pro.bb.13mistakes <- length(mismatches[mismatches==FALSE])/length(classification_pro.bb.mistakes) 


#==== 4 + 3 + 3 = 10  (HERE 150 REPLICATIONS WILL BE USED)

score_L_pro.bb.mistake <- transform.ref.score.lezen(students_abilities_L_split[[1]]$`4`[,4])

score_R_pro.bb.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[1]]$`3`[,4])

score_T_pro.bb.mistakes <- transform.ref.score.taal(students_abilities_T_split[[1]]$`3`[,4])

#Test score
test_score_pro.bb.mistakes<- round(score_L_pro.bb.mistakes[1:150] * 0.234368499 + score_T_pro.bb.mistakes[1:150] * 0.232847623 + score_R_pro.bb.mistakes[1:150] * 0.706118502 + score_S_pro.bb * 0.127383676 + 484.75816)

# Classify 
classification_pro.bb.mistakes <- secondary.ed(test_score_pro.bb.mistakes)

#See mistmatches
mismatches <- true.classification_pro.bb == classification_pro.bb.mistakes

# Calculate classification error
error.pro.bb.10mistakes <- length(mismatches[mismatches==FALSE])/length(classification_pro.bb.mistakes) 


#==== 4 + 3 + 2 = 9  (HERE 150 REPLICATIONS WILL BE USED)

score_L_pro.bb.mistake <- transform.ref.score.lezen(students_abilities_L_split[[1]]$`4`[,4])

score_R_pro.bb.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[1]]$`3`[,4])

score_T_pro.bb.mistakes <- transform.ref.score.taal(students_abilities_T_split[[1]]$`2`[,4])

#Test score
test_score_pro.bb.mistakes<- round(score_L_pro.bb.mistakes[1:150] * 0.234368499 + score_T_pro.bb.mistakes[1:150] * 0.232847623 + score_R_pro.bb.mistakes[1:150] * 0.706118502 + score_S_pro.bb * 0.127383676 + 484.75816)

# Classify 
classification_pro.bb.mistakes <- secondary.ed(test_score_pro.bb.mistakes)

#See mistmatches
mismatches <- true.classification_pro.bb == classification_pro.bb.mistakes

# Calculate classification error
error.pro.bb.9mistakes <- length(mismatches[mismatches==FALSE])/length(classification_pro.bb.mistakes) 
#---------------------
#Combine
errors  <- c(error.pro.bb.16mistakes, error.pro.bb.13mistakes, error.pro.bb.10mistakes, error.pro.bb.9mistakes)
mistakes <- c(16, 13, 10, 9)
errors.pro.bb.df <- data.frame(errors, mistakes)
rm(error.pro.bb.16mistakes, error.pro.bb.13mistakes, error.pro.bb.10mistakes, error.pro.bb.9mistakes)
#------------------

#+++++++++++++++++++
#2. bb/kb
#+++++++++++++++++

#Lezen 
true.score_L_bb.kb <- transform.ref.score.lezen(typical_students[2,1])
#Rekenen
true.score_R_bb.kb<- transform.ref.score.rekenen(typical_students[2,2])
#Taal
true.score_T_bb.kb <- transform.ref.score.taal(typical_students[2,3])
#Schjrijven (THIS WILL BE USE THROUGHOUT)
score_S_bb.kb <- transform.ref.score.schrijven(typical_students[2,4])

#Classify
true.classification_bb.kb  <- "bb/kb"

#Now repeat for different number of errors

#====Max number of errors (5+ 4 + 3 = 12)
#HERE 130 WILL BE USED
score_L_bb.kb.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[2]]$`5`[,4])

score_R_bb.kb.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[2]]$`4`[,4])

score_T_bb.kb.mistakes <- transform.ref.score.taal(students_abilities_T_split[[2]]$`3`[,4])

#Test score
test_score_bb.kb.mistakes <- round(score_L_bb.kb.mistakes[1:130] * 0.234368499 + score_T_bb.kb.mistakes[1:130] * 0.232847623 + score_R_bb.kb.mistakes[1:130] * 0.706118502 + score_S_bb.kb * 0.127383676 + 484.75816)

# Classify 
classification_bb.kb.mistakes <- secondary.ed(test_score_bb.kb.mistakes)

#See mistmatches
mismatches <- true.classification_bb.kb == classification_bb.kb.mistakes

# Calculate classification error
error.bb.kb.12mistakes <- length(mismatches[mismatches==FALSE])/length(classification_bb.kb.mistakes) 

# (4 + 3 + 2 = 9)

score_L_bb.kb.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[2]]$`4`[,4])

score_R_bb.kb.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[2]]$`3`[,4])

score_T_bb.kb.mistakes <- transform.ref.score.taal(students_abilities_T_split[[2]]$`2`[,4])

#Test score
test_score_bb.kb.mistakes<- round(score_L_bb.kb.mistakes[1:200] * 0.234368499 + score_T_bb.kb.mistakes[1:200] * 0.232847623 + score_R_bb.kb.mistakes[1:200] * 0.706118502 + score_S_bb.kb * 0.127383676 + 484.75816)

# Classify 
classification_bb.kb.mistakes <- secondary.ed(test_score_bb.kb.mistakes)

#See mistmatches
mismatches <- true.classification_bb.kb == classification_bb.kb.mistakes

# Calculate classification error
error.bb.kb.9mistakes <- length(mismatches[mismatches==FALSE])/length(classification_bb.kb.mistakes) 

# (3+ 2 + 1 = 6)

score_L_bb.kb.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[2]]$`3`[,4])

score_R_bb.kb.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[2]]$`2`[,4])

score_T_bb.kb.mistakes <- transform.ref.score.taal(students_abilities_T_split[[2]]$`1`[,4])

#Test score
test_score_bb.kb.mistakes<- round(score_L_bb.kb.mistakes[1:200] * 0.234368499 + score_T_bb.kb.mistakes[1:200] * 0.232847623 + score_R_bb.kb.mistakes[1:200] * 0.706118502 + score_S_bb.kb * 0.127383676 + 484.75816)

# Classify 
classification_bb.kb.mistakes <- secondary.ed(test_score_bb.kb.mistakes)

#See mistmatches
mismatches <- true.classification_bb.kb == classification_bb.kb.mistakes

# Calculate classification error
error.bb.kb.6mistakes <- length(mismatches[mismatches==FALSE])/length(classification_bb.kb.mistakes) 


# (2 + 1 + 0 = 4)
#HEre 90 obcervations will be used
score_L_bb.kb.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[2]]$`2`[,4])

score_R_bb.kb.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[2]]$`1`[,4])

score_T_bb.kb.mistakes <- transform.ref.score.taal(students_abilities_T_split[[2]]$`0`[,4])

#Test score
test_score_bb.kb.mistakes<- round(score_L_bb.kb.mistakes[1:90] * 0.234368499 + score_T_bb.kb.mistakes[1:90] * 0.232847623 + score_R_bb.kb.mistakes[1:90] * 0.706118502 + score_S_bb.kb * 0.127383676 + 484.75816)

# Classify 
classification_bb.kb.mistakes <- secondary.ed(test_score_bb.kb.mistakes)

#See mistmatches
mismatches <- true.classification_bb.kb == classification_bb.kb.mistakes

# Calculate classification error
error.bb.kb.3mistakes <- length(mismatches[mismatches==FALSE])/length(classification_bb.kb.mistakes) 

#Combine
errors <- c(error.bb.kb.12mistakes, error.bb.kb.9mistakes, error.bb.kb.6mistakes, error.bb.kb.3mistakes)
mistakes <- c(12, 9, 6, 3)
errors.bb.kb.df <- data.frame(errors, mistakes)
rm(error.bb.kb.12mistakes, error.bb.kb.9mistakes, error.bb.kb.6mistakes, error.bb.kb.3mistakes)

#+++++++++++++++++++
#3. kb/gt
#+++++++++++++++++

#Lezen 
true.score_L_kb.gt <- transform.ref.score.lezen(typical_students[3,1])
#Rekenen
true.score_R_kb.gt<- transform.ref.score.rekenen(typical_students[3,2])
#Taal
true.score_T_kb.gt <- transform.ref.score.taal(typical_students[3,3])
#Schjrijven (THIS WILL BE USE THROUGHOUT)
score_S_kb.gt <- transform.ref.score.schrijven(typical_students[3,4])

#Classify
true.classification_kb.gt <- "kb/gt"

#Now repeat for different number of errors

#====Max number of errors ( 4 + 2 + 3 = 9)
#80 observations will be used
score_L_kb.gt.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[3]]$`4`[,4])

score_R_kb.gt.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[3]]$`2`[,4])

score_T_kb.gt.mistakes <- transform.ref.score.taal(students_abilities_T_split[[3]]$`3`[,4])

#Test score
test_score_kb.gt.mistakes<- round(score_L_kb.gt.mistakes[1:80] * 0.234368499 + score_T_kb.gt.mistakes[1:80] * 0.232847623 + score_R_kb.gt.mistakes[1:80] * 0.706118502 + score_S_kb.gt * 0.127383676 + 484.75816)

# Classify 
classification_kb.gt.mistakes <- secondary.ed(test_score_kb.gt.mistakes)

#See mistmatches
mismatches <- true.classification_kb.gt == classification_kb.gt.mistakes

# Calculate classification error
error.kb.gt.9mistakes <- length(mismatches[mismatches==FALSE])/length(classification_kb.gt.mistakes) 

#(3 + 1 + 2 = 6)

score_L_kb.gt.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[3]]$`3`[,4])

score_R_kb.gt.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[3]]$`1`[,4])

score_T_kb.gt.mistakes <- transform.ref.score.taal(students_abilities_T_split[[3]]$`2`[,4])

#Test score
test_score_kb.gt.mistakes<- round(score_L_kb.gt.mistakes[1:200] * 0.234368499 + score_T_kb.gt.mistakes[1:200] * 0.232847623 + score_R_kb.gt.mistakes[1:200] * 0.706118502 + score_S_kb.gt * 0.127383676 + 484.75816)

# Classify 
classification_kb.gt.mistakes <- secondary.ed(test_score_kb.gt.mistakes)

#See mistmatches
mismatches <- true.classification_kb.gt == classification_kb.gt.mistakes

# Calculate classification error
error.kb.gt.6mistakes <- length(mismatches[mismatches==FALSE])/length(classification_kb.gt.mistakes) 

#( 2+ 0 + 1 = 3)

score_L_kb.gt.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[3]]$`2`[,4])

score_R_kb.gt.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[3]]$`0`[,4])

score_T_kb.gt.mistakes <- transform.ref.score.taal(students_abilities_T_split[[3]]$`1`[,4])

#Test score
test_score_kb.gt.mistakes<- round(score_L_kb.gt.mistakes[1:200] * 0.234368499 + score_T_kb.gt.mistakes[1:200] * 0.232847623 + score_R_kb.gt.mistakes[1:200] * 0.706118502 + score_S_kb.gt * 0.127383676 + 484.75816)

# Classify 
classification_kb.gt.mistakes <- secondary.ed(test_score_kb.gt.mistakes)

#See mistmatches
mismatches <- true.classification_kb.gt == classification_kb.gt.mistakes

# Calculate classification error
error.kb.gt.3mistakes <- length(mismatches[mismatches==FALSE])/length(classification_kb.gt.mistakes)

#( 1+ 0 + 0 = 1)

score_L_kb.gt.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[3]]$`1`[,4])

score_R_kb.gt.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[3]]$`0`[,4])

score_T_kb.gt.mistakes <- transform.ref.score.taal(students_abilities_T_split[[3]]$`0`[,4])

#Test score
test_score_kb.gt.mistakes<- round(score_L_kb.gt.mistakes[1:200] * 0.234368499 + score_T_kb.gt.mistakes[1:200] * 0.232847623 + score_R_kb.gt.mistakes[1:200] * 0.706118502 + score_S_kb.gt * 0.127383676 + 484.75816)

# Classify 
classification_kb.gt.mistakes <- secondary.ed(test_score_kb.gt.mistakes)

#See mistmatches
mismatches <- true.classification_kb.gt == classification_kb.gt.mistakes

# Calculate classification error
error.kb.gt.1mistakes <- length(mismatches[mismatches==FALSE])/length(classification_kb.gt.mistakes) 

#Combine
errors  <- c(error.kb.gt.9mistakes, error.kb.gt.6mistakes, error.kb.gt.3mistakes, error.kb.gt.1mistakes)
mistakes <- c(9, 6, 3, 1)
errors.kb.gt.df <- data.frame(errors, mistakes)
rm(error.kb.gt.9mistakes, error.kb.gt.6mistakes, error.kb.gt.3mistakes, error.kb.gt.1mistakes)

#+++++++++++++++++++
#4. gt/havo
#+++++++++++++++++

#Lezen 
true.score_L_gt.havo <- transform.ref.score.lezen(typical_students[4,1])
#Rekenen
true.score_R_gt.havo<- transform.ref.score.rekenen(typical_students[4,2])
#Taal
true.score_T_gt.havo <- transform.ref.score.taal(typical_students[4,3])
#Schjrijven (THIS WILL BE USE THROUGHOUT)
score_S_gt.havo <- transform.ref.score.schrijven(typical_students[4,4])

#Classify
true.classification_gt.havo <- "gt/havo"

#====Max number of errors (3 +3 +3  = 9)  (ONLY 13 SAMPLES ARE USED)
score_L_gt.havo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[4]]$`3`[,4])

score_R_gt.havo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[4]]$`3`[,4])

score_T_gt.havo.mistakes <- transform.ref.score.taal(students_abilities_R_split[[4]]$`3`[,4])

#Test score
test_score_gt.havo.mistakes<- round(score_L_gt.havo.mistakes[1:13] * 0.234368499 + score_T_gt.havo.mistakes[1:13] * 0.232847623 + score_R_gt.havo.mistakes[1:13] * 0.706118502 + score_S_gt.havo * 0.127383676 + 484.75816)

# Classify 
classification_gt.havo.mistakes <- secondary.ed(test_score_gt.havo.mistakes)

#See mistmatches
mismatches <- true.classification_gt.havo == classification_gt.havo.mistakes

# Calculate classification error
error.gt.havo.9mistakes <- length(mismatches[mismatches==FALSE])/length(classification_gt.havo.mistakes) #

#( 2 +2 +2  = 6)  (78 SAMPLES ARE USED)

score_L_gt.havo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[4]]$`2`[,4])

score_R_gt.havo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[4]]$`2`[,4])

score_T_gt.havo.mistakes <- transform.ref.score.taal(students_abilities_R_split[[4]]$`2`[,4])

#Test score
test_score_gt.havo.mistakes <- round(score_L_gt.havo.mistakes[1:78] * 0.234368499 + score_T_gt.havo.mistakes[1:78] * 0.232847623 + score_R_gt.havo.mistakes[1:78] * 0.706118502 + score_S_gt.havo * 0.127383676 + 484.75816)

# Classify 
classification_gt.havo.mistakes <- secondary.ed(test_score_gt.havo.mistakes)

#See mistmatches
mismatches <- true.classification_gt.havo == classification_gt.havo.mistakes

# Calculate classification error
error.gt.havo.6mistakes <- length(mismatches[mismatches==FALSE])/length(classification_gt.havo.mistakes) #0.32

#(1 + 1 + 1 = 3)

score_L_gt.havo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[4]]$`1`[,4])

score_R_gt.havo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[4]]$`1`[,4])

score_T_gt.havo.mistakes <- transform.ref.score.taal(students_abilities_R_split[[4]]$`1`[,4])

#Test score
test_score_gt.havo.mistakes<- round(score_L_gt.havo.mistakes[1:200] * 0.234368499 + score_T_gt.havo.mistakes[1:200] * 0.232847623 + score_R_gt.havo.mistakes[1:200] * 0.706118502 + score_S_gt.havo * 0.127383676 + 484.75816)

# Classify 
classification_gt.havo.mistakes <- secondary.ed(test_score_gt.havo.mistakes)

#See mistmatches
mismatches <- true.classification_gt.havo == classification_gt.havo.mistakes

# Calculate classification error
error.gt.havo.3mistakes <- length(mismatches[mismatches==FALSE])/length(classification_gt.havo.mistakes) #0.34

#(1 + 0 + 0 = 1)

score_L_gt.havo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[4]]$`1`[,4])

score_R_gt.havo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[4]]$`0`[,4])

score_T_gt.havo.mistakes <- transform.ref.score.taal(students_abilities_R_split[[4]]$`0`[,4])

#Test score
test_score_gt.havo.mistakes<- round(score_L_gt.havo.mistakes[1:200] * 0.234368499 + score_T_gt.havo.mistakes[1:200] * 0.232847623 + score_R_gt.havo.mistakes[1:200] * 0.706118502 + score_S_gt.havo * 0.127383676 + 484.75816)

# Classify 
classification_gt.havo.mistakes <- secondary.ed(test_score_gt.havo.mistakes)

#See mistmatches
mismatches <- true.classification_gt.havo == classification_gt.havo.mistakes

# Calculate classification error
error.gt.havo.1mistakes <- length(mismatches[mismatches==FALSE])/length(classification_gt.havo.mistakes) 

#Combine
errors  <- c(error.gt.havo.9mistakes,error.gt.havo.6mistakes, error.gt.havo.3mistakes, error.gt.havo.1mistakes)
mistakes <- c(9, 6, 3, 1)
errors.gt.havo.df <- data.frame(errors, mistakes)
rm(error.gt.havo.6mistakes, error.gt.havo.3mistakes, error.gt.havo.1mistakes)

#+++++++++++++++++++
#5. havo/vwo
#+++++++++++++++++
#Lezen 
true.score_L_havo.vwo <- transform.ref.score.lezen(typical_students[5,1])
#Rekenen
true.score_R_havo.vwo <- transform.ref.score.rekenen(typical_students[5,2])
#Taal
true.score_T_havo.vwo<- transform.ref.score.taal(typical_students[5,3])
#Schjrijven (THIS WILL BE USE THROUGHOUT)
score_S_havo.vwo <- transform.ref.score.schrijven(typical_students[5,4])

true.classification_havo.vwo <- "havo/vwo"

#====Max number of errors ( 3 +3 + 3 = 9)  
#Since there are a small amount of observations we recycle them leading us to a sampple size of 47
score_L_havo.vwo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[5]]$`2`[,4])

score_R_havo.vwo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[5]]$`2`[,4])

score_T_havo.vwo.mistakes <- transform.ref.score.taal(students_abilities_T_split[[5]]$`2`[,4])

#Test score
test_score_havo.vwo.mistakes <- round(score_L_havo.vwo.mistakes * 0.234368499 + score_T_havo.vwo.mistakes * 0.232847623 + score_R_havo.vwo.mistakes * 0.706118502 + score_S_havo.vwo * 0.127383676 + 484.75816)

# Classify 
classification_havo.vwo.mistakes <- secondary.ed(test_score_havo.vwo.mistakes)

#See mistmatches
mismatches <- true.classification_havo.vwo == classification_havo.vwo.mistakes

# Calculate classification error
error.havo.vwo.9mistakes <- length(mismatches[mismatches==FALSE])/length(classification_havo.vwo.mistakes) 

#====Max number of errors ( 1 + 1 + 1 = 4)  

score_L_havo.vwo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[5]]$`2`[,4])

score_R_havo.vwo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[5]]$`1`[,4])

score_T_havo.vwo.mistakes <- transform.ref.score.taal(students_abilities_T_split[[5]]$`1`[,4])

#Test score
testest_score_havo.vwo.mistakes <- round(score_L_havo.vwo.mistakes[1:200] * 0.234368499 + score_T_havo.vwo.mistakes[1:200] * 0.232847623 + score_R_havo.vwo.mistakes[1:200] * 0.706118502 + score_S_havo.vwo * 0.127383676 + 484.75816)

# Classify 
classification_havo.vwo.mistakes <- secondary.ed(test_score_havo.vwo.mistakes)

#See mistmatches
mismatches <- true.classification_havo.vwo == classification_havo.vwo.mistakes

# Calculate classification error
error.havo.vwo.4mistakes <- length(mismatches[mismatches==FALSE])/length(classification_havo.vwo.mistakes) 

# ( 1 + 1 + 1 = 3)

score_L_havo.vwo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[5]]$`1`[,4])

score_R_havo.vwo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[5]]$`1`[,4])

score_T_havo.vwo.mistakes <- transform.ref.score.taal(students_abilities_T_split[[5]]$`1`[,4])

#Test score
test_score_havo.vwo.mistakes <- round(score_L_havo.vwo.mistakes[1:200] * 0.234368499 + score_T_havo.vwo.mistakes[1:200] * 0.232847623 + score_R_havo.vwo.mistakes[1:200] * 0.706118502 + score_S_havo.vwo * 0.127383676 + 484.75816)

# Classify 
classification_havo.vwo.mistakes <- secondary.ed(test_score_havo.vwo.mistakes)

#See mistmatches
mismatches <- true.classification_havo.vwo == classification_havo.vwo.mistakes

# Calculate classification error
error.havo.vwo.3mistakes <- length(mismatches[mismatches==FALSE])/length(classification_havo.vwo.mistakes) 

# ( 0 + 0 + 0 = 0)

score_L_havo.vwo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[5]]$`0`[,4])

score_R_havo.vwo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[5]]$`0`[,4])

score_T_havo.vwo.mistakes <- transform.ref.score.taal(students_abilities_T_split[[5]]$`0`[,4])

#Test score
test_score_havo.vwo.mistakes<- round(score_L_havo.vwo.mistakes[1:200] * 0.234368499 + score_T_havo.vwo.mistakes[1:200] * 0.232847623 + score_R_havo.vwo.mistakes[1:200] * 0.706118502 + score_S_havo.vwo * 0.127383676 + 484.75816)

# Classify 
classification_havo.vwo.mistakes <- secondary.ed(test_score_havo.vwo.mistakes)

#See mistmatches
mismatches <- true.classification_havo.vwo == classification_havo.vwo.mistakes

# Calculate classification error
error.havo.vwo.0mistakes <- length(mismatches[mismatches==FALSE])/length(classification_havo.vwo.mistakes) #0.2

#Combine
errors  <- c(error.havo.vwo.9mistakes, error.havo.vwo.4mistakes, error.havo.vwo.3mistakes,error.havo.vwo.0mistakes)
mistakes <- c(9, 4, 3, 0)
errors.havo.vwo.df <- data.frame(errors, mistakes)
rm(error.havo.vwo.9mistakes, error.havo.vwo.4mistakes, error.havo.vwo.3mistakes,error.havo.vwo.0mistakes)

#+++++++++++++++++++
#6.vwo
#+++++++++++++++++
#+#Lezen 
true.score_L_vwo <- transform.ref.score.lezen(typical_students[6,1])
#Rekenen
true.score_R_vwo <- transform.ref.score.rekenen(typical_students[6,2])
#Taal
true.score_T_vwo<- transform.ref.score.taal(typical_students[6,3])
#Schjrijven (THIS WILL BE USE THROUGHOUT)
score_S_vwo <- transform.ref.score.schrijven(typical_students[6,4])

true.classification_vwo <- "vwo"

# Max number of mistakes (3 +1 +2 = 6)
#Here we recycle since the number of mistakes (especially for Lzen is low) leading to N = ~80
score_L_vwo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[6]]$`3`[,4])

score_R_vwo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[6]]$`1`[,4])

score_T_vwo.mistakes <- transform.ref.score.taal(students_abilities_T_split[[6]]$`2`[,4])

#Test score
test_score_vwo.mistakes<- round(score_L_vwo.mistakes * 0.234368499 + score_T_vwo.mistakes * 0.232847623 + score_R_vwo.mistakes * 0.706118502 + score_S_vwo * 0.127383676 + 484.75816)

# Classify 
classification_vwo.mistakes <- secondary.ed(test_score_vwo.mistakes)

#See mistmatches
mismatches <- true.classification_vwo == classification_vwo.mistakes

# Calculate classification error
error.vwo.6mistakes <- length(mismatches[mismatches==FALSE])/length(classification_vwo.mistakes)

# (1 + 1 + 1 = 3)
score_L_vwo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[6]]$`1`[,4])
#Since R doesn;t have enough observations we will have to recycyle
score_R_vwo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[6]]$`1`[,4])

score_T_vwo.mistakes <- transform.ref.score.taal(students_abilities_T_split[[6]]$`1`[,4])

#Test score
test_score_vwo.mistakes<- round(score_L_vwo.mistakes[1:200] * 0.234368499 + score_T_vwo.mistakes[1:200] * 0.232847623 + score_R_vwo.mistakes * 0.706118502 + score_S_vwo * 0.127383676 + 484.75816)

# Classify 
classification_vwo.mistakes <- secondary.ed(test_score_vwo.mistakes)

#See mistmatches
mismatches <- true.classification_vwo == classification_vwo.mistakes

# Calculate classification error
error.vwo.3mistakes <- length(mismatches[mismatches==FALSE])/length(classification_vwo.mistakes)

#  (1 + 0 + 0 = 1)

score_L_vwo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[6]]$`1`[,4])

score_R_vwo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[6]]$`0`[,4])

score_T_vwo.mistakes <- transform.ref.score.taal(students_abilities_T_split[[6]]$`0`[,4])
#Test score
test_score_vwo.mistakes<- round(score_L_vwo.mistakes[1:200] * 0.234368499 + score_T_vwo.mistakes[1:200] * 0.232847623 + score_R_vwo.mistakes[1:200] * 0.706118502 + score_S_vwo * 0.127383676 + 484.75816)

# Classify 
classification_vwo.mistakes <- secondary.ed(test_score_vwo.mistakes)

#See mistmatches
mismatches <- true.classification_vwo == classification_vwo.mistakes

# Calculate classification error
error.vwo.1mistakes <- length(mismatches[mismatches==FALSE])/length(classification_vwo.mistakes) 

#  (0 + 0 + 0 = 0)

score_L_vwo.mistakes <- transform.ref.score.lezen(students_abilities_L_split[[6]]$`0`[,4])

score_R_vwo.mistakes <- transform.ref.score.rekenen(students_abilities_R_split[[6]]$`0`[,4])

score_T_vwo.mistakes <- transform.ref.score.taal(students_abilities_T_split[[6]]$`0`[,4])
#Test score
test_score_vwo.mistakes<- round(score_L_vwo.mistakes[1:200] * 0.234368499 + score_T_vwo.mistakes[1:200] * 0.232847623 + score_R_vwo.mistakes[1:200] * 0.706118502 + score_S_vwo * 0.127383676 + 484.75816)

# Classify 
classification_vwo.mistakes <- secondary.ed(test_score_vwo.mistakes)

#See mistmatches
mismatches <- true.classification_vwo == classification_vwo.mistakes

# Calculate classification error
error.vwo.0mistakes <- length(mismatches[mismatches==FALSE])/length(classification_vwo.mistakes) #0

#Combine
errors <- c(error.vwo.6mistakes, error.vwo.3mistakes, error.vwo.1mistakes, error.vwo.0mistakes)
mistakes <- c(6, 3, 1, 0)
errors.vwo.df <- data.frame(errors, mistakes)
rm(error.vwo.6mistakes, error.vwo.3mistakes, error.vwo.1mistakes, error.vwo.0mistakes)

#=======================================================================================

#Combine all: 
classification.classes.error.modA <- rbind(errors.pro.bb.df, errors.bb.kb.df, errors.kb.gt.df, errors.gt.havo.df, errors.havo.vwo.df, errors.vwo.df)
classification.classes.error.modA$class <- rep(c("pro/bb","bb/kb","kb/gt","gt/havo","havo/vwo","vwo"), each = 4)

library(foreign)
write.csv(classification.classes.error.modA, "errors.data.modA.csv")
save.image("classification-errors-mistakes-modA2.RData")
