#========================================================
#Threshold theta values for each subject
#======================================================

setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship/repo/mst/simulation")
library(dplyr)
source("ref-score&classification-functions.R")
load("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship/repo/mst/simulation/classification-errors-mistakes-modA2.RData")

#====================================
#obtain the average theta difference
#===================================

#Lezen

for (i in 1:length(true.theta_L)){
  students_abilities_L[[i]]$diff <-  abs(students_abilities_L[[i]]$true_theta - students_abilities_L[[i]]$theta)
}

#Rekenen
for (i in 1:length(true.theta_R)){
  students_abilities_R[[i]]$diff <-  abs(students_abilities_R[[i]]$true_theta - students_abilities_R[[i]]$theta)
}

#Taal
for (i in 1:length(true.theta_T)){
  students_abilities_T[[i]]$diff <-  abs(students_abilities_T[[i]]$true_theta - students_abilities_T[[i]]$theta)
}


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
  for(i in 1:n_sim){
    re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[1]][i,4])
    re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                             true.scores_T[1] * 0.232847623 + 
                                             true.scores_R[1] * 0.706118502 + 
                                             scores_S[1] * 0.127383676 + 484.75816)
    
    classification[i] <- secondary.ed(re_est_test_score[i])
    misclassifications[i] <- classification[i] == true_classification
    theta.diff <- students_abilities_L[[1]][c(which(misclassifications == FALSE)), 7]
    critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
 }

print(theta.diff)
range(students_abilities_L[[1]][,7]) 
print(critical_scores)  
 
#2. bb/kb
true_classification <- "bb/kb"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[2]][i,4])
  re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                  true.scores_T[2] * 0.232847623 + 
                                  true.scores_R[2] * 0.706118502 + 
                                  scores_S[2] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_L[[2]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

print(theta.diff)  
range(students_abilities_L[[2]][,7])  
print(critical_scores)

#3. kb/gt
true_classification <- "kb/gt"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[3]][i,4])
  re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                  true.scores_T[3] * 0.232847623 + 
                                  true.scores_R[3] * 0.706118502 + 
                                  scores_S[3] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_L[[3]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff)  
range(students_abilities_L[[3]][,7]) 
print(critical_scores)

#4. gt/havo
true_classification <- "gt/havo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[4]][i,4])
  re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                  true.scores_T[4] * 0.232847623 + 
                                  true.scores_R[4] * 0.706118502 + 
                                  scores_S[4] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_L[[4]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff)  
range(students_abilities_L[[4]][,7]) 
print(critical_scores)

#5. havo/vwo
true_classification <- "havo/vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[5]][i,4])
  re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                  true.scores_T[5] * 0.232847623 + 
                                  true.scores_R[5] * 0.706118502 + 
                                  scores_S[5] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_L[[5]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff)   
range(students_abilities_L[[5]][,7])  
print(critical_scores) 

#6. vwo
true_classification <- "vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.lezen(students_abilities_L[[6]][i,4])
  re_est_test_score[i] <- round(re_est_scores[i] * 0.234368499 +
                                  true.scores_T[6] * 0.232847623 + 
                                  true.scores_R[6] * 0.706118502 + 
                                  scores_S[6] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_L[[6]][c(which(misclassifications== FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff)  #No misclassificatins
range(students_abilities_L[[6]][,7]) #-0.9872288  0.2876861
print(critical_scores)


#Store the results
Class <- c("pro/bb", "bb/kb", "kb/gt", "gt/havo", "havo/vwo", "vwo")
ability.diff <- c(0.18, NA, NA, NA, 0.24, NA)
ref.sc <- c(37, NA, NA, NA, 72, NA)
threshold.Lezen <- data.frame(Class, ability.diff, ref.sc)

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
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[1]][i,4])
  re_est_test_score[i] <- round(true.scores_L[1] * 0.234368499 +
                                  true.scores_T[1] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[1] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[1]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) 
range(students_abilities_R[[1]][,7]) 
print(critical_scores) 

#2.  bb/kb
true_classification <- "bb/kb"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications <- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[2]][i,4])
  re_est_test_score[i] <- round(true.scores_L[2] * 0.234368499 +
                                  true.scores_T[2] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[2] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[2]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) 
range(students_abilities_R[[2]][,7]) 
print(critical_scores)

#3.  kb/gt
true_classification <- "kb/gt"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[3]][i,4])
  re_est_test_score[i] <- round(true.scores_L[3] * 0.234368499 +
                                  true.scores_T[3] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[3] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[3]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) 
range(students_abilities_R[[3]][,7])  
print(critical_scores)

#4.  gt/havo
true_classification <- "gt/havo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[4]][i,4])
  re_est_test_score[i] <- round(true.scores_L[4] * 0.234368499 +
                                  true.scores_T[4] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[4] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[4]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) 
range(students_abilities_R[[4]][,7])  
print(critical_scores)

#5.  havo/vwo
true_classification <- "havo/vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[5]][i,4])
  re_est_test_score[i] <- round(true.scores_L[5] * 0.234368499 +
                                  true.scores_T[5] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[5] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[5]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) 
range(students_abilities_R[[5]][,7]) 
print(critical_scores)

#6.  havo/vwo
true_classification <- "vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.rekenen(students_abilities_R[[6]][i,4])
  re_est_test_score[i] <- round(true.scores_L[6] * 0.234368499 +
                                  true.scores_T[6] * 0.232847623 + 
                                  re_est_scores[i] * 0.706118502 + 
                                  scores_S[6] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_R[[6]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) 
range(students_abilities_R[[6]][,7])  
print(critical_scores)

#Store the results
ability.diff <- c(0.19, NA, 0.17, 0.19, 0.18, NA)
ref.sc <- c(12, NA, 28, 33, 36, NA)
threshold.Rekenen <- data.frame(Class, ability.diff, ref.sc)


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
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[1]][i,4])
  re_est_test_score[i] <- round(true.scores_L[1] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[1] * 0.706118502 + 
                                  scores_S[1] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[1]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff)  
range(students_abilities_T[[1]][,7]) 
print(critical_scores)


#2. bb/kb
true_classification <- "bb/kb"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[2]][i,4])
  re_est_test_score[i] <- round(true.scores_L[2] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[2] * 0.706118502 + 
                                  scores_S[2] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[2]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) 
range(students_abilities_T[[2]][,7]) 
print(critical_scores)

#3. kb/gt
true_classification <- "kb/gt"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[3]][i,4])
  re_est_test_score[i] <- round(true.scores_L[3] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[3] * 0.706118502 + 
                                  scores_S[3] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[3]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) 
range(students_abilities_T[[3]][,7]) 
print(critical_scores)

#4. gt/havo
true_classification <- "gt/havo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[4]][i,4])
  re_est_test_score[i] <- round(true.scores_L[4] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[4] * 0.706118502 + 
                                  scores_S[4] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[4]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) 
range(students_abilities_T[[4]][,7]) 
print(critical_scores)

#5. havo/vwo
true_classification <- "havo/vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[5]][i,4])
  re_est_test_score[i] <- round(true.scores_L[5] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[5] * 0.706118502 + 
                                  scores_S[5] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[5]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) 
range(students_abilities_T[[5]][,7]) 
print(critical_scores)

#6. vwo
true_classification <- "vwo"
re_est_scores <- c()
re_est_test_score <- c()
classification <- c()
misclassifications<- c()
critical_scores <- c()
#theta.diff <- c()
for(i in 1:n_sim){
  re_est_scores[i] <-  transform.ref.score.taal(students_abilities_T[[6]][i,4])
  re_est_test_score[i] <- round(true.scores_L[6] * 0.234368499 +
                                  re_est_scores[i] * 0.232847623 + 
                                  true.scores_R[6] * 0.706118502 + 
                                  scores_S[6] * 0.127383676 + 484.75816)
  
  classification[i] <- secondary.ed(re_est_test_score[i])
  misclassifications[i] <- classification[i] == true_classification
  theta.diff <- students_abilities_T[[6]][c(which(misclassifications == FALSE)), 7]
  critical_scores <- re_est_scores[c(which(misclassifications == FALSE))]
}

sort(theta.diff) 
range(students_abilities_T[[6]][,7]) 
print(critical_scores)

#Combine
ability.diff <- c(0.21, NA, 0.29, NA, 0.37, NA)
ref.sc <- c(28, NA, 59, NA, 44, NA)
threshold.Taal <- data.frame(Class, ability.diff, ref.sc)
