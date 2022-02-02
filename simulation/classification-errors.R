#++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Classification errors
#NOTE: Before you run this script be sure to first run: `initial_analysis.R`; `errors_module_A.R`; 
source("ref-score&classification-functions.R")

#Plan: Since, till now it is already evident that mismatches between the true and the re-estimated theta
#tend to appear on the extremes of the ability distribution, we will use only these values to calculate 
# classification errors. So I will pick the top 15 most proficient and top 15 least proficient students/
#This indicates that we have a total of 30 students with a one to one mapping between the thetas for the different subjects
#So the theta value for student 1 in Lezen corresponds to the theta value for student 1 in Rekenen and so on..Which leads to the first
#assumption in this analysis:

#ASSUMPTION: I  assume that a low achieving student in one subject corresponds to a low achieving student in another subject

#For schrijven I will be using the true thetas throughout (so differences will automatically be attributed only to subjects administered using the MST)

#For NOW students who haven;t reached the reference point are labeled as "Not classifed" in the classification (talk about this!)

#++++++++++++++++++++++++++++++
#For the true theta first
#=======================
# Lezen
#======================
#----------------
#For the true theta first 
true.theta_L <- true.theta_L[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)]

#Convert to reference scores 
true.score_L <- transform.ref.score.lezen(true.theta_L)
#=======================
# Rekenen
#======================
true.theta_R <- true.theta_R[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63)]

#Convert to reference scores 
true.score_R <- transform.ref.score.rekenen(true.theta_R)
#=======================
# Taal
#======================
true.theta_T <- true.theta_T[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51)]

#Convert to reference scores 
true.score_T <- transform.ref.score.taal(true.theta_T)

#===================
#Schrijven
#================
#construct the schrijven theta, based on the provided distribution
set.seed(123)
theta_S <- sort(rnorm(2000, 0.254, 0.286), decreasing = FALSE)
#take out hte most extreme values
theta_S <- theta_S[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1986,1987,1988,1989,1990, 1991,1992,1993,1994,1995,1996,1997,1998,1999,2000)]

#Convert to reference scores 
score_S <- transform.ref.score.schrijven(theta_S)


#======= True TEST score
true_test_score <- round(true.score_L * 0.234368499 + true.score_T * 0.232847623 + true.score_R * 0.706118502 + score_S * 0.127383676 + 484.75816)


#Classify the students based on their "true scores"

true.classifications <- secondary.ed(true_test_score)


#=============================================================================================
#============================================================================================

#Now I will recalculate the test scores from re estimated thetas based on different criteria:

#==========================================
#Criteria 1: Number of mistakes in module A
#==========================================
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Note First I will check the perfect situation (i.e., where the high achieving students made 0 mistakes, and the low achoeving students made 6 mistakes)
#For the low achieving students 

#Lezen----------------------------------------
#first collect the estimated thetas for the low achieving students that made 6 mistakes (and take the average, since there might be more than one simulated value)
low_lezen <- c()
for(i in 1:15){
 low_lezen[i] <-  mean(students_abilities_L_split[[i]]$`6`[,4])
}

high_lezen <- c()
for(i in 27:41){
  high_lezen[i] <- mean(students_abilities_L_split[[i]]$`0`[,4])
}

high_lezen <- na.omit(high_lezen)

perfect_thetas_L <- c(low_lezen, high_lezen)
perfect_scores_L <- transform.ref.score.lezen(perfect_thetas_L)

#Rekenen---------------------------
#Same, but now the highest number of mistakes is 5 (since there are 5 items in Module A for Rekenen)
low_rekenen <- c()
for(i in 1:15){
  low_rekenen[i] <-  mean(students_abilities_R_split[[i]]$`5`[,4])
}

high_rekenen <- c()
for(i in 49:63){
  high_rekenen[i] <- mean(students_abilities_R_split[[i]]$`0`[,4])
}

high_rekenen <- na.omit(high_rekenen)

perfect_thetas_R <- c(low_rekenen, high_rekenen)
perfect_scores_R <- transform.ref.score.rekenen(perfect_thetas_R)

#Taal---------------------------

low_taal <- c()
for(i in 1:15){
  low_taal[i] <-  mean(students_abilities_T_split[[i]]$`5`[,4])
}

high_taal <- c()
for(i in 37:51){
  high_taal[i] <- mean(students_abilities_T_split[[i]]$`0`[,4])
}

high_taal <- na.omit(high_taal)

perfect_thetas_T <- c(low_taal, high_taal)
perfect_scores_T <- transform.ref.score.taal(perfect_thetas_T)


#----------------------------------------------
#Calculate the perfect test score (Note once more, the scores for Schrijven remain the same)
perfect_test_score <- round(perfect_scores_L * 0.234368499 + perfect_scores_T * 0.232847623 + perfect_scores_R * 0.706118502 + score_S * 0.127383676 + 484.75816)

#Classify

perfect.classifications <- secondary.ed(perfect_test_score)

#Finally, obtain the mismatches between the classification based on the true scoes and this one

mismatches_perfect <- true.classifications == perfect.classifications

# Obtain the classification error
error_perfect_scores <- length(mismatches_perfect[mismatches_perfect==FALSE])/30 #0.06666667


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Now we repeat the same, but when with increasing/decreasing the number of mistakes, for the most and least proficient students respectively

#Lezen----------------------------------------
low_lezen <- c()
for(i in 1:15){
  low_lezen[i] <-  mean(students_abilities_L_split[[i]]$`5`[,4])
}

high_lezen <- c()
for(i in 27:41){
  high_lezen[i] <- mean(students_abilities_L_split[[i]]$`1`[,4])
}

high_lezen <- na.omit(high_lezen)

one_mistake_thetas_L <- c(low_lezen, high_lezen)  #even though the object is names `one_mistake` for the low achieving students it means highest number of mistakes - 1
one_mistake_scores_L <- transform.ref.score.lezen(one_mistake_thetas_L)

#Rekenen---------------------------
#Same, but now the highest number of mistakes is 5 (since there are 5 items in Module A for Rekenen)
low_rekenen <- c()
for(i in 1:15){
  low_rekenen[i] <-  mean(students_abilities_R_split[[i]]$`4`[,4])
}

high_rekenen <- c()
for(i in 49:63){
  high_rekenen[i] <- mean(students_abilities_R_split[[i]]$`1`[,4])
}

high_rekenen <- na.omit(high_rekenen)

one_mistake_thetas_R <- c(low_rekenen, high_rekenen)
one_mistake_scores_R <- transform.ref.score.rekenen(one_mistake_thetas_R)

#Taal---------------------------

low_taal <- c()
for(i in 1:15){
  low_taal[i] <-  mean(students_abilities_T_split[[i]]$`4`[,4])
}

high_taal <- c()
for(i in 37:51){
  high_taal[i] <- mean(students_abilities_T_split[[i]]$`1`[,4])
}

high_taal <- na.omit(high_taal)

one_mistake_thetas_T <- c(low_taal, high_taal)
one_mistake_scores_T <- transform.ref.score.taal(one_mistake_thetas_T)


#----------------------------------------------
#Calculate the perfect test score (Note once more, the scores for Schrijven remain the same)
one_mistake_test_score <- round(one_mistake_scores_L * 0.234368499 + one_mistake_scores_T * 0.232847623 + one_mistake_scores_R * 0.706118502 + score_S * 0.127383676 + 484.75816)

#Classify

one_mistake.classifications <- secondary.ed(one_mistake_test_score)

#Finally, obtain the mismatches between the classification based on the true scoes and this one

mismatches_one_mistake <- true.classifications == one_mistake.classifications

# Obtain the classification error
error_one_mistake <- length(mismatches_one_mistake[mismatches_one_mistake==FALSE])/30 #0.2


#It is evident that as the number of mistakes rises from 0 to 1, there trend to be mismatches for the most profficient stdents.



#===========================================
#Criteria 2: Number of mistakes in day 1

#TBA






#==========================================
#Criteria 3: Different paths (this will only vary for Taal, since we spotted differnces only there)

#TBA
