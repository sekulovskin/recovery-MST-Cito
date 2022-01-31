#++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Classification errors
library(dplyr)
#Plan: Since, till now it is already evident that mismatches between the true and the re-estimated theta
#tend to appear on the extremes of the ability distribution, we will use only these values to calculate 
# classification errors. So I will pick the top 10 most proficient and top 10 least proficient students

#ASSUMPTION: I  assume that a low achieving student in one subject corresponds to a low achieving student in another subject

#For schrijven I will be using the true thetas trhroughout.

#WHAT IF THE STUDENT HASN'T REACHED THE REFERENCE POITN?? (FOR NOW I IGNORE THIS)
#Should I maybe expand the theta continuum??
#++++++++++++++++++++++++++++++
#For the true theta first
#=======================
# Lezen
#======================
#----------------
#For the true theta first 
true.theta_L <- true.theta_L[c(1,2,3,4,5,6,7,8,9,10,32,33,34,35,36,37,38,39,40,41)]

#Convert to scores according to the supplied rules
true.score_L <- c(14, 15,  15,   19,  21,   23,  25,
          28,   30,   33,  77,  78,   78,  79,
         80 , 80,  81L,  81,  81, 82) 
#=======================
# Rekenen
#======================
true.theta_R <- true.theta_R[c(1,2,3,4,5,6,7,8,9,10,54,55,56,57,58,59,60,61,62,63)]

true.score_R <- c(2, 2, 2, 3, 3, 3, 4, 4,  4, 5, 39, 40, 40, 40, 40, 40, 40, 40, 40, 40)

#=======================
# Taal
#======================
true.theta_T <- true.theta_T[c(1,2,3,4,5,6,7,8,9,10,42,43,44,45,46,47,48,49,50,51)]

true.score_T <- c(3, 4, 4, 5, 6, 6, 7, 8, 9, 11, 67, 68, 68, 68, 68, 69, 69, 69, 69, 69)

#===================
#Schrijven
#================

#construct the schrijven theta, based on the provided distribution
set.seed(123)
theta_S <- sort(rnorm(2000, 0.254, 0.286), decreasing = FALSE)
#take out hte most extreme values
theta_S <- theta_S[c(1,2,3,4,5,6,7,8,9,10, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000)]

score_S <- c(6, 7, 7, 7, 7, 8, 8, 8, 9, 9, 58, 58, 58, 58, 58, 58, 58, 59, 59, 59)

#======= Ture test score
true_test_score <- round(true.score_L * 0.234368499 + true.score_T * 0.232847623 + true.score_R * 0.706118502 + score_S * 0.127383676 + 484.75816)

#======= Classification

secondary.ed <- function(x){
  results <- c()
 for(i in 1:length(x)){
  if(x[i] <= 500){
    results[i] <-  "Not classfied"
  }
  else if (x[i] >= 500 & x[i] <= 504){
    results[i] <-"pro"
  }
  
  else if (x[i] >= 505 & x[i] <= 524){
    results[i] <-"bb"
  }
  
  else if (x[i] >= 525 & x[i] <= 532){
    results[i] <-"kb"
  }
  
  else if (x[i] >= 533 & x[i] <= 539){
    results[i] <-"gt"
  }
  
  else if (x[i] >= 540 & x[i] <= 544){
    results[i] <-"havo"
  }
  
  else if (x[i] >= 550){
    results[i] <- "vwo"
  }
 }
return(results)
}

#Classify the students based on their "true scores"

true.decisions <- secondary.ed(true_test_score)


#=============================================================================================
#============================================================================================
#Now I will recalculate the test scores from reestimated thetas based on different criteria:

#==========================================
#Criteria1: Number of mistakes in module A











#===========================================
#Criteria 2: Number of mistakes in day 1








#==========================================
#Criteria3: Different paths (this will only vary for Taal, since we spotted mistakes only there)
