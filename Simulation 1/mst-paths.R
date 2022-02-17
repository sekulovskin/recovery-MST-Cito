#============================================================
#Are large differences in real and observed theta at the edges of distribution are related to 
#forbidden paths in the MST
#=========================================================
#setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship/repo/mst/simulation")
#load("simulated.responses.RData")
#NOTE: This is not exactly what we initially had in mind

#We will be looking at the edges of the distribution i.e, I will take the  most proficient and least proficient students
#Then I will inspect if there is more than one path i.,e other paths than the "perfect one" and then
#look at the differences between the average re-estimated thetas in both situations 

#------------------------------------------------------
# LEZEN
#------------------------------------------------------

for(i in c(1,   2,   3,   4,   5,   6,   7,   8,  41,  59,  78, 162, 166, 196, 197, 198, 199, 200)){
  print(unique(students_abilities_L[[i]]$booklet_id))
}


# Students  41,  59,  78, 162, 166, 196, 197 have more than one path

# so we will look at students  196, 197

# Student 196 (true theta 1.2)

student196_L <- split(students_abilities_L[[196]], students_abilities_L[[196]]$booklet_id)
mean(student196_L[[1]]$theta)   #0.88522   #ABFI
mean(student196_L[[2]]$theta)   #1.180437  #ACFI



student197_L <- split(students_abilities_L[[197]], students_abilities_L[[197]]$booklet_id)
mean(student197_L[[1]]$theta)   #0.9527938
mean(student197_L[[2]]$theta)   #1.297269

thetas <- c(0.88522, 1.180437, 0.9527938, 1.297269)
paths <- rep(c("A-B-F-I", "A-C-F-I"), 2)
true.thetas <- rep(c(true.theta_L[196], true.theta_L[197]), each =  2)

paths.lezen <- data.frame(paths, thetas, true.thetas)

library(foreign)
write_csv(paths.lezen, "paths.lezen.csv")
#------------------------------------------------------
# REKENEN
#------------------------------------------------------

for(i in c(1,   2,   3,  22,  45,  81,  89,  98, 141, 187, 200)){
  print(unique(students_abilities_R[[i]]$booklet_id))
}


#Only student 187 (theta =  0.93) has more than one path

student187_R <- split(students_abilities_R[[187]], students_abilities_R[[187]]$booklet_id)
mean(student187_R[[1]]$theta)   #0.88522
mean(student187_R[[2]]$theta) 
mean(student187_R[[3]]$theta)  
mean(student187_R[[4]]$theta)  
mean(student187_R[[5]]$theta)  
mean(student187_R[[6]]$theta)  

thetas <- c(0.7225602, 0.8149699, 0.6342958, 0.7954973, 0.6462335, 0.9474332)
paths <- c("A-B-E-H", "A-B-E-I", "A-C-E-H", "A-C-E-I", "A-C-F-H", "A-C-F-I")
true.thetas <- rep(true.theta_R[187], 6)

paths.rekenen <- data.frame(paths, thetas, true.thetas)

write_csv(paths.rekenen, "paths.rekenen.csv")

#------------------------------------------------------
# Taal
#------------------------------------------------------

for(i in c(1,   2,  35,  40,  50,  80, 114, 126, 151, 173, 197, 198, 199, 200)){
  print(unique(students_abilities_T[[i]]$booklet_id))
}

#The most extreme value with more than one path observed was student 173 (theta = 0.6)

student173_T <- split(students_abilities_T[[173]], students_abilities_T[[173]]$booklet_id)
mean(student173_T[[1]]$theta)   
mean(student173_T[[2]]$theta) 
mean(student173_T[[3]]$theta)  
mean(student173_T[[4]]$theta)  
mean(student173_T[[5]]$theta)  
mean(student173_T[[6]]$theta)  
mean(student173_T[[7]]$theta)  
mean(student173_T[[8]]$theta)  

thetas <- c(0.388778, 0.503121, 0.3782605, 0.5765877, 0.3862099, 0.4398309, 0.3825453, 0.6185134)
paths <- c("A-B-E-H", "A-B-E-I", "A-B-F-H", "A-B-F-I", "A-C-E-H", "A-C-E-I", "A-C-F-H", "A-C-F-I")
true.thetas <- rep(true.theta_T[173], 8)

paths.taal <- data.frame(paths, thetas, true.thetas)

write_csv(paths.taal, "paths.taal.csv")