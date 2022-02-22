#=============================================
#Analyses of simulated response patterns 
#============================================
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship/repo/mst/Simulation 1")
load("initial.simulated.responses.RData")
#============================================

#Lezen

lezen_welch <- lapply(students_abilities_L, function(x){t.test(x[,4],x[,5])})
p <- data.frame()
  for(i in 1:length(true.theta_L)){
    p[i,1] <- lezen_welch[[i]]$p.value 
  }
names(p) <- "p values"


which(p$`p values` <= 0.01)
true.theta_L[c(1,   2,   3,   4,   5,   6,   7,   8,  41,  59,  78, 162, 166, 196, 197, 198, 199, 200)]
transform.ref.score.lezen(true.theta_L[c(1,   2,   3,   4,   5,   6,   7,   8,  41,  59,  78, 162, 166, 196, 197, 198, 199, 200)])


#=============================================================================================================
#PROOF THAT PERFORMING THE WELCH TESTS LEAD TO THE SAME RESULTS AS PERFORMING THE ONE SAMPLE T-TEST
#==============================================================================================================
lezen_welch.1 <- list()

for(i in 1:length(true.theta_L)){
  lezen_welch.1[[i]] <- t.test(students_abilities_L[[i]][,4], mu = true.theta_L[i])
}


p1 <- data.frame()
for(i in 1:length(true.theta_L)){
  p1[i,1] <- lezen_welch.1[[i]]$p.value 
}
names(p1) <- "p values"


which(p1$`p values` <= 0.01)

# 1   2   3   4   5   6   7   8  41  59  78 162 166 196 197 198 199 200

#-------------------------------------

#Rekenen

rekenen_welch <- lapply(students_abilities_R, function(x){t.test(x[,4],x[,5])})
p <- data.frame()
  for(i in 1:length(true.theta_R)){
    p[i,1] <- rekenen_welch[[i]]$p.value 
  }
names(p) <- "p values"

which(p$`p values` <= 0.01)   
true.theta_R[c( 1,   2,   3,  22,  45,  81,  89,  98, 141, 187, 200)]
transform.ref.score.rekenen(true.theta_R[c(1,   2,   3,  22,  45,  81,  89,  98, 141, 187, 200)])

#=============================================================================================================
#PROOF THAT PERFORMING THE WELCH TESTS LEAD TO THE SAME RESULTS AS PERFORMING THE ONE SAMPLE T-TEST
#==============================================================================================================
rekenen_welch.1 <- list()

for(i in 1:length(true.theta_R)){
  rekenen_welch.1[[i]] <- t.test(students_abilities_R[[i]][,4], mu = true.theta_R[i])
}


p1 <- data.frame()
for(i in 1:length(true.theta_R)){
  p1[i,1] <- rekenen_welch.1[[i]]$p.value 
}
names(p1) <- "p values"


which(p1$`p values` <= 0.01)

#1   2   3  22  45  81  89  98 141 187 200
#-----------------------------------

#Taal

taal_welch <- lapply(students_abilities_T, function(x){t.test(x[,4],x[,5])})
p <- data.frame()
  for(i in 1:length(true.theta_T)){
    p[i,1] <- taal_welch[[i]]$p.value 
  }
names(p) <- "p values"

which(p$`p values` <= 0.01)   #
true.theta_T[c(1,   2,  35,  40,  50,  80, 114, 126, 151, 173, 197, 198, 199, 200)]
transform.ref.score.taal(true.theta_R[c(1,   2,  35,  40,  50,  80, 114, 126, 151, 173, 197, 198, 199, 200)])


#=============================================================================================================
#PROOF THAT PERFORMING THE WELCH TESTS LEAD TO THE SAME RESULTS AS PERFORMING THE ONE SAMPLE T-TEST
#==============================================================================================================
taal_welch.1 <- list()

for(i in 1:length(true.theta_T)){
  taal_welch.1[[i]] <- t.test(students_abilities_T[[i]][,4], mu = true.theta_T[i])
}


p1 <- data.frame()
for(i in 1:length(true.theta_T)){
  p1[i,1] <- taal_welch.1[[i]]$p.value 
}
names(p1) <- "p values"


which(p1$`p values` <= 0.01)

#1   2  35  40  50  80 114 126 151 173 197 198 199 200
