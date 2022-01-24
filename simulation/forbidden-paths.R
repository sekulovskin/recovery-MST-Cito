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

#++++++++++++++++++++++++++++
#Lezen
#++++++++++++++++++++++++++
#####Low achieving students: Here we expect the path "LDA1-LDB1-LDD1-LDG1" to be dominant

#Look at the first  15 student, i.e., true thetas ranging from -0.7 to 0

for(i in 1:15){
  print(unique(students_abilities_L[[i]]$booklet_id))
}


# Students 10, 11, 12, 13, 14 and 15 have more than one path

#####For student 10 #####  (true theta = -0.25)

student10_L <- split(students_abilities_L[[10]], students_abilities_L[[10]]$booklet_id)
summary(student10_L[[1]]) #  (mean)theta   :-0.25718   "LDA1-LDB1-LDD1-LDG1" 
summary(student10_L[[2]])  #  (mean)theta   :-0.05987    "LDA1-LDB1-LDE1-LDG1"
# As expected there is a big difference between the true and estimated theta, when a student takes
#module E instead of module D in the third stage, even though he gets back at module G in the last stage
# Indicating that it is not as easy to compensate for "mistakes" in the second stage

#####For student 11 #####  (true theta = -0.2)

student11_L <- split(students_abilities_L[[11]], students_abilities_L[[11]]$booklet_id)
summary(student11_L[[1]]) #  (mean)theta   :-0.19824   "LDA1-LDB1-LDD1-LDG1" 
summary(student11_L[[2]])  #  (mean)theta   :-0.08923   "LDA1-LDB1-LDE1-LDG1"
summary(student11_L[[3]])  #  (mean)theta   :-0.1285    "LDA1-LDC1-LDE1-LDG1"
# As with student 10 there is a big discrepancy between when he took module D instead of module E in the third stage,
#However, an even more interesting observation is that if he take module C in the second stage and then proceeds to E
#and G, than he has an end theta estimate that is closer to his true theta.

#####For student 12 #####  (true theta =  - 0.15)

student12_L <- split(students_abilities_L[[12]], students_abilities_L[[12]]$booklet_id)
summary(student12_L[[1]]) #  (mean)theta   :-0.16047  "LDA1-LDB1-LDD1-LDG1"
summary(student12_L[[2]])  #  (mean)theta   :-0.09746    "LDA1-LDB1-LDE1-LDG1"
summary(student12_L[[3]])  #  (mean)theta   :0.006875   "LDA1-LDB1-LDE1-LDH1"
summary(student12_L[[4]])  #  (mean)theta   :-0.02351   "LDA1-LDC1-LDE1-LDG1"

#The estimate for theta when the student takes the easiest path is slightly lower than he's true theta
#(which is expected); The re-estimate is even worse when he takes module E in the the third stage. 
#The difference is even bigger when he ends up in module H in the last stage, and again, surprisingly it is less big when
#he takes path C in the second stage and then continues with E and G. THIS MEANS THAT CORRECT ANSWERS IN THE FIRST MODULE
#CAN STILL BE CORRECTED FOR IN THE SUBEQUENT ONES

#####For student 13 #####  (true theta =  - 0.1)

student13_L <- split(students_abilities_L[[13]], students_abilities_L[[13]]$booklet_id)
summary(student13_L[[1]]) #  (mean)theta   :-0.11410  "LDA1-LDB1-LDD1-LDG1"
summary(student13_L[[2]])  #  (mean)theta   :-0.024985 "LDA1-LDB1-LDE1-LDG1"
summary(student13_L[[3]])  #   (mean)theta   :0.08129  "LDA1-LDB1-LDE1-LDH1"

# There is a bigger discrepancy between the true and the estimated theta when a student ends up in module
#H in the fourth stage; and as previously there is a (not so big) discrepancy between students having different modules
#in stage 3 (D vs E)

#####For student 14 #####  (true theta =  - 0.05)

student14_L <- split(students_abilities_L[[14]], students_abilities_L[[14]]$booklet_id)
summary(student14_L[[1]]) #  (mean)theta   :-0.05877   "LDA1-LDB1-LDD1-LDG1"
summary(student14_L[[2]])  #  (mean)theta   :-0.027566 "LDA1-LDB1-LDE1-LDG1
summary(student14_L[[3]])  #   (mean)theta   :0.043119  LDA1-LDB1-LDE1-LDH1
summary(student14_L[[4]])  #   (mean)theta   :0.010623  "LDA1-LDC1-LDE1-LDH1  
# As with previously it can be seen that earlier mismatches (situation 4) can be more easily compensated then
#mismatches at a later stage (situation 3)


#####For student 15 #####  (true theta =   0)

student15_L <- split(students_abilities_L[[15]], students_abilities_L[[15]]$booklet_id)
summary(student15_L[[1]]) #  (mean)theta   :-0.024226  LDA1-LDB1-LDD1-LDG1
summary(student15_L[[2]])  #  (mean)theta   : 0.08644 LDA1-LDB1-LDD1-LDH1   
summary(student15_L[[3]])  #  (mean)theta   :0.003348  LDA1-LDB1-LDE1-LDG1
summary(student15_L[[4]])  #   (mean)theta   :0.10371  "LDA1-LDB1-LDE1-LDH1"
summary(student15_L[[5]])  #   (mean)theta   :0.08774   "LDA1-LDC1-LDE1-LDH1"

#----------------------------------------------------------------------
#Look at the last  15 student, i.e., true thetas ranging from 0.55 to 1.3

for(i in 26:41){
  print(unique(students_abilities_L[[i]]$booklet_id))
}

#For student 26 (true theta = 0.55)
student26_L <- split(students_abilities_L[[26]], students_abilities_L[[26]]$booklet_id)
summary(student26_L[[1]]) #  (mean)theta   :0.3343  LDA1-LDB1-LDE1-LDH1
summary(student26_L[[2]])  #  (mean)theta   : 0.3442 LDA1-LDB1-LDE1-LDI1  
summary(student26_L[[3]])  #  (mean)theta   :0.5163  LDA1-LDB1-LDF1-LDI1
summary(student26_L[[4]])  #   (mean)theta   :0.3471   "LDA1-LDC1-LDF1-LDH1
summary(student26_L[[5]])  #   (mean)theta   :0.5610  "LDA1-LDC1-LDF1-LDI1"

#As can be seen the closest estimate is when the student takes the most difficult route ACFI, however, it is also evident,
#that when he/she makes early mistakes and takes a route such as ABFI, he can mainly recover from those mistakes at
#a later stage. Hoever if he doesn't recover from that mistake in the third stage, so instead of going from B to F, 
#he goes fro B to E, then regardless of whether he ends up in H or I in the last stage. there is an underestimate of the ability.

#For student 27 (true theta = 0.6)
student27_L <- split(students_abilities_L[[27]], students_abilities_L[[27]]$booklet_id)
summary(student27_L[[1]]) #  (mean)theta   :0.3932      A B E H
summary(student27_L[[2]])  #  (mean)theta   :0.4357     A B F H  
summary(student27_L[[3]])  #  (mean)theta   :0.5659     A B F I
summary(student27_L[[4]])  #   (mean)theta   :0.3196    A C E H
summary(student27_L[[5]])  #   (mean)theta   :0.40500   A C F H
summary(student27_L[[6]])  #   (mean)theta   :0.6318    A C F I

#As previously the closest estimate is when the student takes the most difficult route, afterwards,
#interestingly the closest estimate is when he takes the route A B F I, indicating once more, that early mistakes
#in module A can be compensated for, this is even more evident when we compare it with A C F H and A C E H and see that ,
#later mistakes in stages 2 and 3, respectively are harder to compensate. It is also interesting to note, that 
#route A B E H gives a higher theta estimate than A C E H!!!!!


#For student 27 (true theta = 0.65)
student28_L <- split(students_abilities_L[[28]], students_abilities_L[[28]]$booklet_id)
summary(student28_L[[1]]) #  (mean)theta   :0.5549       A B F I
summary(student28_L[[2]])  #  (mean)theta   :0.4206     A C E H  
summary(student28_L[[3]])  #  (mean)theta   0.4306      A C F H
summary(student28_L[[4]])  #   (mean)theta   :0.6958   A C F I

# We can see that as that now, when the student follows the most difficult route. there is a slight overestimate of 
#his theta. Again, when taking the route ABFI the student has a re-restimate that is closer to his true theta
#than when making mistakes at stages 2 and 3!!


# I will skip the rest of the students and only do student 31, sicne the rwest have only routes and ABFI

# For Student 31 (true theta = 0.8)
student31_L <- split(students_abilities_L[[31]], students_abilities_L[[31]]$booklet_id)
summary(student31_L[[1]]) #  (mean)theta   :0.6868      A B F I
summary(student31_L[[2]])  #  (mean)theta   :0.4306     A C F H 
summary(student31_L[[3]])  #  (mean)theta   :0.7915      A C F I

# The conclusions are the same as previously

# OVERALL CONCLUSION FOR LEZEN: None of the students on the negative and positive extreme approached a path ACF... or
# ABD, respectively (so no issues with forbidden rules were found). However, the most important observation, 
#on both extremes is that students can compensate very well for mismatches on the first stage (so ending up in module C for
#low achieving students and ending up un module B for high achieving stunts). This is a very important observation,
#since it gives, or at least hints an answer to the main research question.

rm(student10_L, student10_L, student11_L, student12_L, student13_L, student14_L, student15_L, student26_L, 
   student27_L, student28_L, student31_L)



#++++++++++++++++++++++++++++
#Rekenen
#++++++++++++++++++++++++++
#####Low achieving students: Here we expect the path "RDA1-RDB1-RDD1-RDG1" to be dominant

#Look at the first  25 student, i.e., true thetas ranging from -1.3 to -0.1

for(i in 1:25){
  print(unique(students_abilities_R[[i]]$booklet_id))
}


# Students 19, 21, 23, 24, 25

#####For student 19 #####  (true theta = -0.4)

student19_R <- split(students_abilities_R[[19]], students_abilities_R[[19]]$booklet_id)
summary(student19_R[[1]]) #  (mean)theta   :-0.4086   A B D G
summary(student19_R[[2]])  #  (mean)theta   :-0.3801  A C D G
# No forbidden paths approached; best estimate when taking easiest path; good potential for recovering when making
# early mistakes.

#####For student 21 #####  (true theta = -0.3)
student21_R <- split(students_abilities_R[[21]], students_abilities_R[[21]]$booklet_id)
summary(student21_R[[1]]) #  (mean)theta    :-0.3091   A B D G
summary(student21_R[[2]])  #  (mean)theta   :-0.1905   A C D G
summary(student21_R[[3]])  #  (mean)theta   :-0.1483   A C E G

# No forbidden paths approached; best estimate when taking easiest path; good potential for recovering when making
# early mistakes.

#####For student 23 #####  (true theta = -0.2)
student23_R <- split(students_abilities_R[[23]], students_abilities_R[[23]]$booklet_id)
summary(student23_R[[1]]) #  (mean)theta    :-0.21494   A B D G
summary(student23_R[[2]])  #  (mean)theta   :-0.11439   A B E G
summary(student23_R[[3]])  #  (mean)theta   :-0.02135   A C D G
summary(student23_R[[4]])  #  (mean)theta   :-0.1487   A C E G
# Taking a more "gradual" recovery path like A C E G instead of ACEG (so in the third stage going to E instead directly to D),
# Lead to a better estimate (THIS IS THE ONLY CASE WHERE THIS IS HAPPENING)


#####For student 24 #####  (true theta = -0.15)
student24_R <- split(students_abilities_R[[24]], students_abilities_R[[24]]$booklet_id)
summary(student24_R[[1]]) #  (mean)theta    :-0.14752   A B D G
summary(student24_R[[2]])  #  (mean)theta   :-0.1279   A C D G
summary(student24_R[[3]])  #  (mean)theta   :-0.04656   A C E G


#####For student 25 #####  (true theta = -0.1)
student25_R <- split(students_abilities_R[[25]], students_abilities_R[[25]]$booklet_id)
summary(student25_R[[1]]) #  (mean)theta    :-0.10386    A B D G
summary(student25_R[[2]]) #  (mean)theta    :-0.07343   A B E G
summary(student25_R[[3]])  #  (mean)theta   :-0.08453   A C D G
summary(student25_R[[4]])  #  (mean)theta   :-0.034121   A C E G

#----------------------------------------------------------------------
#Look at the last  25 student, i.e., true thetas ranging from 0.6 to 1.8

for(i in 39:63){
  print(unique(students_abilities_R[[i]]$booklet_id))
}

#####For student 39 #####  (true theta = 0.6)
student39_R <- split(students_abilities_R[[39]], students_abilities_R[[39]]$booklet_id)
summary(student39_R[[1]]) #  (mean)theta    :0.5234    A B E H   ???
summary(student39_R[[2]]) #  (mean)theta    :0.5478   A C E H
summary(student39_R[[3]])  #  (mean)theta   :0.6556   A C E I
summary(student39_R[[4]])  #  (mean)theta   :0.55811  A C F H
summary(student39_R[[5]])  #  (mean)theta   :0.69511  A C F I

# when he takes the hardest route there tends to be an overestimate, however even very different paths don;t lead to a big discrepancy, indicating
# both no problems with forbidden paths and also possibility for recovery due to early mistakes

#####For student 40 #####  (true theta = 0.65)
student40_R <- split(students_abilities_R[[40]], students_abilities_R[[40]]$booklet_id)
summary(student40_R[[1]]) #  (mean)theta    :0.3878     A B E H   
summary(student40_R[[2]]) #  (mean)theta    :0.6856     A B E I ( Very big potential for recovery!!)
summary(student40_R[[3]]) #  (mean)theta    :0.5372     A C E H
summary(student40_R[[4]])  #  (mean)theta   :0.6731     A C E I
summary(student40_R[[5]])  #  (mean)theta   :0.5710     A C F H
summary(student40_R[[6]])  #  (mean)theta   :0.6967     A C F I

#Again we can see very big potential for recovery from early mistakes, and also an overestimate for theta when taking
#"more difficult" routes,

#.
#.
#.

rm(student19_R, student21_R, student23_R, student24_R, student25_R, student39_R, student40_R)


#++++++++++++++++++++++++++++
#Taal
#++++++++++++++++++++++++++
#####Low achieving students: Here we expect the path "TDA1-TDB1-TDD1-TDG1" to be dominant

#Look at the first  25 student, i.e., true thetas ranging from -1.3 to -0.1

for(i in 1:25){
  print(unique(students_abilities_T[[i]]$booklet_id))
}

# By far this subject has the most variability in paths

# Students 14,...,19(!!)

#I will directly start from student 19, since there, more paths, and also forbidden paths are approached

#####For student 18 #####  (true theta = -0.2)

student18_T <- split(students_abilities_T[[18]], students_abilities_T[[18]]$booklet_id)
summary(student18_T[[1]]) #  (mean)theta   :-0.20917   A B D G
summary(student18_T[[2]])  #  (mean)theta   :-0.1460  A B E G  
summary(student18_T[[3]])  #  (mean)theta   :-0.1752  A B F G(interesting)
summary(student18_T[[4]])  #  (mean)theta   :-0.15654  A C D G
summary(student18_T[[5]])  #  (mean)theta   :-0.233  A C E G
summary(student18_T[[6]])  #  (mean)theta   :-0.02979  A C E H
summary(student18_T[[7]])  #  (mean)theta   :-0.05831  A C F H

# As with the other two subjects ,the best estimate is obtained when taking the easiest route. When taking the route A B F G
# we can see that the estimate is not that different, indicating that there is possible recovery even for mistakes made  in stage 2
#Most importantly we can see that when approaching an "unlikely" path A C F H, there is ~0.15 units discrepancy, however
#it is smaller than for A C E H, but we will look into the other students as well


####For student 19 #####  (true theta = -0.15)
student19_T <- split(students_abilities_T[[19]], students_abilities_T[[19]]$booklet_id)
summary(student19_T[[1]]) #  (mean)theta    :-0.1705   A B D G (STILL BEST ROUTE, GIVES SLIGHT UNDERESTIA)
summary(student19_T[[2]])  #  (mean)theta   :-0.10408  A B E G  
summary(student19_T[[3]])  #  (mean)theta   :-0.03122  A B E H 
summary(student19_T[[4]])  #  (mean)theta   :-0.06777  A B D H
summary(student19_T[[5]])  #  (mean)theta   :-0.171042 A C D G 
summary(student19_T[[6]])  #  (mean)theta   :-0.02968  A C E G
summary(student19_T[[7]])  #  (mean)theta   : 0.04863  A C E H
summary(student19_T[[8]])  #  (mean)theta   :-0.0005469 A C F G  !!!!!

# As with the last two cases, , of course there is a huge discrepancy when taking the path A C F G, but even more so,
#when taking A C E H.  In both cases the student cannot compensate enough.
#SO YES, in some rare cases, big discrepancies are due to forbidden paths, but that is not the sole reason.


#----------------------------------------------------------------------
#Look at the last  25 student, i.e., true thetas ranging from 0.25 to 1.45

for(i in 27:51){
  print(unique(students_abilities_T[[i]]$booklet_id))
}

#Now this is interesting because we also have variablility in paths in the uppermost theta extreme

#fOR STUDENT 27 (TRUE THETA = 0.25)

student27_T <- split(students_abilities_T[[27]], students_abilities_T[[27]]$booklet_id)
summary(student27_T[[1]]) #  (mean)theta    :0.07221  A B D G  (EASIEST PATH, AND LIGICALLY MOST DISCREPANCY)
summary(student27_T[[2]])  #  (mean)theta   :0.11029  A B D H  (NOT ENOUGH POWER TO RECOVER AFTER MODULE 3)
summary(student27_T[[3]])  #  (mean)theta   :0.08805  A B E G  (VERY WEIRD PATH, WHICH LEADS TO AN UNDERESTIMATE)
summary(student27_T[[4]])  #  (mean)theta   :0.23152  A B E H  (PROOF THAT THERE CAN BE RECOVERY EVEN IF TAKING THE MIDDLE PATHS IN THE LAST TWO STAGES)
summary(student27_T[[5]])  #  (mean)theta   :0.3552   A B E I (NOw this is the opposite, and it shows that there can be even an overestimate, when recovering)
summary(student27_T[[6]])  #  (mean)theta   :0.35303   A B F G (THIS TIME THE STUDENT RECOVERED FRO THE FIRST STAGE BUT THEN FELL AGAIN IN THE LAST ONE, HOVWEVER THERE IS STILL AN OVERESTIMATE)
summary(student27_T[[7]])  #  (mean)theta   : 0.2573  A B F H  (RIGHT ON POINT)
summary(student27_T[[8]])  #  (mean)theta   0.3252    A B F I
summary(student27_T[[9]])  #  (mean)theta   :0.03684  A C D G
summary(student27_T[[10]])  #  (mean)theta   :0.2272  A C E H
summary(student27_T[[11]])  #  (mean)theta   :0.2008  A C F G
summary(student27_T[[12]])  #  (mean)theta   :0.2135  A C F H 
summary(student27_T[[13]])  #  (mean)theta   :0.3530  A C F I

# All of this shows that for a student having an average ability, many routes are possible, and in general, recovery is possible
#however, there is a bigger problem with over-estimation, when taking certain paths 

# = student 50 (true theta = 1.4)

student50_T <- split(students_abilities_T[[50]], students_abilities_T[[50]]$booklet_id)
summary(student50_T[[1]]) #  (mean)theta    :1.0938  A B F I  (diff of 0.31)
summary(student50_T[[2]])  #  (mean)theta   :1.4235   A C F I



# nOw lets look at the last student 51 (true theta = 1.45)

student51_T <- split(students_abilities_T[[51]], students_abilities_T[[51]]$booklet_id)
summary(student51_T[[1]]) #  (mean)theta    :1.146 A B F I   (diff of 0.304)
summary(student51_T[[2]])  #  (mean)theta   :1.5074   A C F I

#CONCLUSION: WHEN compensating for early mistakes (in module a, so A - B - F..) for students having a high true ability,
#there is quite a big discrepancy ~0.30, this is only the case with this subject (Taal)

#-------------------------------------------
#OVERALL CONCLUSIONS:

############lezen:
# 1. For all courses, it is easier to compensate for "mistakes" in the First stage (module A) than in the second stage (mod B/C)
# 2. It is also interesting to note, that route A B E H gives a higher theta estimate than A C E H!!!!! (modules B and C are longer that's why!)
# Not approaching forbidden paths (??) 

# ##########Rekenen:
# 3. Recovering from early mistakes (in stage 1/module A) is still present;
# 4. Way less variability in possible routes
# 5. More over-estimates present
# Not approaching forbidden paths (??)

# #########Taal:

#6. Most variability in paths out of all 3 subjects, variability present also at the top-most positive extreme
#7. Some possibilities of recovering from "mistakes" made in stages 2 and 3 (for example taking a route A B F G) (for a student with a low true theta!!)
# When approaching an unlikely path (like A C F H), there is ~0.15 units discrepancy, however
#it is smaller than for A C E H, but we will look into the other students as well
# 8. there is a huge discrepancy when taking the path A C F G (which is a very unlikely path), but even more so,
#when taking A C E H.  In both cases the student cannot compensate enough.#
# SO YES, in some rare cases, big discrepancies are due to "forbidden" (or unlikely) paths, but that is not the sole reason.
#THIS IS THE ONLY COURSE WHERE THESE PATHS ARE APPROACHED
#FOR STUDENTS ON THE HIGHEST EXTREME OF TAAL, THERE IS A A BIG DISCREPANCY EVEN WHEN RECOVERING FROM EARLY MISTAKES IN STAGE 1






#IGNORE THE CODE BELOW (FOR NOW)
#-----------------------------------------------
#++++++++++++++
#Lezen
#++++++++++++++
#See which items belong to Module A and extract the weights (points) for each from the pars df
test_design_L %>%
  filter(module_id == "LDA1")

LDA.weights <- pars_L %>%
  filter(item_id ==  c("BL00518", "BL00519", "BL00520", "BL00521", "BL02412",  "OP00142")) %>%
  select(item_score)

test_design_L %>%
  filter(module_id == "LDB1")

LDB.weights <- pars_L %>%
  filter(item_id == "BL00474D ") %>%
  select(item_score)


test_design_L %>%
  filter(module_id == "LDA1")

LDA.weights <- pars_L %>%
  filter(item_id ==  c("BL00518", "BL00519", "BL00520", "BL00521", "BL02412",  "OP00142")) %>%
  select(item_score)


test_design_L %>%
  filter(module_id == "LDA1")

LDA.weights <- pars_L %>%
  filter(item_id ==  c("BL00518", "BL00519", "BL00520", "BL00521", "BL02412",  "OP00142")) %>%
  select(item_score)




# There are 18 theoretically possible combinations if we consider the MST design and ignore the routing rules

# For now, forbidden paths, theoretically wold be A - B - D - I and A - C - F - G

#Now lets see how many of these combinations are present in "practice" (in our simulation), for all three subjects, 
#and then inspect the discrepancy between the ability estimates for the student having the most paths, out of which, some are
#on the boundary.

#================
#Lezen
#===============
#Inspect how many unique combinations there are
combinations_L <- NULL
for (i in 1:length(true.theta_L)){
 combinations_L[i] <-  length(unique(students_abilities_L[[i]]$booklet_id))
}

combinations_L

#The maximum number of combinations lies in the middle of the theta continuum;

length(unique(students_abilities_L[[20]]$booklet_id))

#================
#Rekenen
#===============
#Significant differences were present for students with true theta of 1.8; 1.75; 1.7; 1.65;
#0.25 (?); -1.2; -1.25; -1.3
#i.e., students 63, 62, 61, 60, 30(?), 3, 2, 1

combinations_R <- NULL
for (i in 1:length(true.theta_R)){
  combinations_R[i] <-  length(unique(students_abilities_R[[i]]$booklet_id))
}

combinations_R
#[1]  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  2  1  3  2  3  4  4  5  4  7  7  7  7  7  7 10  7  9  6  6  5  5  5
#[42]  6  4  4  4  4  3  4  2  3  2  2  2  2  1  1  1  1  1  1  1  1  1
#Student number 34 
length(unique(students_abilities_R[[29]]))




#================
#Taal
#===============
#Significant differences were present for students with true theta of 1.35
#i.e., student 49