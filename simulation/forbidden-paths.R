#============================================================
#Are large differences in real and observed theta at the edges of distribution are related to 
#forbidden paths in the MST
#=========================================================

#NOTE: This is not exactly what we initially had in mind

#We will be looking at the edges of the distribution i.e, I will take the 10 most proficient and 10 least proficient students
#Then I will inspect if there is more than one path i.,e other paths than the "erfect one" and then
#look at the differences between the average r-re estimated thetas in both situations 

#++++++++++++++++++++++++++++
#Lezen
#++++++++++++++++++++++++++
#####Low achieving students: Here we expect the path "LDA1-LDB1-LDD1-LDG1" to be dominant

#Look at the first  15 student, i.e., true thetas ranging from -0.7 to 0

for(i in 1:15){
  print(unique(students_abilities_L[[i]]$booklet_id))
}


# Students 11, 12, 13, 14 and 15 have more than one path

#####For student 11 #####  (true theta = -0.2)


student11_L <- split(students_abilities_L[[11]], students_abilities_L[[11]]$booklet_id)
summary(student11_L[[1]]) #  (mean)theta   :-0.20131   "LDA1-LDB1-LDD1-LDG1" 
summary(student11_L[[2]])  #  (mean)theta   :-0.06956   "LDA1-LDB1-LDE1-LDG1"
# As expected there is a big difference between the true and estimated theta, when a student takes
#module E insted of module D in the third stage, even though he gets back at modoule G in the last stage

#####For student 12 #####  (true theta =  - 0.15)

student12_L <- split(students_abilities_L[[12]], students_abilities_L[[12]]$booklet_id)
summary(student12_L[[1]]) #  (mean)theta   :-0.15036   "LDA1-LDB1-LDD1-LDG1"
summary(student12_L[[2]])  #  (mean)theta   :-0.14359   "LDA1-LDB1-LDE1-LDG1"
#There is a difference between the true and estimated theha when the students changes routes, but it is not as
#big as with student 11

#####For student 13 #####  (true theta =  - 0.1)

student13_L <- split(students_abilities_L[[13]], students_abilities_L[[13]]$booklet_id)
summary(student13_L[[1]]) #  (mean)theta   :-0.10777  "LDA1-LDB1-LDD1-LDG1"
summary(student13_L[[2]])  #  (mean)theta   : 0.05351 "LDA1-LDB1-LDD1-LDH1" (opposite sign)  
summary(student13_L[[3]])  #  (mean)theta   :-0.05150 "LDA1-LDB1-LDE1-LDG1"
summary(student13_L[[4]])  #   (mean)theta   :0.05389  "LDA1-LDB1-LDE1-LDH1"
# There is a bigger discrepancy between the true and the estimated theta when a student ends up in module
#H in the fourth stage; and as previously there is a (not so big) discrepancy between students having different modules


#####For student 14 #####  (true theta =  - 0.05)

student14_L <- split(students_abilities_L[[14]], students_abilities_L[[14]]$booklet_id)
summary(student14_L[[1]]) #  (mean)theta   :-0.07416  "LDA1-LDB1-LDD1-LDG1"
summary(student14_L[[2]])  #  (mean)theta   :-0.02993 "LDA1-LDB1-LDD1-LDH1"
summary(student14_L[[3]])  #  (mean)theta   :-0.004185 "LDA1-LDB1-LDE1-LDG1
summary(student14_L[[4]])  #   (mean)theta   :0.056028  LDA1-LDB1-LDE1-LDH1
summary(student14_L[[5]])  #   (mean)theta   :0.07631   "LDA1-LDC1-LDE1-LDH1  !!
# There is a bigger discrepancy between the true and the estimated theta when a student ends up in module
#H in the fourth stage; and as previously there is a (not so big) discrepancy between students having different modules


#####For student 15 #####  (true theta =   0)

student15_L <- split(students_abilities_L[[15]], students_abilities_L[[15]]$booklet_id)
summary(student15_L[[1]]) #  (mean)theta   :-0.03543  LDA1-LDB1-LDD1-LDG1
summary(student15_L[[2]])  #  (mean)theta   : 0.08101 LDA1-LDB1-LDD1-LDH1   
summary(student15_L[[3]])  #  (mean)theta   :0.001348 LDA1-LDB1-LDE1-LDG1
summary(student15_L[[4]])  #   (mean)theta   :0.06224   "LDA1-LDB1-LDE1-LDH1"
summary(student15_L[[5]])  #   (mean)theta   :0.1144  "LDA1-LDC1-LDE1-LDG1"
summary(student15_L[[6]])  #   (mean)theta   :-0.01566   "LDA1-LDC1-LDE1-LDH1"
summary(student15_L[[7]])  #   (mean)theta   :0.14710  "LDA1-LDC1-LDF1-LDH1














































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