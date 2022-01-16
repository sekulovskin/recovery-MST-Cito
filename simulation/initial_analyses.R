#=============================================
#Analyses of simulated response patterns 
#============================================

#============================================
# For the reading test (Lezen)
#===========================================
#===========================================
#Option 1: looking for a significant difference between estimated and "true" theta
#for each student's(having different theta values) multiple "measurements"
#============================================

# Apply the welch test 
lapply(students_abilities_L, function(x){t.test(x[,4],x[,5])})

#We observe a significant difference between the true and the average estimated "true" theta
# for 10 studentd

# These are the students with "true" theta of:
# 1.5; 1.4; 0.7; -0.8; -1; -1.1; -1.2; -1.3; -1.4 & -1.5.

#============================================
# For the Math test (Rekenen)
#===========================================
#===========================================
#Option 1: looking for a significant difference between estimated and "true" theta
#for each student's(having different theta values) multiple "measurements"
#============================================

# Apply the welch test 
lapply(students_abilities_R, function(x){t.test(x[,4],x[,5])})

#We observe a significant difference between the true and the average estimated "true" theta
# for 10 studentd

# These are the students with "true" theta of:
# -1.3; -1.4; -1.5.


#============================================
# For the Language test (Taal)
#===========================================
#===========================================
#Option 1: looking for a significant difference between estimated and "true" theta
#for each student's(having different theta values) multiple "measurements"
#============================================

# Apply the welch test 
lapply(students_abilities_T, function(x){t.test(x[,4],x[,5])})

#We observe a significant difference between the true and the average estimated "true" theta
# for 10 studentd

# These are the students with "true" theta of:
# 1.5; -1.4; -1.5.


#OVERALL ROUGH CONCLUSION(s):

# 1: Significant differences tend to appear only on the extremes (especially for low "true" thetas)
# 2: It appears that for the Reading test there are the most discrepancies present, compared to the other two
#that only have 3 each (with most of them on the left extreme)

#===============================================================================
# Option 2: comparing abilities vs module path (not yet sure of this option)
#==============================================================================
#Define a column indicatig the (absolute)difference between true and estimated theta
for (i in 1:length(true.theta_L)){
  students_abilities_L[[i]]$diff <-  abs(students_abilities_L[[i]]$true_theta - students_abilities_L[[i]]$theta)
}

# Since the Question is "to what degree are students able to recover from early mistakes?"
#First, let's focus on students with high "real" theta, who made early mistakes
# Reasoning: high ability students not following the difficult module route A-C-F-I 
non_perfect_difficult_route_L <- list()
for (i in 1:length(true.theta_L)){
  non_perfect_difficult_route_L[[i]] <-students_abilities_L[[i]] %>%
    filter(booklet_id != "LDA1-LDC1-LDF1-LDI1")
}

#Based on a visual inspection of the results we can see that for the true thetas ranging from 1.5 to
# to 1, only the route A-C-F-I is present for all repeated "measurements"
#Afterwards students with "real" theta values of 0.6  0.7  0.8  0.9, have 13, 10, 2 and 3 non-A-C-F-I,
#routes, respectively.

#The next step would be to check whether there is a "significant" difference in the mean difference values
#for the students who had different patterns and those who didn't

t.test(students_abilities_L[[22]]$diff, students_abilities_L[[31]]$diff)


#We can repeat the same thing with the students having the lowest "true" theta valus and with intermediate ones.
#And also with many other combinations

#Let;s look at the easiest route A-B-D-G
non_perfect_easy_route_L <- list()
for (i in 1:length(true.theta_L)){
  non_perfect_easy_route_L[[i]] <-students_abilities_L[[i]] %>%
    filter(booklet_id != "LDA1-LDB1-LDD1-LDG1")
}

#Based on a visual inspection of the results we can see that for the true thetas ranging from -1.5 to
# to -0.1 (with the exception of -0.2 having one different route), only the route  A-B-D-G is present 
#for all repeated "measurements"
#Afterwards students with "real" theta values of 0, 0.1, 0.2, 0.3, 0.4 have 12, 33, 74, 97 and 99 
#non A-B-D-G patterns.

t.test(students_abilities_L[[1]]$diff, students_abilities_L[[16]]$diff)


##OVERALL ROUGH CONCLUSION(s):
#Perfect easy/difficult routes tend to be most present in the measurements for students having
# extreme true theta scores
#Interesting observation: Significant differences are present between students having ONLY
#perfect routes and those that do not. However, those having a perfect route tend to have
#a higher (absolute) value for the difference



#Repeat for each subject