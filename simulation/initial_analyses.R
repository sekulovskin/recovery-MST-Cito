#=============================================
#Analyses of simulated response patterns 
#============================================

#============================================
# For the reading test (Lezen)
#===========================================
#Note I am using alpha = 0.01 as a cutoff value (since using alpha of 0.05 is too lenient for this situation (based on previous results))
#Also note, I am ignoring differences in the middle of the continuum that are barely significant, since I am also looking at the actual difference and not just the p-val
#Before we look into the analyses it is good to look at the summary statistics
#of the "true" theta values
#summary(true.theta_L)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-0.5195  0.0664  0.2178  0.2207  0.3775  1.0264 

#===========================================
#Approach 1: looking for a significant difference between estimated and "true" theta
#for each student's(having different theta values) multiple "measurements"
#============================================

# Apply the welch test 
lapply(students_abilities_L, function(x){t.test(x[,4],x[,5])})

#IN THE CASE WHEN USING true-theta based on the observed distributions:
#  1.3; 1.2; 

#-----------------------------------------------------------------------------------
#THIS WAS IN THE CASE WHEN RUNNING IT WITH THE OLD TRUE THETA (i.e., ranging from -1.5 to 1.5)
#We observe a significant difference between the true and the average estimated "true" theta
# for 10 students
# OF course, these results slightly differ every time we re-reun the analysis
# These are the students with "true" theta of:
# 1.5; 1.4; 1.3; 0.9; -0.9; -1; -1.1; -1.2; -1.3; -1.4 & -1.5.
#--------------------------------------------------------------------------------


#============================================
# For the Math test (Rekenen)
#===========================================
#===========================================
#Before we look into the analyses it is good to look at the summary statistics
#of the "true" theta values
#summary(true.theta_R)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-0.4390  0.1093  0.3612  0.3436  0.5951  1.0211 

# Apply the welch test 
lapply(students_abilities_R, function(x){t.test(x[,4],x[,5])})
#IN THE CASE WHEN USING true-theta based on the observed distributions:
# 1.8; 1.75; 1.7; 1.65; 0.25; -1.2; -1.25; -1.3  (CONCLUSION: on the extremes)

#-----------------------------------------------------------------------------------
#THIS WAS IN THE CASE WHEN RUNNING IT WITH THE OLD TRUE THETA (i.e., ranging from -1.5 to 1.5)
#We observe a significant difference between the true and the average estimated "true" theta
# for 10 students
# These are the students with "true" theta of:
# 1.5; 1.4; -1.3; -1.4; -1.5. (they usually tend to be on the negative extreme only)
#-----------------------------------------------------------------------------------

#============================================
# For the Language test (Taal)
#===========================================
#Before we look into the analyses it is good to look at the summary statistics
#of the "true" theta values
#summary(true.theta_T)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-0.3190  0.1053  0.3003  0.2866  0.4812  0.8109  

# Apply the welch test 
lapply(students_abilities_T, function(x){t.test(x[,4],x[,5])})

#IN THE CASE WHEN USING true-theta based on the observed distributions:
#1.35 only

#-----------------------------------------------------------------------------------
#THIS WAS IN THE CASE WHEN RUNNING IT WITH THE OLD TRUE THETA (i.e., ranging from -1.5 to 1.5)
#We observe a significant difference between the true and the average estimated "true" theta
# for 10 studentd
# These are the students with "true" theta of:
# 1; -1.4; -1.5.
#-----------------------------------------------------------------------------------

#OVERALL ROUGH CONCLUSION(s):

# 1: Significant differences tend to appear only on the extremes (especially for low "true" thetas)
# 2: It appears that for the Reading test there are the most discrepancies present, compared to the other two
#that only have 3-4 each (with most of them on the left extreme).
#This tells us that we should focus on low and high "true" ability values, and more on the reading
#test.





#===============================================================================
# Approach 2: comparing abilities vs module path (not yet sure of this option)
#==============================================================================

#================================
#Lezen
#===============================
#Define a column indicating the (absolute)difference between true and estimated theta
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

#There tend to be very few mismatches ofr high thetas


#-------------------------------------------------------------------------------------------------------
#WHEN USING THE OLD TRUE THETA
#Based on a visual inspection of the results we can see that for the true thetas ranging from 1.5 to
# to 1, only the route A-C-F-I is present for all repeated "measurements"
#Afterwards students with "real" theta values of 0.6  0.7  0.8  0.9, have 13, 10, 2 and 3 non-A-C-F-I,
#routes, respectively.
#The next step would be to check whether there is a "significant" difference in the mean difference values
#for the students who had different patterns and those who didn't
#----------------------------------------------------------------------------------------------------

t.test(students_abilities_L[[41]]$diff, students_abilities_L[[24]]$diff)
#students with higher "true" thetas, tend to have a bigger difference between true and estimated ability
#We can repeat the same thing with the students having the lowest "true" theta valus and with intermediate ones.
#And also with many other combinations

#Let;s look at the easiest route A-B-D-G
non_perfect_easy_route_L <- list()
for (i in 1:length(true.theta_L)){
  non_perfect_easy_route_L[[i]] <-students_abilities_L[[i]] %>%
    filter(booklet_id != "LDA1-LDB1-LDD1-LDG1")
}
#Very few mismatches for very low "true" thetas
#-------------------------------------------------------------------------------------------------------
#WHEN USING THE OLD TRUE THETA (however similar results follow when using the ACET abilities)
#Based on a visual inspection of the results we can see that for the true thetas ranging from -1.5 to
# to -0.1 (with the exception of -0.2 having one different route), only the route  A-B-D-G is present 
#for all repeated "measurements"
#Afterwards students with "real" theta values of 0, 0.1, 0.2, 0.3, 0.4 have 12, 33, 74, 97 and 99 
#non A-B-D-G patterns.
#---------------------------------------------------------------------------------------------------
t.test(students_abilities_L[[1]]$diff, students_abilities_L[[16]]$diff)

#There are significant differences between students with extreme values and no mismatch and those with more moderate values
#and again, students with more extreme values tend to have a higher average difference between the true theta and re-restimated theta.

##OVERALL ROUGH CONCLUSION(s):
#Perfect easy/difficult routes tend to be most present in the measurements for students having
# extreme true theta scores
#Interesting observation: Significant differences are present between students having ONLY
#perfect routes and those that do not. However, those having a perfect route tend to have
#a higher (absolute) value for the difference (this is in both cases)



#Repeat for each subject


#================================
#Rekenen
#===============================
#Define a column indicating the (absolute)difference between true and estimated theta
for (i in 1:length(true.theta_R)){
  students_abilities_R[[i]]$diff <-  abs(students_abilities_R[[i]]$true_theta - students_abilities_R[[i]]$theta)
}


non_perfect_difficult_route_R <- list()
for (i in 1:length(true.theta_R)){
  non_perfect_difficult_route_R[[i]] <-students_abilities_R[[i]] %>%
    filter(booklet_id != "RDA1-RDC1-RDF1-RDI1")
}

#Very few mismatches
t.test(students_abilities_R[[63]]$diff, students_abilities_R[[39]]$diff)
#same conclusion as with Lezen
non_perfect_easy_route_R <- list()
for (i in 1:length(true.theta_R)){
  non_perfect_easy_route_R[[i]] <-students_abilities_R[[i]] %>%
    filter(booklet_id != "RDA1-RDB1-RDD1-RDG1")
}
#Very little mismatches

#DISCREPANCIES ALWAYS TEND TO BE HIGHER FOR MORE EXTREME THETAS
#---------------------------------------------------------------------------------------------------
t.test(students_abilities_R[[1]]$diff, students_abilities_R[[26]]$diff)
#same conclusion


#================================
#Taal
#===============================
#Define a column indicating the (absolute)difference between true and estimated theta
for (i in 1:length(true.theta_T)){
  students_abilities_T[[i]]$diff <-  abs(students_abilities_T[[i]]$true_theta - students_abilities_T[[i]]$theta)
}


non_perfect_difficult_route_T <- list()
for (i in 1:length(true.theta_T)){
  non_perfect_difficult_route_T[[i]] <-students_abilities_T[[i]] %>%
    filter(booklet_id != "TDA1-TDC1-TDF1-TDI1")
}

#Way more mismatches compared to Lezen and Rekenen!!!!!!

t.test(students_abilities_T[[51]]$diff, students_abilities_T[[30]]$diff)

#Same conclusion: sig diff: yes; but higher average difference for more extreme true thetas

non_perfect_easy_route_T <- list()
for (i in 1:length(true.theta_T)){
  non_perfect_easy_route_T[[i]] <-students_abilities_T[[i]] %>%
    filter(booklet_id != "TDA1-TDB1-TDD1-TDG1")
}
#Not so many mismatches on the negative side


#---------------------------------------------------------------------------------------------------
t.test(students_abilities_T[[1]]$diff, students_abilities_T[[10]]$diff)
#same conclusion

