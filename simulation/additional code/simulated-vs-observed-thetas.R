#=============================================================
##############Simulated theta's range####################
#============================================================
# I ran it with different sizes, and after N = 40000 it doesn't make any difference so I stick with that value
#The simulated "true" thetas, used for simulating response patterns are based on these results;
set.seed(123)

#Lezen
theta_obs_L <- sort(rnorm(40000, 0.219, 0.236), decreasing = FALSE)
summary(theta_obs_L)

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.67157  0.06081  0.21898  0.21847  0.37658  1.37767 (however after 1.337 , the next value is 1.24; 1.20...)

#Rekenen
theta_obs_R <- sort(rnorm(40000, 0.326, 0.389), decreasing = FALSE)
summary(theta_obs_R)

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-1.40551  0.06277  0.32801  0.32636  0.59219  1.83920 
#but -1.4 is an outlier, the other usually start from ~1.3
#on the positive side it is okay 1.839203 1.824951 1.790765.......


#Taal

theta_obs_T <- sort(rnorm(40000, 0.273, 0.301), decreasing = FALSE)
summary(theta_obs_T)

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-1.03067  0.07014  0.27378  0.27359  0.47691  1.42986  (no outliers)